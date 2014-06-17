#!/usr/bin/perl

use Modern::Perl 2014;
use autodie;
use JSON::XS qw(encode_json);
use File::Slurp qw(write_file read_file);
use URI::Escape qw(uri_escape_utf8);
use Lingua::EN::Inflect qw(WORDLIST);
use utf8;
$|++;

my $unsanitary = qr/[^\p{L}\s\-']/;

my $dictionaryFile = 'download/wiktionary.xml';
open DICT, "<:utf8", $dictionaryFile;

my %plurals;
my $term;
my $ns;
my $is_English;
my $is_noun;

say "Processing wiktionary...";

#It's not worth parsing the XML 'properly' as the actual page
# contents are just flat text (with wiki markup)
while (<DICT>) {

  print "\r$. lines processed" unless $. % 10000;

  #A new term begins
  if (/<title>(?<term>.+)<\/title>/) {

    $term = $+{term};

    #Suppress storing anything until we know it's
    # a dictionary entry and an English noun
    $ns = 1;
    $is_English = 0;
    $is_noun = 0;

  }

  #Non-entry pages e.g. Help pages have non-zero ns
  if (/<ns>(?<ns>\d+)<\/ns>/) {
    $ns = $+{ns};
  }
  next if $ns;

  #Ensure the term is English
  $is_English++ if /=English=/;
  next unless $is_English;
  
  #Ensure the term is a noun
  $is_noun++ if /=Noun=/;
  next unless $is_noun;

  #Don't want weirdness
  next if $term =~ /$unsanitary/;

  #Skip acronyms and intialisms
  next if $term =~ /^[^a-z]{2,}$/;

  #All terms are stored in lower case, and 
  # matched case-insensitively. This is a tradeoff
  # between allowing for users' idiosyncracies in 
  # capitalisation (e.g. 'What Is The Plural Of Starfish')
  # and the edge case where two different case-forms
  # of the same word (e.g. 'Zulu' and 'zulu') have 
  # different correct plural forms (this is extremely
  # unlikely, I couldn't actually find any real examples).
  # However, even in this edge case it will still return
  # all the possible correct pluralisations.
  my $termKey = lc($term);

  #Parse the Template:en-noun to grab the plural
  # Reference: https://en.wiktionary.org/wiki/Template:en-noun
  if (/{{en-noun(\|(?<plurals>[^\}]+))?}}/) {

    #If no plural form information is given,
    # the plural form is '-s'
    if (! $+{plurals}) {
      $plurals{$termKey}{$term}{$term . 's'} = undef;;
      next; 
    }

    my @forms = split(/\|/, $+{plurals});

    foreach my $form (@forms) {

      #Hyphen indicates uncountable or usually uncountable,
      # meaningless for our purposes
      if ($form eq '-') {
        next; 

      #Tilde indicates countable and uncountable, with -s
      # pluralisation unless an alternative is specified
      } elsif ($form eq '~') {
        $plurals{$termKey}{$term}{$term . 's'} = undef if @forms == 1;

      #Exclamation point indicates an unattested plural,
      # question mark indicates uncertain or unknown;
      # ignore these for now
      } elsif ($form eq '!' || $form eq '?') {
        next;

      #'s' and 'es' indicate standard -s or -es forms
      } elsif ($form eq 's' || $form eq 'es') {
        warn('Form ' . $form . ' looks unsanitary') if $form =~ /$unsanitary/;
        next if $form =~ /$unsanitary/;
        $plurals{$termKey}{$term}{$term . $form} = undef;

      #Markup in square brackets is used for multi-word
      # terms, with -s pluralisation unless an alternative
      # is specified
      } elsif ($form =~ /\[|\]/) {
        $plurals{$termKey}{$term}{$term . 's'} = undef if @forms == 1;
      
      #Anything else is an explicit specification of a
      # plural form, usually irregular
      } else {
        next if $form =~ /$unsanitary/;
        $plurals{$termKey}{$term}{$form} = undef;
      }
    }
  }
}
say "\r$. lines processed";
close DICT;

#Write as JSON
say "Writing plurals.json...";
my $json = encode_json \%plurals;
write_file('plurals.json', { binmode => ':raw' }, $json);
say "Done";

#Write as output.txt
say "Writing output.txt...";
open OUTPUT, '>:utf8', 'output.txt';
foreach my $term (sort keys %plurals) {

  my $output = join("\t",(
    $term, #Title
    'A', #Type
    '', #Only for redirects
    '', #Other uses
    '', #Categories
    '', #References
    '', #See also
    '', #Further reading
    '', #External link
    '', #Disambiguation
    '', #Images
    wrap_html($term, %{$plurals{$term}}), #Abstract
    wiktionary_URL($term) #Source URL
  ));

  say OUTPUT $output;

}
close OUTPUT;
exit;
    
sub wrap_html {

  (my $lc, my %pluralForms) = @_;

  #Inject CSS and open div
  state $css = read_file('style_flat.css');
  chomp $css;
  my $html = "<style type='text/css'>$css</style>";
  $html .= "<div class='zci--plural'>";

  #For each matching caseform, construct a natural-language
  # statment of the plural forms with links to Wiktionary
  my @statements;
  foreach my $caseForm (sort keys %pluralForms) {
    my @pluralForms = map { "<span class='plural'><a href='" . wiktionary_URL($_) . "'>" . $_ . "</a></span>" } keys %{$pluralForms{$caseForm}};
    my $article = @statements == 0 ? 'The' : 'the';
    my $statement = "<span class='label'>$article plural of $caseForm is</span> " . natural_list(@pluralForms);
    push(@statements, $statement);
  }

  #Join the statements together and capitalise
  # the first word
  my $statement = join("<span class='label'>; </span>", @statements);
  $statement = ucfirst($statement);
  $html .= $statement;
  
  #Link back to Wiktionary and close off
  $html .= "<div class='source'><a href='" . wiktionary_URL($lc) ."'>More at Wiktionary</a></div>";
  $html .= "</div>";

  return $html;

}

sub wiktionary_URL {

  my $term = shift;
  $term = uri_escape_utf8($term);
  return 'https://en.wiktionary.org/wiki/' . $term;

}

sub natural_list {

  my @items = @_;
  return WORDLIST(sort @items, {conj => "<span class='label'>or</span>"});

}

sub nested_forms {

  my %hash = @_;
  my @forms;
  foreach my $key (keys %hash) {
    push(@forms, $_) for keys %{$hash{$key}};
  }
  return sort @forms;

}

