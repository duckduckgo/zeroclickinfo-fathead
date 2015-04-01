#!/usr/bin/perl

use Modern::Perl 2013;
use autodie;
use XML::Twig;
use URI::Escape qw(uri_escape_utf8);
use Lingua::EN::Inflect qw(WORDLIST);
use utf8;
$|++;

#Regex to catch weirdness (malformed tags etc.)
# we don't want leaking into the results
my $unsanitary = qr/[^\p{L}\s\-']/;

#Define triggers
my @triggersStart = ("plural of", "pluralise", "pluralize", "what is the plural of");
my @triggersEnd = ("plural");

my $wiktionary = 'download/wiktionary.xml';

#Get plural forms from wiktionary
my $processed;
my %plurals;
say "Processing wiktionary...";
my $wiktionaryTwig = XML::Twig->new( twig_handlers => { page => \&page } );
$wiktionaryTwig->parsefile($wiktionary);
say "\r$processed terms processed";

#Write output file
say 'Writing output...';
open my $outputFH, '>:utf8', 'output.txt';
foreach my $key (keys %plurals) {
    print_output($key, $_ . ' ' . $key) for @triggersStart;
    print_output($key, $key . ' ' . $_) for @triggersEnd;
}
close $outputFH;

exit;

#Article parser
sub page {

    (my $twig, my $page) = @_;

    ++$processed;
    print "\r$processed terms processed" unless $processed % 1000;

    #Get the title of the page
    my $term = $page->first_child_text('title');

    #Skip Wiktionary internal pages
    return if $term =~ /^Wiktionary:|
                        ^Template:|
                        ^Index: 
    /x;

    #Don't want weirdness
    return if $term =~ /$unsanitary/;

    #Skip acronyms and intialisms
    return if $term =~ /^[^a-z]{2,}$/;

    #Get the wikitext of the page
    my $wikitext = $page->first_child('revision')->first_child_text('text');

    #Find and parse Template:en-noun templates
    # Reference: https://en.wiktionary.org/wiki/Template:en-noun
    while ($wikitext =~ /{{en-noun(\|(?<plurals>[^\}]+))?}}/g) {

        #If no plural form information is given,
        # the plural form is '-s'
        if (! $+{plurals}) {
            $plurals{lc($term)}{$term}{$term .'s'}++;
            return; 
        }

        my @forms = split(/\|/, $+{plurals});

        foreach my $form (@forms) {

            #Hyphen indicates uncountable or usually uncountable,
            # meaningless for our purposes
            if ($form eq '-') {
                return; 

            #Tilde indicates countable and uncountable, with -s
            # pluralisation unless an alternative is specified
            } elsif ($form eq '~') {
                $plurals{lc($term)}{$term}{$term .'s'}++;

            #Exclamation point indicates an unattested plural,
            # question mark indicates uncertain or unknown;
            # ignore these for now
            } elsif ($form eq '!' || $form eq '?') {
                return;

            #'s' and 'es' indicate standard -s or -es forms
            } elsif ($form eq 's' || $form eq 'es') {
                $plurals{lc($term)}{$term}{$term . $form}++;

            #Markup in square brackets is used for multi-word
            # terms, with -s pluralisation unless an alternative
            # is specified
            } elsif ($form =~ /\[|\]/) {
                $plurals{lc($term)}{$term}{$term .'s'}++;

            #Anything else is an explicit specification of a
            # plural form, usually irregular
            } else {
                $plurals{lc($term)}{$term}{$form}++;
            }
        }
    }

    #Free up memory
    $page->purge;
    $twig->purge;
}

sub print_output {

    (my $key, my $trigger) = @_;

    my $output = join("\t",(
            $trigger, #Title
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
            wrap_answer($key), #Abstract
            wiktionary_URL($key) #Source URL
            ));
    say $outputFH $output;
}

#Wrap an answer up into an English statement
sub wrap_answer {

    (my $key) = shift;

    #For each matching caseform, construct a natural-language
    # statment of the plural forms with links to Wiktionary
    my @statements;
    foreach my $caseForm (sort keys %{$plurals{$key}}) {
        my $article = @statements == 0 ? 'The' : 'the';
        my $statement = "$article plural of $caseForm is " . natural_list(keys %{$plurals{$key}{$caseForm}});
        push(@statements, $statement);
    }

    #Join the statements together into a readable sentence
    my $statement = join("; ", @statements);
    $statement = ucfirst($statement);
    $statement .= '.';

    return $statement;

}

sub wiktionary_URL {
    my $term = shift;
    $term = uri_escape_utf8($term);
    return 'https://en.wiktionary.org/wiki/' . $term;
}

sub natural_list {
    return WORDLIST(sort @_, {conj => "or"});
}
