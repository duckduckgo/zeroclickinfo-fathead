#!/usr/bin/perl

use Modern::Perl 2013;
use autodie;
use XML::Twig;
use URI::Escape qw(uri_escape_utf8);
use Lingua::EN::Inflect qw(WORDLIST);
use utf8;
$|++;

# Regex to catch weirdness (malformed tags etc.) we don't want leaking into
# the results
my $unsanitary = qr/[^\p{L}\s\-']/;

# Get plural forms from Wiktionary
my $processed;
my $wiktionary = 'download/wiktionary.xml';
my %plurals;
say "Processing wiktionary...";
my $wiktionaryTwig = XML::Twig->new( twig_handlers => { page => \&page } );
$wiktionaryTwig->parsefile($wiktionary);
say "\r$processed terms processed";

# Write output file
say 'Writing output...';
open my $outputFH, '>:utf8', 'output.txt';
print_output($_) for keys %plurals;
close $outputFH;

exit;

# Article parser
sub page {

    (my $twig, my $page) = @_;

    ++$processed;
    print "\r$processed terms processed" unless $processed % 1000;

    # Get the title of the page
    my $term = $page->first_child_text('title');

    # Skip Wiktionary internal pages
    return if $term =~ /^Wiktionary:|
                        ^Template:|
                        ^Index: 
    /x;

    # Don't want weirdness
    return if $term =~ /$unsanitary/;

    # Skip acronyms and intialisms
    return if $term =~ /^[^a-z]{2,}$/;

    # Get the wikitext of the page
    my $wikitext = $page->first_child('revision')->first_child_text('text');

    # Find and parse Template:en-noun templates
    # Reference: https://en.wiktionary.org/wiki/Template:en-noun
    while ($wikitext =~ /{{en-noun\|?(?<plurals>[^\}]+)}}/g) {

        # Note it's possible for @forms to be length 0
        my @forms = split(/\|/, $+{plurals});

        # Forms that look like 'head=[[hot]] [[dog]]' are functionally
        # equivalent to that token just not existing, since if no other form
        # is given the plural '-s' is assumed while if other forms are given
        # '-s' is not assumed. So we'll splice the head= forms out.
        while (my ($index, $form) = each @forms) {
            if ($form =~ /^head=.+$/) {
                splice(@forms, $index, 1);
            }
        }

        # If no plural form information is given, the plural form '-s' is
        # assumed
        if (scalar @forms == 0) {
            $plurals{lc($term)}{$term}{$term .'s'}++;
            return; 
        }

        # Sometimes a 'qualifier' form is included the list, which is actually
        # a qualifier for a form earlier in the list (for example, 'moose' has
        # the inflection given as
        # 'en-noun|moose|mooses|meese|pl3qual=uncommon, humorous'. So we have
        # to detect these, and back-apply the qualification to the relevant
        # form
        while (my ($index, $form) = each @forms) {
            if ($form =~ /^pl(?<number>\d+)qual=(?<qualifier>.+)$/) {
              my $targetIndex = $+{number} - 1;
              $forms[$targetIndex] = $forms[$targetIndex] . " (" . $+{qualifier} . ")";
              splice(@forms, $index, 1);
            }
        }

        foreach my $form (@forms) {

            # Hyphen indicates uncountable or usually uncountable, meaningless
            # for our purposes
            if ($form eq '-') {
                return; 

            # Tilde indicates countable and uncountable, with -s pluralisation
            # unless an alternative is specified
            } elsif ($form eq '~') {
                $plurals{lc($term)}{$term}{$term .'s'}++;

            # Exclamation point indicates an unattested plural, question mark
            # indicates uncertain or unknown; ignore these for now
            } elsif ($form eq '!' || $form eq '?') {
                return;

            # 's' and 'es' indicate standard -s or -es forms
            } elsif ($form eq 's' || $form eq 'es') {
                $plurals{lc($term)}{$term}{$term . $form}++;

            # Markup in square brackets is used for multi-word terms, with -s
            # pluralisation unless an alternative is specified
            } elsif ($form =~ /\[|\]/) {
                $plurals{lc($term)}{$term}{$term .'s'}++;

            # Sometimes there are undocumented tokens in the forms list. With
            # the exception of the 'qual' tokens handled above, most of these
            # are more or less useless so they will be ignored 
            } elsif ($form =~ /=/) {
              return;

            # Anything else is an explicit specification of a plural form,
            # usually irregular
            } else {
                $plurals{lc($term)}{$term}{$form}++;
            }
        }
    }

    # Free up memory
    $page->purge;
    $twig->purge;
}

sub print_output {

    (my $key) = @_;

    my $output = join("\t",(
            $key,                 # Title
            'A',                  # Type
            '',                   # Only for redirects
            '',                   # Other uses
            '',                   # Categories
            '',                   # References
            '',                   # See also
            '',                   # Further reading
            '',                   # External link
            '',                   # Disambiguation
            '',                   # Images
            wrap_answer($key),    # Abstract
            wiktionary_URL($key)  # Source URL
            ));
    say $outputFH $output;
}

# Wrap an answer up into an English statement
sub wrap_answer {

    (my $key) = shift;

    # For each matching caseform, construct a natural-language statement of
    # the plural forms with links to Wiktionary
    my @statements;
    foreach my $caseForm (sort keys %{$plurals{$key}}) {
        my $article = @statements == 0 ? 'The' : 'the';
        my $statement = "$article plural of $caseForm is " . natural_list(keys %{$plurals{$key}{$caseForm}});
        push(@statements, $statement);
    }

    # Join the statements together into a readable sentence
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
