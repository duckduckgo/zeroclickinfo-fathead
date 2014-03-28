#!/usr/bin/env perl

use strict;
use warnings;

use IO::All;
use Data::Dumper;

my $source = 'https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/plain/Documentation/sysctl/kernel.txt';
open my $output, '>', 'output.txt' or die 'Could not open output.txt for writing';

my $documentation = io->file('kernel.txt')->slurp or die "Could not open ./kernel.txt\nHave you run ./parse.sh?";
my @sections = split /==============================================================/, $documentation;
shift @sections for 1..2;

my %answers;

sub include_alternate_spellings {
    my $file = shift;
    if ($file =~ /_/) {
        my $alternate_spelling = $file;
        $alternate_spelling =~ s/_/ /g;
        $answers{$alternate_spelling}{redirect} = $file;

        $alternate_spelling = $file;
        $alternate_spelling =~ s/_/-/g;
        $answers{$alternate_spelling}{redirect} = $file;

        $alternate_spelling = $file;
        $alternate_spelling =~ s/_/ /g;
        $answers{$alternate_spelling}{redirect} = $file;
    }
    if ($file =~ /-/) {
        my $alternate_spelling = $file;
        $alternate_spelling =~ s/-/ /g;
        $answers{$alternate_spelling}{redirect} = $file;

        $alternate_spelling = $file;
        $alternate_spelling =~ s/-//g;
        $answers{$alternate_spelling}{redirect} = $file;

        $alternate_spelling = $file;
        $alternate_spelling =~ s/-/_/g;
        $answers{$alternate_spelling}{redirect} = $file;
    }
    if ($file =~ / /) {
        my $alternate_spelling = $file;
        $alternate_spelling =~ s/ /-/g;
        $answers{$alternate_spelling}{redirect} = $file;

        $alternate_spelling = $file;
        $alternate_spelling =~ s/ /_/g;
        $answers{$alternate_spelling}{redirect} = $file;
    }
}

for my $section (@sections) {
    my $file = (grep { s/:// } split /\n/, $section)[0];
    next unless $file;
    my $details = $section =~ s/\n\n$file:\n\n|\n*$//gr;
    $details =~ s/\n/ /g;
    $answers{$file}{abstract} = $details;
    include_alternate_spellings $file;
    map {
        $answers{$_}{redirect} = $file;
        include_alternate_spellings $_;
    } split / +& +/, $file if $file =~ /&/;
    map {
        s/\)$//g;
        $answers{$_}{redirect} = $file;
        include_alternate_spellings $_;
    } split / +\(/, $file if $file =~ /\(/;
    map {
        s/and +//;
        $answers{$_}{redirect} = $file;
        include_alternate_spellings $_;
    } split /, +/, $file if $file =~ /, +/;
}

%answers = map { lc, $answers{$_} } keys %answers;

print $output join "\n", map {
    join "\t", 
        $_,                               # Title
        exists $answers{$_}{redirect} ?   # Type
            'R' : 'A', 
        exists $answers{$_}{redirect} ?   # Redirect
            $answers{$_}{redirect} : '', 
        '',                               # Other uses
        '',                               # Categories
        '',                               # References
        '',                               # See also
        '',                               # Further reading
        '',                               # External links
        '',                               # Disambiguation
        '',                               # Images
        exists $answers{$_}{abstract} ?   # Abstract
            $answers{$_}{abstract} : '',
        $source                           # Source URL
} sort keys %answers;
