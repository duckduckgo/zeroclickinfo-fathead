#!/usr/bin/env perl

use strict;
use warnings;

use IO::All;

my $source = 'https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/plain/Documentation/sysctl/kernel.txt';
open my $output, '>', 'output.txt' or die 'Could not open output.txt for writing';

my $documentation = io->file('kernel.txt')->slurp or die "Could not open ./kernel.txt\nHave you run ./parse.sh?";
my @sections = split /==============================================================/, $documentation;
shift @sections for 1..2;

my %answers;

for my $section (@sections) {
    my $file = (grep { s/:// } split /\n/, $section)[0];
    next unless $file;
    my $details = $section =~ s/\n\n$file:\n\n|\n*$//gr;
    $details =~ s/\n/ /g;
    $answers{$file} = $details;
    map { $answers{$_} = $details } split / +& +/, $file if $file =~ /&/;
    map { s/\)$//g; $answers{$_} = $details } split / +\(/, $file if $file =~ /\(/;
    map { s/and +//; $answers{$_} = $details } split /, +/, $file if $file =~ /, +/;
    map { s/_/ /g; $answers{$_} = $details } sort keys %answers;
    map { s/-/ /g; $answers{$_} = $details } sort keys %answers;
    map { s/-//g; $answers{$_} = $details } sort keys %answers;
}

%answers = map { lc, $answers{$_} } keys %answers;

print $output join "\n", map {
    join "\t", 
        $_,           # Title
        'A',          # Type
        '',           # Redirect
        '',           # Other uses
        '',           # Categories
        '',           # References
        '',           # See also
        '',           # Further reading
        '',           # External links
        '',           # Disambiguation
        '',           # Images
        $answers{$_}, # Abstract
        $source       # Source URL
} sort keys %answers;
