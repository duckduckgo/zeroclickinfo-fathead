#!/usr/bin/env perl

use strict;
use warnings;

use IO::All;

my $source = 'https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/plain/Documentation/sysctl/kernel.txt';
open my $output, '>', 'output.txt';

my $documentation = io->file('kernel.txt')->slurp or die 'Could not open ./kernel.txt';
my @sections = split /==============================================================/, $documentation;
shift @sections for 1..2;

my %answers;

for my $section (@sections) {
    my $file = (grep {s/://} (split /\n/, $section))[0];
    next unless $file;
    my $details = $section =~ s/\n\n$file:\n\n|\n*$//gr;
    $details =~ s/\n/ /g;
    $answers{$file} = $details;
    map { $answers{$_} = $details } split / & /, $file if $file =~ /&/;
    map { chop if /\)$/; $answers{$_} = $details } split / \(/, $file if $file =~ /\(/;
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
} keys %answers;
