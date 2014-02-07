#!/usr/bin/env perl

use strict;
use warnings;

use IO::All;

my $source = 'https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/plain/Documentation/sysctl';
open my $output, '>', 'output.txt' or die 'Could not open output.txt for writing';

sub parse {
    my ($filename, $shift_sections, $pop_sections) = @_;
    my $documentation = io->file("documentation/$filename")->slurp
        or die "Could not open ./kernel.txt\nHave you run ./fetch.sh?";
    my @sections = split /==============================================================/, $documentation;
    shift @sections for 1..$shift_sections;
    pop @sections for 1..$pop_sections;

    my %answers;

    for my $section (@sections) {
        my $file = (grep { s/:// } split /\n/, $section)[0];
        next unless $file;
        my $details = $section =~ s/\n\n$file:\n\n|\n*$//gr;
        $details =~ s/\n/ /g;
        $answers{$file} = $details;
        map { $answers{$_} = $details } split / & /, $file if $file =~ /&/;
        map { s/\)$//; $answers{$_} = $details } split / \(/, $file if $file =~ /\(/;
    }

    %answers = map { lc, $answers{$_} } keys %answers;

    join "\n", map {
        join "\t", 
            $_,                 # Title
            'A',                # Type
            '',                 # Redirect
            '',                 # Other uses
            '',                 # Categories
            '',                 # References
            '',                 # See also
            '',                 # Further reading
            '',                 # External links
            '',                 # Disambiguation
            '',                 # Images
            $answers{$_},       # Abstract
            "$source/$filename" # Source URL
    } keys %answers;
}

my %parse_options = (
    #abi    => [2, 1],
    fs     => [2, 1],
    kernel => [2, 0],
    #net    => [2, 0],
    #sunrpc => [2, 0],
    #vm     => [2, 0],
);

print $output map { parse "$_.txt", @{$parse_options{$_}} } keys %parse_options;
