#!/usr/bin/env perl

use strict;
use warnings;
use Data::Dumper;

use IO::All;

my $source = 'https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/plain/Documentation/sysctl';
open my $output, '>', 'output.txt' or die 'Could not open output.txt for writing';

sub parse {
    my ($filename, $shift_sections, $pop_sections) = @_;
    my $documentation = io->file("documentation/$filename")->slurp
        or die "Could not open ./kernel.txt\nHave you run ./fetch.sh?";
    my @sections = split /==========.{20,60}==========/, $documentation;
    shift @sections for 1..$shift_sections;
    pop @sections for 1..$pop_sections;

    my %answers;

    for my $section (@sections) {
        my $file = (grep { s/(?:- )?(.):?/$1/ } split /\n/, $section)[0];
        next unless $file;
        my $details = $section =~ s/\n\n(- )?$file:?\n(-+)?\n|\n*$//gr;
        $details =~ s/\n/ /g;
        $answers{$file} = $details;

        map { $answers{$_} = $details } split / (?:&|,) /, $file if $file =~ / (&|,) /;
        map { chop; $answers{$_} = $details } split / \(/, $file if $file =~ /\(/;

        #map { s/_/-/g; $answers{$_} = $details } split /_/, $file if $file =~ /_/;
        #map { s/-/_/g; $answers{$_} = $details } split /_/, $file if $file =~ /-/;
        #map { s/_/ /g; $answers{$_} = $details } split /_/, $file if $file =~ /_/;
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
    #abi    => [2, 0], # working
    #fs     => [2, 1], # working, but misses last section
    #kernel => [2, 0], # not working
    #net    => [2, 0], # not working, different layout
    #sunrpc => [2, 0], # manually written below
    vm     => [2, 0], # not working
);

my $sunrpc_documentation = "The files in the /proc/sys/sunrpc directory can be used to (re)set the debug flags of the SUN Remote Procedure Call (RPC) subsystem in the Linux kernel. This stuff is used for NFS, KNFSD and maybe a few other things as well.\n\nThe files in there are used to control the debugging flags: rpc_debug, nfs_debug, nfsd_debug and nlm_debug.\n\nThese flags are for kernel hackers only. You should read the source code in net/sunrpc/ for more information.";

my @sunrpc_files = ('rpc_debug', 'nfs_debug', 'nfsd_debug', 'nlm_debug');

print $output
    join "\n", map {
        join "\t", 
            $_,                    # Title
            'A',                   # Type
            '',                    # Redirect
            '',                    # Other uses
            '',                    # Categories
            '',                    # References
            '',                    # See also
            '',                    # Further reading
            '',                    # External links
            '',                    # Disambiguation
            '',                    # Images
            $sunrpc_documentation, # Abstract
            "$source/sunrpc.txt"   # Source URL
    } @sunrpc_files;

print $output map { parse "$_.txt", @{$parse_options{$_}} } keys %parse_options;
