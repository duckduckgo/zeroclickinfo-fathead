#!/usr/bin/perl

use strict;
use warnings;

use IO::Uncompress::AnyInflate;
use XML::LibXML::Reader;

my $trace = $| = 1; # 1 = enable debug messages; 0 = disable messages

my $infile  = shift or die '[ERROR] Required input archive not given.';
my $outfile = shift or die '[ERROR] Required output file not given.';

trace("Opening input archive: $infile");

my $infh  = new IO::Uncompress::AnyInflate $infile
    or die "[ERROR] Failed to open input archive: $infile";
my $outfh = new IO::File;

trace("Opening output file: $outfile");

$outfh->open(">$outfile") or die "[ERROR] Failed to open output file: $outfile";
$outfh->binmode(':utf8');

trace('Creating XML::LibXML::Reader from input archive');

my $reader = new XML::LibXML::Reader IO => $infh;

trace('Closing filehandles');

$infh->close();
$outfh->close();
$reader->close();

# For debug purposes only. Displays diagnostic message if $trace is defined.
sub trace {
    my $message = shift;
    print "[DEBUG] $message\n" if $trace;
}
