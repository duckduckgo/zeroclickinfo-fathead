#!/usr/bin/perl

use strict;
use warnings;

use IO::Uncompress::AnyInflate;
use XML::LibXML::Reader;

my $infile  = shift or die '[ERROR] Required input file not given.';
my $outfile = shift or die '[ERROR] Required output file not given.';

my $infh  = new IO::Uncompress::AnyInflate $infile
    or die "[ERROR] Failed to open input file: $infile";
my $outfh = new IO::File;

$outfh->open(">$outfile") or die "[ERROR] Failed to open output file: $outfile";
$outfh->binmode(':utf8');

my $reader = new XML::LibXML::Reader IO => $infh;

$infh->close();
$outfh->close();
$reader->close();
