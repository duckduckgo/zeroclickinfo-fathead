#!/usr/bin/perl

use strict;
use warnings;

use IO::Uncompress::AnyInflate;
use XML::LibXML::Reader;

my $start_time = time;

my $trace = $| = 1; # 1 = enable debug messages; 0 = disable messages

trace('Starting CVE parser at ' . localtime);

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

trace('Starting XML tree traversal');

# Parse XML tree as a list of <item> tags, extracting the CVE ID and description.
while ($reader->nextElement('item') && $reader->readState() != XML_READER_ERROR) {
    my $name = $reader->getAttribute('name');
    my $desc = $1 if $reader->readInnerXml =~ m|<desc.*>(.*)</desc>|;
    my $url  = "http://www.cvedetails.com/cve/$name";

    trace("Adding entry for $name");

    print $outfh join "\t", (
        $name,    # Title
        'A',      # Type
        '',       # Redirect
        '',       # Other uses
        '',       # Categories
        '',       # References
        '',       # See also
        '',       # Further reading
        '',       # External links
        '',       # Disambiguation
        '',       # Images
        $desc,    # Abstract
        $url,     # Source URL
        "\n"
    );
}

trace('Finished XML tree traversal');
trace('Closing filehandles');

$infh->close();
$outfh->close();
$reader->close();

my $end_time = time;

trace('Total time: ' . ($end_time - $start_time) . ' seconds');

# For debug purposes only. Displays diagnostic message if $trace is defined.
sub trace {
    my $message = shift;
    print "[DEBUG] $message\n" if $trace;
}
