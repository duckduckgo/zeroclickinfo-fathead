#!/usr/bin/perl

use strict;
use warnings;

use IO::Uncompress::AnyInflate qw/$AnyInflateError/;
use XML::LibXML::Reader;

my $start_time = time;

my $trace = $| = 0; # 1 = enable debug messages; 0 = disable messages

trace('Starting CVE parser at ' . localtime);

my $infile  = shift or die '[ERROR] Required input archive not given.';
my $outfile = shift or die '[ERROR] Required output file not given.';

trace("Opening input archive: $infile");

my $infh  = new IO::Uncompress::AnyInflate $infile or die '[ERROR] ' . ucfirst $AnyInflateError;
my $outfh = new IO::File;

trace("Opening output file: $outfile");

$outfh->open(">$outfile") or die "[ERROR] Cannot open file: $!";
$outfh->binmode(':utf8');

trace('Creating XML reader from input archive');

my $reader = new XML::LibXML::Reader IO => $infh;

trace('Starting XML tree traversal');

# Parse XML tree as a list of <item> tags, extracting the CVE-ID and description.
while ($reader->nextElement('item') && $reader->readState() != XML_READER_ERROR) {
    my $name      = $reader->getAttribute('name');
    my $inner_xml = $reader->readInnerXml;

    my ($desc, $url) = parse_entry($name, $inner_xml);

    # Ignore reserved and rejected identifiers.
    if (not (defined $url or defined $desc)) {
        trace("Ignoring entry for $name (reserved/reject)");
        next;
    }
    else {
        trace("Adding entry for $name");
    }

    add_entry($outfh, $name, $desc, $url);
}

$infh->close();
$outfh->close();
$reader->close();

my $end_time = time;

trace('Total time: ' . ($end_time - $start_time) . ' seconds');

# Parses the inner XML of a given CVE entry and returns the appropriate URL
# and descrption.
#
# Returns the description and associated URL for a valid CVE-ID. Rejected and
# reserved identifiers return undef for both the description and URL.
sub parse_entry {
    my ($name, $inner_xml) = @_;
    my $desc = $1 if $inner_xml =~ m|<desc.*>(.*)</desc>|;
    my $url  = "http://www.cvedetails.com/cve/$name";

    if ($desc =~ /^\*\* (REJECT|RESERVED) \*\* /) {
        $desc = undef;
        $url  = undef;
    }
    else {
        $desc = "Vulnerability description: $desc";
    }

    return ($desc, $url);
}

# Adds a given CVE entry to the output file.
sub add_entry {
    my ($fh, $name, $desc, $url) = @_;

    print $fh join "\t", (
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

    # Add additional entry with xxxx-yyyy component for "cve" trigger keyword.
    my $short = $name;
    $short =~ s/^CVE-//;
    print $fh join "\t", ($short, 'R',
                          $name, '', '', '', '', '', '', '', '',
                          '', '', "\n");
}

# For debug purposes only. Displays diagnostic message if $trace is defined.
sub trace {
    my $message = shift;
    print "[DEBUG] $message\n" if $trace;
}
