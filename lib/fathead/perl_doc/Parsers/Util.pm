#!/usr/bin/env perl
package Util;
use strict;
use warnings;
use Data::Dumper;
use Exporter qw/import/;
our @EXPORT_OK = qw/get_row/;

# return tab formatted row for fathead output.txt file
sub get_row {
    my ($title, $abs, $url, $type) = @_;
    $type = 'A' unless $type;
    return "$title\t$type\t\t\t\t\t\t\t\t\t\t$abs\t$url";
}
1;
