#!/usr/bin/env perl
#
#

use strict;
use warnings;
use Text::CSV;

binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";

my $ia = $ARGV[0];
my $path = "../fathead/" . $ia . "/";
my $csv = Text::CSV->new({sep_char => "\t"});
my $file = $path . 'Output.txt';
open(my $fh, '<:encoding(UTF-8)', $file) 
    or die "Could not open $file: $!";

my @titles;

while( my $line = $csv->getline($fh))  {   
    $file->[1] =~ /A/ or next;  #skip unless it's an article
    push @titles, $file->[0]; #first field is the title
}
