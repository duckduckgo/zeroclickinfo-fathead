#!/usr/bin/env perl
#
#

use strict;
use warnings;
use FindBin;

binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";

my $ia = $ARGV[0] or die "Please specify an IA";
my $path = $FindBin::Dir . "/../fathead/" . $ia . "/";
my $file = $path . 'output.txt';

# Read article titles from Output.txt
open my $fh, '<:encoding(UTF-8)', $file or die "Could not open $file: $!";

my @titles;
my @fields;

while ( my $line = <$fh>)  {   
    @fields = split("\t+", $line);
    $fields[1] =~ /A/ or next;  #skip unless it's an article
    push @titles, $fields[0]; #first field is the title
}
close $fh;

# Write list of articles to the new file
open my $fw, '>:encoding(UTF-8)', $path . "articles.txt" or die();

foreach my $title (sort @titles) {
    print $fw $title . "\n";
}
close $fw;
