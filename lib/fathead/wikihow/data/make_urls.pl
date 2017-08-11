#!/usr/bin/perl
# Converts wikiHow titles into URLs

use strict;
use warnings;

use 5.14.0;
use File::Slurp;

my @titles = read_file('lib/fathead/wikiHow/data/wikihowcom-20170713-titles.txt');
open(OUTPUT, ">urls.txt") || die "Can't write file";

foreach my $title (@titles) {
    $title =~ s/\s/-/g;
    say "http://wikihow.com/" . $title;
    say OUTPUT "http://wikihow.com/" . $title;
}

close(OUTPUT);