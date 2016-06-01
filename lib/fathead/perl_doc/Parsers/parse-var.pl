#!/usr/bin/env perl
use strict;
use warnings;
binmode STDOUT, ":utf8";
use IO::All;
use Mojo::DOM;
use Data::Dumper;
use Term::ProgressBar;
use Cwd qw( getcwd );
use Util qw( get_row trim_abstract);

my @pages = glob(getcwd(). "/../download/perlvar/*.html");
my $skip = qr/To get the best experience. |Please note: Many features/;

foreach my $page (@pages){
    my $html < io($page);
    $html =~ s/<a.+?href=".+?>(.+)<\/a>/$1/g;
    $html =~ s/<code.+?><a.+?href=".+?>(.+)<\/a><\/code>/<code>$1<\/code>/g;
    $html =~ s/<strong>//g;
    $html =~ s/<\/strong>//g;

    my $dom = Mojo::DOM->new($html);

    $dom = $dom->at('div[id="content_body"]');

    my $description;
    foreach my $n ($dom->find('*')->each){
        # find li, set flag
        # if 2 elements
        # 1st redirece
        # 2nd parse content for article
    }
    next unless $description;
    $description = trim_abstract($description, 100);

    $page =~ s/^.*download\///;
    $page =~ s/\.html$//;
    $page =~ s/functions_//;

    $description = "<code><br>$hint<br></code><br>". $description;

    printf("%s\n", get_row($title, $description, "http://perldoc.perl.org/$page", 'A'));
}
