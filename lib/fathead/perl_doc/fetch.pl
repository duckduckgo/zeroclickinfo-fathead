#!/usr/bin/env perl
use strict;
use warnings;
use IO::All;
binmode STDOUT, ":utf8";
use Mojo::DOM;
use Mojo::DOM::CSS;
use LWP::Simple;
use Data::Dumper;
use Term::ProgressBar;
use Cwd qw( getcwd );

my @pages = qw/ internals utilities pragmas language functions /;
#my @pages = qw/ language /;

# skip sidebar tags
my $skip_qr = qr/index-|preferences/;

my $download_dir = getcwd(). '/download';
warn $download_dir;
mkdir $download_dir;

foreach my $page (@pages){
    # get each index page
    my $html = get "http://perldoc.perl.org/index-$page.html";
    my @inner_links;

    # dom object
    my $dom = Mojo::DOM->new($html);

    # parse out links:
    #  ul ->
    #       <a href: link to subpage
    #       inner text: short description. not always included
    for my $ul ( $dom->find('ul')->each ){
        for my $a ($ul->find('a[href]')->each ){
            if(my ($link) = $a =~ /href="(.*)"/){
                push(@inner_links, $link) unless $link =~ /$skip_qr/;
            }
        }
    }

    printf("Found %d links on %s\n", scalar @inner_links, $page);

    my $progress = Term::ProgressBar->new( scalar @inner_links );

    mkdir "$download_dir/$page";

    while( my ($index, $link) = each @inner_links ){
        $progress->update($index);
        my $name = $link;
        $link = "http://perldoc.perl.org/$link";
        $name =~ s/\//_/g;

        my $status = getstore($link, "$download_dir/$page/$name.html");
        sleep(3);
    }
}

# get perlvar page
getstore("http://perldoc.perl.org/perlvar.html", "$download_dir/perlvar/perlvar.html");
