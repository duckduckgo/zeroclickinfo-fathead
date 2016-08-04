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

my @pages = glob(getcwd(). "/../download/internals/*.html");
@pages = (@pages, 
    glob(getcwd(). "/../download/language/*.html"),
    glob(getcwd(). "/../download/pragmas/*.html"),
    glob(getcwd(). "/../download/utilities/*.html"),
);

foreach my $page (@pages){
    my $html < io($page);

    $html =~ s/<a.+?href=".+?>(.+)<\/a>/$1/g;

    my $dom = Mojo::DOM->new($html);

    my $title = $dom->at('title')->text;
    $title =~ s/\s-\s.*//;

    # iterate through page
    my $headings = $dom->find('a[name]')->map(attr => 'name');
    $_ =~ s/-/ /g for @$headings;

    my $capture = 0;
    my $description;
    foreach my $n ($dom->find('*')->each){
        next unless $n->text;

        if($n->tag eq 'h1' && $n->text eq "DESCRIPTION"){
            $capture = 1;
            next;
        }

        my $strip_heading = $n->text =~ s/-/ /gr;
        last if ($capture && grep $_  eq $strip_heading, @$headings);

        # fix span tags inside code tags
        next if ($n->tag eq 'span' && $n->parent->tag eq 'code');

        $description .= $n if $capture;
    }

    next unless $description;

    $description = trim_abstract($description, 100);

    $page =~ s/^.*(utilities|language|pragmas|internals|functions)\///;
    $page =~ s/\.html$//;

    if($title =~ /^perl/){
        my $redirect = $title;
        $redirect =~ s/^perl//;
        printf("%s\n", get_row($title, undef, undef, 'R', $redirect));
    }

       printf("%s\n", get_row($title, $description, "http://perldoc.perl.org/$page", 'A'));
}
