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
use File::Find::Rule;

my @pages = File::Find::Rule->file
    ->name('perlfaq*.html')
    ->in('../perldoc-html');

sub get_link {
    my ($n, $title) = @_;
    my $a = $title->previous('a');
    return unless $a;
    $a = $a->attr('name');
    return "http://perldoc.perl.org/perlfaq$n.html#$a";
}

foreach my $page (@pages){
    my $html < io($page);
    my ($faq_n) = $page =~ /perlfaq(\d+)\.html/ or next;
    $html =~ s/<a.+?href=".+?>(.+)<\/a>/$1/g;
    $html =~ s/<code.+?><a.+?href=".+?>(.+)<\/a><\/code>/<code>$1<\/code>/g;
    $html =~ s/<strong>//g;
    $html =~ s/<\/strong>//g;

    my $dom = Mojo::DOM->new($html);

    foreach my $faq_title ($dom->find('h2')->each) {
        my $description;
        foreach my $para ($faq_title->following('p')->each) {
            $description .= $para;
        }
        $description =~ s/\n/ /g;
        my $title = $faq_title->text;
        my $link = get_link($faq_n, $faq_title);
        next unless $link;
        printf("%s\n", get_row($title, $description, $link, 'A'));
    }
}
