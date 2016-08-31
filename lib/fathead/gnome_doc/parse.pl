#!/usr/bin/env perl

use strict;
use warnings;

use Util qw(article disambiguation indexer parser);

my $gnome_gtk = 'https://developer.gnome.org/gtk3/stable';

my $gtk_api_symbol_index = indexer(
    index_url => "$gnome_gtk/api-index-full.html",
    index_function => sub {
        my ($dom) = @_;
        $dom->find('div.index a.link[title]:last-child')->map(attr => 'href')->to_array;
    },
    assign_parsers => sub { [\&gtk_api_parse_functions] },
);

sub gtk_api_parse_functions {
    my ($dom) = @_;
    my $fnd = $dom->at('div.refsect1 > a[name$="functions_details"]')->parent;
    my @articles;
    foreach my $fn ($fnd->find('div.refsect2')->each) {
        $fn->find('pre.programlisting')->map(
            sub { $_->content($_->content =~ s/\s{2,}/ /gr); },
        );
        push @articles, article(
            anchor => $fn->at('a[name]')->attr('name'),
            title  => $fn->at('h3')->text =~ s/ \(\)//r,
            abstract => $fn->find('h3 ~ *')->join(),
        );
    }
    return {
        articles => \@articles,
        disambiguations => [],
    };
}

my $parser = parser(
    indexers  => [$gtk_api_symbol_index],
);

$parser->parse;
