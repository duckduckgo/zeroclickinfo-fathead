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
    assign_parsers => sub { [\&gtk_api_parse_page, \&gtk_api_parse_functions] },
);

sub page_main_title {
    my ($dom) = @_;
    $dom->at('span.refentrytitle')->all_text;
}

sub normalize_program_listing {
    my ($dom) = @_;
    $dom->find('pre.programlisting')->map(
        sub { $_->content($_->content =~ s{\n}{<br />}gr) },
    );
    return $dom;
}

sub gtk_normalize_dom {
    my ($dom) = @_;
    normalize_listing_code(normalize_program_listing($dom));
}

sub normalize_listing_code {
    my ($dom) = @_;
    $dom->find('table td.listing_code')->map(tag => 'span')->map('parent')
        ->map(tag => 'span');
    $dom->find('td.listing_lines')->map(
        sub { $_->attr(style => 'display:none;') },
    );
    return $dom;
}

# Main page description
sub gtk_api_parse_page {
    my ($dom) = @_;
    $dom = gtk_normalize_dom($dom);
    my $title = page_main_title($dom);
    my $includes = $dom->at('div.refsect1 > a[name$="includes"]')->parent->to_string;
    my $description = $dom->at('div.refsect1 > a[name$="description"]')->parent->to_string;
    my $abstract = "$includes\n$description";
    my $a = article(
        title => $title,
        abstract => $abstract,
        related => ["$title functions"],
    );
    return {
        articles => [$a],
        disambiguations => [],
    };
}

sub gtk_api_parse_functions {
    my ($dom) = @_;
    $dom = gtk_normalize_dom($dom);
    my $fnd = $dom->at('div.refsect1 > a[name$="functions_details"]')->parent;
    my @articles;
    foreach my $fn ($fnd->find('div.refsect2')->each) {
        push @articles, article(
            anchor => $fn->at('a[name]')->attr('name'),
            title  => $fn->at('h3')->text =~ s/ \(\)//r,
            abstract => $fn->find('h3 ~ *')->join(),
            categories => [
                page_main_title($dom) . ' functions',
            ],
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
