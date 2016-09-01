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

sub normalize_listing_code {
    my ($dom) = @_;
    # This works for the most part in getting code on the right lines.
    $dom->find('td.listing_code span')->grep(
        sub { $_->content =~ /^\s{2,}/ },
    )->map(sub {$_->prepend_content('<br />') });
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
    $dom = normalize_listing_code($dom);
    my $title = $dom->at('span.refentrytitle')->all_text;
    my $includes = $dom->at('div.refsect1 > a[name$="includes"]')->parent->to_string;
    my $description = $dom->at('div.refsect1 > a[name$="description"]')->parent->to_string;
    my $abstract = "$includes\n$description";
    my $a = article(
        title => $title,
        abstract => $abstract,
    );
    return {
        articles => [$a],
        disambiguations => [],
    };
}

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
