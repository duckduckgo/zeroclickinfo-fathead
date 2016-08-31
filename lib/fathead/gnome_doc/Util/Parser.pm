package Util::Parser;

use strict;
use warnings;

binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";

use DBI;
use File::Spec;
use IO::All -utf8;
use Mojo::DOM;
use Moo;
use Text::CSV_XS;
use URI;
use List::Util qw(first);
use List::MoreUtils qw(uniq);

use Util::DB;

my %links;

has indexer => (
    is       => 'ro',
    isa      => sub { die 'Not a Util::Index' unless ref $_[0] eq 'Util::Index' },
    required => 1,
    doc      => 'Used to generate indices for parsing',
);

has db => (
    is      => 'ro',
    isa     => sub { die 'Not a Util::DB' unless ref $_[0] eq 'Util::DB' },
    doc     => 'Internal database interface',
    builder => 1,
    lazy    => 1,
);

sub _build_db {
    return Util::DB->new;
}

sub normalize_dom_links {
    my ($url, $dom)  = @_;
    $dom->find('a')->map(sub {
        my $link = $_[0]->attr('href') or return;
        $_[0]->attr(href => URI->new_abs($link, $url)->as_string);
    });
}

#######################################################################
#                       Normalize Parse Results                       #
#######################################################################

sub dom_for_parsing {
    my ($page) = @_;
    # NOTE: This probably destructively modifies the pages DOM.
    my $dom = $page->dom;
    normalize_dom_links($page->url, $dom);
    $dom->find('strong')->map('strip');
    return $dom;
}

sub parse_page {
    my ( $self, $index, $page ) = @_;
    my @parsed;
    foreach my $parser (@{$index->parsers_for($page)}) {
        push @parsed, $parser->(dom_for_parsing($page));
    }
    foreach my $parsed (@parsed) {
        for my $article ( @{ $parsed->{articles} } ) {
            # TODO: Not really sure how to get around this at the moment.
            $article->_set_page($page);
            $self->db->article($article);
        }

        for my $alias ( @{ $parsed->{aliases} } ) {
            $self->db->alias( $alias->{new}, $alias->{orig} );
        }
        for my $disambiguation ( @{ $parsed->{disambiguations} } ) {
            $self->db->disambiguation( $disambiguation );
        }
    }
}

sub text_for_disambiguation {
    my ($abstract) = @_;
    return $abstract;
}

sub parse {
    my ( $self ) = @_;

    my @pages = @{$self->indexer->build_indices};
    foreach my $page (@pages) {
        $self->parse_page($self->indexer, $page);
    }

    $self->db->build_output;

}

1;
