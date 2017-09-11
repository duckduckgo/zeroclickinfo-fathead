package Util::Parser;

use strict;
use warnings;

binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";

use Mojo::DOM;
use Moo;
use URI;

use Util::DB;

my %links;

has indexers => (
    is       => 'ro',
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

#######################################################################
#                       Normalize Parse Results                       #
#######################################################################

sub parse_page {
    my ( $self, $index, $page ) = @_;
    my @parsed;
    foreach my $parser (@{$index->parsers_for($page)}) {
        push @parsed, $parser->($page->dom);
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

    foreach my $indexer (@{$self->indexers}) {
        foreach my $page (@{$indexer->pages}) {
            $self->parse_page($indexer, $page);
        }
    }

    $self->db->build_output;

}

1;
