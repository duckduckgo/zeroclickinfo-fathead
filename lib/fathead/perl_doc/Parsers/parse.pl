#!/usr/bin/env perl

use strict;
use warnings;

binmode STDOUT, ":utf8";

use Cwd qw( getcwd );
use DBI;
use File::Spec;
use IO::All;
use Mojo::DOM;
use Moo;
use Text::CSV_XS;
use URI;
use Util qw( get_row trim_abstract);
use List::Util qw(first);

has perldoc_url => ( is => 'lazy' );
sub _build_perldoc_url {
    'http://perldoc.perl.org/';
}

has working_dir => ( is => 'lazy' );
sub _build_working_dir {
    getcwd;
}

has docs_dir => ( is => 'lazy' );
sub _build_docs_dir {
    File::Spec->catdir( $_[0]->working_dir, qw/ .. download / );
}

has indices => ( is => 'lazy' );
sub _build_indices {
    my ( $self ) = @_;
    my $indices;
    my @index_pages = ( qw/
       index-tutorials
       index-faq
       index-language
       index-functions
       index-pragmas
       index-utilities
       index-internals
       index-platforms
    / );
    push @index_pages, map { "index-modules-$_" } grep { $_ !~ /[JKQRVWYZ]/ } 'A'..'Z';

    for ( @index_pages ) {
        $indices->{$_} = $self->links_from_index( $_ );
    }
    return $indices;
}

has tsv => ( is => 'lazy' );
sub _build_tsv {
    my $dbh = DBI->connect ("dbi:CSV:", undef, undef, {
        csv_sep_char     => "\t",
        csv_class        => "Text::CSV_XS",
    });

    $dbh->do ( sprintf ( "CREATE TABLE output.txt (%s)",
        join ( ', ', map { "$_ CHAR" } ( qw/
            title type alias null1 categories null2 related null3
            links disambiguation image abstract sourceurl
        / ) )
    ) );

    return $dbh;
}

sub dom_for_file { Mojo::DOM->new( io($_[0])->all ); }

sub doc_fullpath {
    my ( $self, @parts ) = @_;
    $parts[-1] = $parts[-1] . '.html';
    File::Spec->catfile( $self->docs_dir, @parts );
}

sub doc_fullurl {
    my ( $self, $part ) = @_;
    URI->new(
        sprintf( '%s/%s', $self->perldoc_url, $part )
    )->canonical
}

my %parser_map = (
    faq     => 'parse_faq',
    module  => 'get_synopsis',
    default => 'get_anchors',
);

my @parsers = sort keys %parser_map;

sub get_parser {
    my $index = shift;
    my $parser = (first { $index =~ /$_/i } @parsers) // 'default';
    return $parser_map{$parser};
}

sub links_from_index {
    my ( $self, $index ) = @_;
    my $path = $self->doc_fullpath( $index );
    return unless ( -f $path );
    my $links;
    my $parser = get_parser($index);

    my $dom = dom_for_file( $path );

    my $content = $dom->find('ul')->[4];

    for my $link ( @{ $content->find('a')->to_array } ) {
        my $name     = $link->content;
        my $filename = $link->attr('href');
        my $basename = $filename =~ s/\.html$//r;

        $links->{ $name }->{ basename } = $basename;
        $links->{ $name }->{ filename } = $filename;
        $links->{ $name }->{ parser } = $parser;
    }

    return $links;
}

sub insert {
    my ( $self, $data ) = @_;
    my %data = %$data;
    my @keys = keys %data;
    my $sql = sprintf( "INSERT INTO output.txt (%s) VALUES (%s)",
        join( ", ", @keys ),
        join( ", ", map { '?' } @keys ),
    );
    my @values = map { $data{$_} } @keys;
    $self->tsv->do( $sql, undef, @values );
}

sub alias {
    my ( $self, $new, $orig ) = @_;
    $self->insert({
        title => $new,
        type  => 'R',
        alias => $orig,
    });
}

sub entry {
    my ( $self, $title, $text, $url ) = @_;
    $self->insert({
        title => $title,
        type  => 'A',
        abstract => $text,
        sourceurl => $url,
    });
}

sub get_synopsis {
    my ( $self, $dom ) = @_;
    my $articles;
    my $title = $dom->at('title')->text;
    my $module = $title =~ s/\s.*//r;
    my $first_code_block = $dom->find('pre')->[0];

    if ( !$first_code_block ) {
        warn "No code block for $module";
        return {};
    }

    my @articles = @{$articles->{articles} || []};
    push @articles, {
        title => $title,
        text  => $first_code_block->to_string
    };
    $articles->{articles} = \@articles;

    my @aliases = @{$articles->{aliases} || []};
    push @aliases, {
        new  => $module,
        orig => $title,
    };
    $articles->{aliases} = \@aliases;

    return $articles;
}

sub get_anchors {
    my ( $self, $dom ) = @_;
    my $articles;
}

#######################################################################
#                                FAQs                                 #
#######################################################################

sub get_faq_link {
    my ($title) = @_;
    my $a = $title->previous('a');
    return unless $a;
    return $a->attr('name');
}

sub build_description {
    my $question = shift;
    my $description;
    foreach my $para ($question->following('p')->each) {
        $description .= $para;
    }
    $description =~ s/\n/ /g;
    return $description;
}

sub parse_faq {
    my ($self, $dom) = @_;
    my %parsed;
    my @articles;
    foreach my $faq_title ($dom->find('h2')->each) {
        my $link = get_faq_link($faq_title);
        my $title = $faq_title->text;
        my $description = build_description($faq_title);
        next unless $link;
        push @articles, {
            anchor => $link,
            title  => $title,
            text => $description,
        };
    }
    $parsed{articles} = \@articles;
    return \%parsed;
}

sub parse_page {
    my ( $self, $page ) = @_;
    my $fullpath = $self->doc_fullpath( $page->{basename} );
    my $url = $self->doc_fullurl( $page->{filename} );
    my $parser = $page->{parser};
    my $parsed = $self->$parser( dom_for_file( $fullpath ) );

    for my $article ( @{ $parsed->{articles} } ) {
        my $anchored_url = $url;
        $anchored_url .= "#" . $article->{anchor} if $article->{anchor};

        $self->entry( $article->{title}, $article->{text}, $anchored_url );
    }

    for my $alias ( @{ $parsed->{aliases} } ) {
        $self->alias( $alias->{new}, $alias->{orig} );
    }
}

sub parse {
    my ( $self ) = @_;
    foreach my $index ( sort keys %{$self->indices} ) {
        foreach my $page ( sort keys %{$self->indices->{ $index }} ) {
            $self->parse_page( $self->indices->{ $index }->{ $page } );
        }
    }
}

main->new->parse;
