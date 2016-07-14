#!/usr/bin/env perl

use strict;
use warnings;

binmode STDOUT, ":utf8";

use IO::All;
use Mojo::DOM;
use Cwd qw( getcwd );
use Util qw( get_row trim_abstract);
use Moo;
use File::Spec;
use DBI;
use Text::CSV_XS;

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
    File::Spec->catdir( $_[0]->working_dir, qw/ .. download perldoc-html / );
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

has other_pages => ( is => 'lazy' );
sub _build_other_pages {
    my ( $self ) = @_;
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

sub links_from_index {
    my ( $self, $index ) = @_;
    my $path = $self->doc_fullpath( $index );
    return unless ( -f $path );
    my $links;

    my $html < io( $path );
    my $dom = Mojo::DOM->new( $html );

    my $content = $dom->find('ul')->[4];

    for my $link ( @{ $content->find('a')->to_array } ) {
        my $name     = $link->content;
        my $filename = $link->attr('href');
        my $basename = $filename =~ s/\.html$//r;

        $links->{ $name }->{ basename } = $basename;
        $links->{ $name }->{ filename } = $filename;
    }

    return $links;
}

sub insert {
    my ( $self, $data ) = @_;
    my @keys = keys $data;
    my $sql = sprintf( "INSERT INTO output.txt (%s) VALUES (%s)",
        join( ", ", @keys ),
        join( ", ", map { '?' } @keys ),
    );
    $self->tsv->do( $sql, undef, @{ $data }{ @keys } );
}

sub alias {
    my ( $self, $new, $orig ) = @_;
    $self->insert({
        title => $new,
        type  => 'R',
        alias => $orig,
    });
}

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

sub test_csv {
    my ( $self ) = @_;
    $self->insert({ title => 'asd', sourceurl => 'bsd' });
}

main->new->test_csv;

