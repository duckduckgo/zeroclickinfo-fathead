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

sub insert {
    my ( $self, $data ) = @_;
    my @keys = keys $data;
    my $sql = sprintf( "INSERT INTO output.txt (%s) VALUES (%s)",
        join( ", ", @keys ),
        join( ", ", map { '?' } @keys ),
    );
    $self->tsv->do( $sql, undef, @{ $data }{ @keys } );
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

