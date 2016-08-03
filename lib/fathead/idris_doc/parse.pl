#!/usr/bin/env perl

use strict;
use warnings;

binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";

use Cwd qw( getcwd );
use DBI;
use File::Spec;
use IO::All;
use Mojo::DOM;
use Moo;
use Text::CSV_XS;
use URI;
use List::MoreUtils qw(uniq);
use File::Find::Rule;
use Path::Tiny;

my %links;

has idris_doc_url => ( is => 'lazy' );
sub _build_idris_doc_url {
    'http://www.idris-lang.org/docs/current/';
}

has working_dir => ( is => 'lazy' );
sub _build_working_dir {
    getcwd;
}

has docs_dir => ( is => 'lazy' );
sub _build_docs_dir {
    return $_[0]->working_dir;
}

has pages => (
    is => 'ro',
    builder => 1,
);

my $base = 'download/docs/current/';

sub _build_pages {
    my ($self) = @_;
    my @pages = File::Find::Rule->file->name('*.html')->in($base);
    return [map { { path => $_, sub => $_ =~ s/\Q$base\E//r } } @pages];
}

has aliases => (
    is => 'ro',
    default => sub { {} },
);

has tsv => ( is => 'lazy' );
sub _build_tsv {
    my $dbh = DBI->connect ("dbi:CSV:", undef, undef, {
        f_encoding       => "UTF-8",
        csv_sep_char     => "\t",
        csv_class        => "Text::CSV_XS",
        csv_quote_char   => '',
        csv_escape_char  => '',
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
    File::Spec->catfile( $self->docs_dir, @parts );
}

sub doc_fullurl {
    my ( $self, $part ) = @_;
    URI->new(
        sprintf( '%s%s', $self->idris_doc_url, $part )
    )->canonical
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

sub select {
    my ($self, $what, $matching) = @_;
    my $sql = "SELECT * FROM output.txt WHERE $what = ?";
    return $self->tsv->selectrow_hashref($sql, undef, $matching);
}

sub alias {
    my ( $self, $new, $orig ) = @_;
    my @existing = @{ $self->aliases->{$new} // [] };
    $self->aliases->{$new} = [@existing, $orig];
}

sub insert_alias {
    my ($self, $new, $orig) = @_;
    $self->insert({
        title => $new,
        type  => 'R',
        alias => $orig,
    });
}

sub disambiguation {
    my ($self, $disambiguation) = @_;
    my @disambiguations = map {
        "*[[$_->{link}]], $_->{description}.";
    } @{ $disambiguation->{disambiguations} };
    my $dtext = join '\n', @disambiguations;
    $self->insert({
        type => 'D',
        title => $disambiguation->{title},
        disambiguation => $dtext,
    });
}

has articles => (
    is => 'ro',
    default => sub { {} },
);

sub article {
    my ($self, $article) = @_;
    my $title = $article->{title};
    warn "Duplicate article with title '$title' detected\n" and return
        if exists $self->articles->{$title};
    $links{path($article->{url})->basename} = $title;
    $self->articles->{$title} = $article;
}

sub entry {
    my ( $self, %article ) = @_;
    my ($title, $text, $url, $related) = @article{qw(title text url related)};
    my $related_text = '';
    # TODO: Find out how the related links should *actually* be formatted
    if (defined $related && @$related) {
        $related_text = join '', map { "[[$_]]" } @$related;
    }
    my $category_text = join '\n', @{$article{categories} || []};
    return warn "No text for '$title'" unless $text;
    $self->insert({
        abstract => $text,
        categories => $category_text,
        title => $title,
        type  => 'A',
        related  => $related_text,
        sourceurl => $url,
    });
}

#######################################################################
#                               Helpers                               #
#######################################################################

sub make_aliases {
    my ($title, @aliases) = @_;
    my @valid_aliases = grep { $_ ne $title } @aliases;
    map { { new => $_, orig => $title } } @valid_aliases;
}

#######################################################################
#                       Normalize Parse Results                       #
#######################################################################

sub normalize_article {
    my ($article) = @_;
    my $text = $article->{text};
    $text =~ s/\n/ /g;
    return {
        %$article,
        text => $text,
    };
}

sub normalize_parse_result {
    my ($parsed) = @_;
    $parsed->{articles} = [
        map { normalize_article($_) } (@{$parsed->{articles}})
    ];
    return $parsed;
}

sub dom_for_parsing {
    my ($url, $page) = @_;
    my $dom = dom_for_file($page);
    $dom->find('strong')->map('strip');
    $dom->find('code > a')->grep(sub { $_->parent->all_text eq $_->text })
        ->map( sub { $_->parent->strip });
    return $dom;
}

sub build_abstract {
    my %info = @_;
    my ($type, $desc) = @info{qw(type description)};
    return "$type$desc";
}

sub display_datatype {
    my ($desc) = @_;
    my $initial = $desc->find('p')->first;
    $initial = $initial ? $initial->to_string : '';
    my $c_text = display_constructors($desc);
    return "${initial}Constructors:\\n$c_text";
}

sub display_constructors {
    my ($desc) = @_;
    my $text = '';
    foreach my $con ($desc->find('dt > .name.constructor')->map('parent')->each) {
        $text .= display_constructor($con);
        $text .= $con->next->all_text;
    }
    return $text;
}

sub display_type {
    my ($type) = @_;
    return $type->to_string;
}

sub display_constructor {
    my ($cons) = @_;
    return '<pre><code>' . display_type($cons) . '</code></pre>';
}

sub parser {
    my %options = (
        aliases => sub { () },
        description => sub { $_[0]->to_string },
        @_,
    );
    return sub {
        my ($self, $dom) = @_;
        my @entities = $options{decls}->($dom);
        my @articles;
        my @aliases;
        foreach my $decl (@entities) {
            my $title = $decl->attr('id');
            my $anchor = $decl->attr('id');
            my $desc = $decl->next;
            next unless $desc->tag eq 'dd';
            my $type = display_type($decl->at('.signature'));
            $type = "<pre><code>$type</code></pre>";
            @aliases = (@aliases, make_aliases($title,
                $options{aliases}->($decl, $title),
            ));
            my $text = build_abstract(
                name => $title,
                description => $options{description}->($desc),
                type => $type,
            );
            my $article = {
                text => $text,
                title => $title,
                anchor => $anchor,
            };
            push @articles, $article;
        }
        return {
            articles => \@articles,
            aliases => \@aliases,
        };
    };
}

sub parse_data {
    parser(
        decls => sub { $_[0]->find('.decls > dt[id] > span.word')
            ->grep(qr/data/)->map('parent')->each },
        description => \&display_datatype,
        aliases => sub { ($_[0]->find('.name.type')->first->text) },
    )->(@_);
}

sub parse_functions {
    parser(
        decls => sub { $_[0]->find('.decls > dt[id] > span.name.function')
            ->map('parent')->each },
        aliases => sub { ($_[0]->find('.name.function')->first->text) },
    )->(@_);
}

sub parse_page {
    my ( $self, $page ) = @_;
    my $fullpath = $self->doc_fullpath( $page->{path} );
    my $url = $self->doc_fullurl($page->{sub});
    my @parsers = qw(parse_functions parse_data);
    foreach my $parser (@parsers) {
        my $parsed = $self->$parser(dom_for_parsing($url, $fullpath));
        $parsed = normalize_parse_result($parsed);
        for my $article ( @{ $parsed->{articles} } ) {
            my $anchored_url = $url;
            $anchored_url .= "#" . $article->{anchor} if $article->{anchor};

            $article->{url} = $anchored_url;
            $self->article($article);
        }

        for my $alias ( @{ $parsed->{aliases} } ) {
            $self->alias( $alias->{new}, $alias->{orig} );
        }
        for my $disambiguation ( @{ $parsed->{disambiguations} } ) {
            $self->disambiguation( $disambiguation );
        }
    }
}

sub resolve_aliases {
    my ($self) = @_;
    my %aliases = %{$self->aliases};
    while (my ($alias, $to) = each %aliases) {
        my @to = @$to;
        @to == 1 and $self->insert_alias($alias, $to[0]) and next;
        my @articles = map { $self->articles->{$_} } @to;
        scalar (uniq map { $_->{title} } @articles ) == 1
            and $self->insert_alias($alias, $to[0]) and next;
        $self->disambiguation({
            title => $alias,
            disambiguations => [map {
                { link => $_->{title}, description => Mojo::DOM->new->parse(
                        $_->{text})->at('code')->all_text,
                },
            } @articles],
        });
    }
}

sub resolve_articles {
    my ($self) = @_;
    my %articles = %{$self->articles};
    foreach my $article (values %articles) {
        my $dom = Mojo::DOM->new->parse($article->{text});
        $dom->find('a[href]')->map(sub {
            my $link = $_->attr('href');
            if (my $point = $links{$link}) {
                $_->attr(href => "/?q=$point&ia=about");
            }
        });
        $article->{text} = $dom->to_string;
        $self->entry(%$article);
    }
}

sub parse {
    my ( $self ) = @_;
    foreach my $page (sort @{$self->pages}) {
        $self->parse_page($page);
    }

    $self->resolve_articles;
    $self->resolve_aliases;
}

main->new->parse;
