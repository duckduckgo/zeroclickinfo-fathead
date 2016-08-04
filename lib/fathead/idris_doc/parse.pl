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

my %module_packages;
sub _build_pages {
    my ($self) = @_;
    my @pages = File::Find::Rule->file->name('*.html')->in($base);
    my %modules;
    my @valid_pages;
    foreach my $page (@pages) {
        my $module = path($page)->basename =~ s/\.html$//r;
        my $package = path($page)->parent->parent->basename;
        if (my $existing = $modules{$module}) {
            if (path($existing)->slurp eq path($page)->slurp) {
                my @existing = @{$module_packages{$module} // []};
                $module_packages{$module} = [@existing, $package];
                next;
            }
        }
        push @valid_pages, $page;
        $modules{$module} = $page;
        my @existing = @{$module_packages{$module} // []};
        $module_packages{$module} = [@existing, $package];
    }
    return [map { { path => $_, sub => $_ =~ s/\Q$base\E//r } } @valid_pages];
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
    $new = lc $new;
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

sub make_links {
    my ($self, $article) = @_;
    my ($module) = path($article->{url})->basename =~ /^(.+)\.html/;
    my $old_package = path($article->{url})->parent->parent->basename;
    foreach my $package (@{$module_packages{$module} // []}) {
        my $url = $article->{url} =~ s/$old_package/$package/r;
        $links{$url} = $article->{title};
    }
}

sub article {
    my ($self, $article) = @_;
    my $title = $article->{title};
    warn "Duplicate article with title '$title' detected\n" and return
        if exists $self->articles->{$title};
    $self->make_links($article);
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

sub normalize_dom_links {
    my ($url, $dom) = @_;
    $dom->find('a')->map(sub {
        my $link = $_[0]->attr('href') or return;
        $_[0]->attr(href => URI->new_abs($link, $url)->as_string);
    });
}

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
    normalize_dom_links($url, $dom);
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

sub display_description {
    my ($desc) = @_;
    return '' unless $desc;
    $desc->find('br')->map('remove');
    if (my $fix = $desc->find('.fixity')->first) {
        $fix->parent->strip;
        $fix->remove;
    }
    if (my $fixy = $desc->at('.fixity')) {
        $fixy->tag('i');
        $fixy->prepend_content('Fixity: ');
    }
    if (my $dl = $desc->find('dl')->last) {
        $dl->remove;
    }
    $desc->to_string;
}

sub display_type {
    my ($type) = @_;
    return '' unless $type && $type->children->each;
    return '<pre><code>' . $type->to_string . '</code></pre>';
}

sub display_implements {
    my ($type, $desc) = @_;
    my $text = '';
    foreach my $con ($desc->find("dt > .name.$type")->map('parent')->each) {
        $text .= display_type($con);
        $text .= display_description($con->next);
    }
    if (my $d = $desc->at("dt > .name.$type")) {
        $d->parent->remove;
    }
    return $text;
}

sub display_mother {
    my ($ctype, $cname, $decl, $desc) = @_;
    my $ma = display_ma(@_);
    display_header($decl, $desc) . $ma;
}

sub display_ma {
    my ($ctype, $cname, $decl, $desc) = @_;
    my $c_text = display_implements($ctype, $desc);
    return "<b>$cname:</b>\\n$c_text";
}

sub display_header {
    my ($decl, $desc) = @_;
    my $type = $decl->at('.signature');
    my $tt = display_type($type);
    my $dt = display_description($desc);
    return "$tt$dt";
}

sub display_interface {
    display_mother('function', 'Methods', @_);
}

sub display_datatype {
    display_mother('constructor', 'Constructors', @_);
}

sub display_record {
    my $text = display_ma('function', 'Fields', @_);
    display_mother('constructor', 'Constructor', @_) . $text;
}

sub parser {
    my %options = (
        aliases => sub { () },
        description => \&display_header,
        name_type => '',
        @_,
    );
    return sub {
        my ($self, $dom) = @_;
        my @entities = $options{decls}->($dom);
        my @articles;
        my @aliases;
        foreach my $decl (@entities) {
            my $title_no_p = $decl->attr('id');
            my $title = $title_no_p;
            $title .= " ($options{name_type})" if $options{name_type};
            my $anchor = $decl->attr('id');
            my $desc = $decl->next;
            @aliases = (@aliases, make_aliases($title,
                $title_no_p, $options{aliases}->($decl, $title_no_p),
            ));
            my $text = $options{description}->($decl, $desc);
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

sub decls_default {
    my ($look, $match) = @_;
    return sub {
        my ($dom) = @_;
        my $items = $dom->find(
            join ', ', map { ".decls > dt[id] > span$_" } @$look
        );
        return ($match ? $items->grep(qr/$match/) : $items)
            ->map('parent')->each;
    };
}

sub aliases_default {
    my ($look) = @_;
    return sub {
        my $short = $_[0]->find(join ', ', @$look)->first->text;
        my @aliases = ($short);
        if ($short =~ /^\((.+)\)$/) {
            push @aliases, $1;
        }
        return @aliases;
    };
}

sub parse_data {
    parser(
        decls       => decls_default(['.word'], 'data'),
        description => \&display_datatype,
        name_type   => 'Data Type',
        aliases     => aliases_default(['.name.type']),
    )->(@_);
}

sub parse_interfaces {
    parser(
        decls       => decls_default(['.word'], 'interface'),
        description => \&display_interface,
        name_type   => 'Interface',
        aliases     => aliases_default(['.name.type']),
    )->(@_);
}

sub parse_functions {
    parser(
        decls   => decls_default(  ['.name.function', '.name.constructor']),
        aliases => aliases_default(['.name.function', '.name.constructor']),
    )->(@_);
}

sub parse_records {
    parser(
        decls => decls_default(['.word'], 'record'),
        description => \&display_record,
        name_type => 'Record',
        aliases => aliases_default(['.name.type']),
    )->(@_);
}

sub parse_page {
    my ( $self, $page ) = @_;
    my $fullpath = $self->doc_fullpath( $page->{path} );
    my $url = $self->doc_fullurl($page->{sub});
    my @parsers = qw(parse_functions parse_data parse_interfaces parse_records);
    my @articles;
    foreach my $parser (@parsers) {
        my $parsed = $self->$parser(dom_for_parsing($url, $fullpath));
        $parsed = normalize_parse_result($parsed);
        for my $article ( @{ $parsed->{articles} } ) {
            push @articles, $article;
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
    $self->disambiguation({
        title => path($page->{path})->basename =~ s/\.html//r,
        disambiguations => [
            map { {
                link => $_->{title},
                description => text_for_disambiguation($_->{text}),
            } } @articles,
        ],
    });
}

sub text_for_disambiguation {
    my ($abstract) = @_;
    if (my $short = Mojo::DOM->new->parse($abstract)->at('code,p')) {
        $short->all_text;
    } else {
        '';
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
            disambiguations => [
                map { {
                    link => $_->{title},
                    description => text_for_disambiguation($_->{text}),
                } } @articles
            ],
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
