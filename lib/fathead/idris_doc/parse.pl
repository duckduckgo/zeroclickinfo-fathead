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
my %module_clash;
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
            $module_clash{$module} = 1;
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

has disambiguations => (
    is => 'ro',
    default => sub { {} },
);

sub disambiguation {
    my ($self, $disambiguation) = @_;
    $self->disambiguations->{$disambiguation->{title}} = $disambiguation;
}

sub insert_disambiguation {
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
    my ($old_package, $module) = $article->{url} =~ /\/(\w+)\/docs\/(.+)\.html/;
    $links{$article->{url}} = $article->{title};
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

sub retrieve_entry {
    my ($self, $title) = @_;
    return $self->articles->{$title} // $self->disambiguations->{$title};
}

#######################################################################
#                               Helpers                               #
#######################################################################

sub make_aliases {
    my ($title, @aliases) = @_;
    my @valid_aliases = uniq grep { $_ ne $title } @aliases;
    map { { new => $_, orig => $title } } @valid_aliases;
}

# r: Data.Vect.Vect, Vect.Vect, Vect
# l: Data.Vect.Vect, Data.Vect, Data
sub alias_components {
    my ($name, $dir) = @_;
    my @names;
    if ($dir eq 'r') {
        my @comps = reverse(split /\./, $name);
        foreach my $c (0..$#comps) {
            push @names, (join '.', reverse(@comps[0..$c]));
        }
    } elsif ($dir eq 'l') {
        my @comps = (split /\./, $name);
        foreach my $c (0..$#comps) {
            push @names, (join '.', (@comps[0..$c]));
        }
    }
    return @names;
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
    # TODO: When the FatHead parser is fixed, allow links in the types (they
    # are useful!)
    $dom->find('.signature > a')->map(sub { $_->attr(href => '') });
    $dom->find('.signature > a')->map(tag => 'span');
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

sub normalize_fixities {
    my ($dom) = @_;
    $dom->find('dt.fixity')->map('remove');
    $dom->find('dd.fixity')->map(sub {
        $_->tag('p');
        $_->wrap_content('<i></i>');
    });
    $dom->find('dd > dl > .fixity')->map(sub {
        $_->parent->strip;
    });
}

sub dom_for_parsing {
    my ($url, $page) = @_;
    my $dom = dom_for_file($page);
    normalize_dom_links($url, $dom);
    normalize_fixities($dom);
    $dom->find('strong')->map('strip');
    return $dom;
}

sub display_description {
    my ($desc) = @_;
    return '' unless $desc;
    $desc->find('br')->map('remove');
    my $text = '';
    if (my $ps = $desc->children('p')) {
        $text .= $ps->join;
    }
    return $text;
}

sub display_type {
    my ($type) = @_;
    return '' unless $type && $type->children->each;
    return '<pre>' . $type->to_string . '</pre>';
}

sub display_decls {
    my ($decls) = @_;
    return '' unless $decls;
    my $text = '';
    foreach my $dt ($decls->children('dt')->each) {
        $text .= display_function($dt, $dt->next);
    }
    return $text;
}

sub display_function {
    my ($ft, $fd) = @_;
    my $signature = display_type($ft);
    my $description = display_description($fd);
    return "$signature$description";
}

sub display_heading {
    my ($heading, $text) = @_;
    return '' unless $text ne '';
    return "<b>$heading:</b>\\n$text";
}

sub display_decls_type {
    my ($dd, $t) = @_;
    return display_decls(
        $dd->children('dl.decls')->grep(at => ".name.$t")->first
    );
}

# Data Types, Interfaces etc. (sub-functions)
sub display_rich {
    my ($headers) = @_;
    return sub {
        my ($dt, $dd) = @_;
        my $text = '';
        $text .= display_function($dt, $dd);
        foreach my $header (@$headers) {
            my ($head, $type) = @$header;
            $text .= display_heading($head, display_decls_type($dd, $type));
        }
        return $text;
    };
}

sub inject_package {
    my ($title, $package) = @_;
    $title =~ / \(.+\)$/
        ? $title =~ s/ \((.+)\)$/ ($1 - $package)/r
        : "$title ($package)";
}

my %type_names = (
    data      => 'Data Type',
    interface => 'Interface',
    record    => 'Record',
    class     => 'Interface',
);

sub get_category_rich {
    my ($c) = @_;
    my ($w) = $c->at('.word')->text =~ /(\w+)/;
    return "$c->{id} ($type_names{$w})";
}

my @parsers;

sub parser {
    my %options = (
        aliases => sub { () },
        description => \&display_function,
        name_type => '',
        @_,
    );
    push @parsers, sub {
        my ($self, $dom, $meta) = @_;
        my @entities = $options{decls}->($dom);
        my @articles;
        my @aliases;
        my $should_inject = $module_clash{$meta->{module}};
        foreach my $decl (@entities) {
            my @categories = ($meta->{full_module});
            my $title_no_p = $decl->attr('id');
            my $title = $title_no_p;
            $title .= " ($options{name_type})" if $options{name_type};
            $title = inject_package($title, $meta->{package})
                if $should_inject;
            my $anchor = $decl->attr('id');
            my $desc = $decl->next;
            @aliases = (@aliases, make_aliases($title,
                $title_no_p, $options{aliases}->($decl, $title_no_p),
                alias_components($title_no_p, 'r'),
            ));
            if (my $d = $decl->parent->parent->previous) {
                if ($d->matches('dt[id]')) {
                    my $cat = get_category_rich($d);
                    $cat = inject_package($cat, $meta->{package})
                        if $should_inject;
                    push @categories, $cat;
                }
            }
            my $text = $options{description}->($decl, $desc);
            my $article = {
                text => $text,
                title => $title,
                anchor => $anchor,
                categories => \@categories,
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
        return ($match ? $items->grep(sub { $_->text =~ qr/$match/ }) : $items)
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

parser(
    name        => 'Data Types',
    decls       => decls_default(['.word'], 'data'),
    description => display_rich([
        ['Constructors', 'constructor'],
    ]),
    name_type   => 'Data Type',
    aliases     => aliases_default(['.name.type']),
);

# NOTE: The 'class' keyword is deprecated
parser(
    name        => 'Interfaces',
    decls       => decls_default(['.word'], 'interface|class'),
    description => display_rich([
        ['Methods', 'function'],
    ]),
    name_type   => 'Interface',
    aliases     => aliases_default(['.name.type']),
);

parser(
    name    => 'Functions',
    decls   => decls_default(['.name.function']),
    aliases => aliases_default(['.name.function']),
);

parser(
    name      => 'Constructors',
    decls     => decls_default(  ['.name.constructor']),
    name_type => 'Constructor',
    aliases   => aliases_default(['.name.constructor']),
);

parser(
    name        => 'Records',
    decls       => decls_default(['.word'], 'record'),
    description => display_rich([
        ['Constructor', 'constructor'], ['Fields', 'function']
    ]),
    name_type   => 'Record',
    aliases     => aliases_default(['.name.type']),
);

sub parse_page {
    my ( $self, $page ) = @_;
    my $fullpath = $self->doc_fullpath( $page->{path} );
    my $url = $self->doc_fullurl($page->{sub});
    my @articles;
    my $meta = {
        module => path($fullpath)->basename =~ s/\.html$//r,
        package => path($fullpath)->parent->parent->basename
            =~ s/_doc$//r,
    };
    my $full = "$meta->{module} (module)";
    $full = inject_package($full, $meta->{package})
        if $module_clash{$meta->{module}};
    $meta->{full_module} = $full;
    foreach my $parser (@parsers) {
        my $parsed = $parser->($self, dom_for_parsing($url, $fullpath), $meta);
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
    my @module_aliases = make_aliases($full,
        alias_components($meta->{module}, 'l'),
        alias_components($meta->{module}, 'r'),
    );
    for my $alias (@module_aliases) {
        $self->alias( $alias->{new}, $alias->{orig} );
    }
    $self->alias($meta->{module}, $full);
    $self->disambiguation({
        title => $full,
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
    return '' unless defined $abstract;
    my $abs = Mojo::DOM->new->parse($abstract);
    if (my $p = $abs->at('pre:first-of-type + p:first-of-type')) {
        return $p->all_text unless $p->children('i')->first;
    }
    if (my $t = $abs->at('pre:first-of-type')) {
        if (my $sig = $t->at('.signature')) {
            my $sigtext = $sig->all_text;
            return $sig->all_text if $sig->children->first;
        }
        return $t->all_text;
    }
    return '';
}

sub resolve_aliases {
    my ($self) = @_;
    my %aliases = %{$self->aliases};
    while (my ($alias, $to) = each %aliases) {
        my @to = @$to;
        @to == 1 and $self->insert_alias($alias, $to[0]) and next;
        my @articles = map { $self->retrieve_entry($_) } @to;
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

sub resolve_disambiguations {
    my ($self) = @_;
    foreach my $d (values %{$self->disambiguations}) {
        $self->insert_disambiguation($d);
    }
}

sub parse {
    my ( $self ) = @_;
    foreach my $page (sort @{$self->pages}) {
        $self->parse_page($page);
    }

    $self->resolve_articles;
    $self->resolve_aliases;
    $self->resolve_disambiguations;
}

main->new->parse;
