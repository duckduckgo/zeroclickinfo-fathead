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

my %links;

has perldoc_url => ( is => 'lazy' );
sub _build_perldoc_url {
    'http://perldoc.perl.org/';
}

has indexer => (
    is       => 'ro',
    isa      => sub { die 'Not a Util::Index' unless ref $_[0] eq 'Util::Index' },
    required => 1,
    doc      => 'Used to generate indices for parsing',
);

has aliases => (
    is => 'ro',
    builder => 1,
);

sub _build_aliases {
    my $csv = Text::CSV_XS->new( { binary => 1, allow_whitespace => 1 });
    open my $fh, "<", "aliases.txt"
        or (warn "No external aliases file detected" and return {});
    my %aliases;
    while (my $row = $csv->getline($fh)) {
        my ($alias, @orig) = @$row;
        $aliases{$alias} = [@orig];
    }
    close $fh;
    return \%aliases;
}

has force_redirect => (
    is => 'ro',
    builder => 1,
);

sub _build_force_redirect {
    my $csv = Text::CSV_XS->new({
            binary => 1,
            sep => '>',
            allow_whitespace => 1
    });
    open my $fh, "<", "redirect.txt"
        or (warn "No external redirect file detected" and return {});
    my %force_redirects;
    while (my $row = $csv->getline($fh)) {
        my ($from, $to, undef) = @$row;
        $force_redirects{$from} = $to;
    }
    close $fh;
    return \%force_redirects;
}

has related => ( is => 'lazy' );
sub _build_related {
    # As with the ad-hoc aliases, this should be moved to an external source.
    [
        [ 'For Loops', 'Foreach Loops', 'Loop Control', 'Compound Statements', 'Statement Modifiers' ],
    ]
}

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

has output_txt => ( is => 'lazy' );
sub _build_output_txt {
    open my $fh, '>:encoding(UTF-8)', 'output.txt';
    return $fh;
};

sub dom_for_file { Mojo::DOM->new( io($_[0])->all ); }

# Parsers for the 'index-*' keys will run on *all* files produced from parsing
# links in the index files.
# Parsers for other keys (basenames) will only run on the matching file.
my %parser_map = (
    'index-faq'       => ['parse_faq'],
    'index-functions' => ['parse_functions'],
    'index-module'    => ['get_synopsis'],
    'index-default'   => ['get_anchors'],
    'perldiag'        => ['parse_diag_messages'],
    'perlglossary'    => ['parse_glossary_definitions'],
    'perlop'          => ['parse_operators'],
    'perlpod'         => ['parse_pod_formatting_codes'],
    'perlpodspec'     => ['parse_pod_commands'],
    'perlre'          => [
        'parse_regex_modifiers',
    ],
    'perlrun'         => ['parse_cli_switches'],
    'perlvar'         => ['parse_variables'],
);

my @parsers = sort keys %parser_map;

sub get_parsers {
    my ($index, $basename) = @_;
    my $index_parsers = $parser_map{$index};
    my $basename_parsers = $parser_map{$basename};
    return (@{$index_parsers || []}, @{$basename_parsers || []});
}

my %link_parser_for_index = (
    'functions' => 'parse_index_functions_links',
    'default'   => 'parse_index_links',
);

sub link_parser_for_index {
    my $index = shift;
    $index =~ s/index-//;
    return $link_parser_for_index{$index} // $link_parser_for_index{default};
}

sub parse_index_links {
    my ($self, $dom) = @_;
    my $content = $dom->find('ul')->[4];
    return @{$content->find('a')->to_array};
}

sub normalize_dom_links {
    my ($url, $dom)  = @_;
    $dom->find('a')->map(sub {
        my $link = $_[0]->attr('href') or return;
        $_[0]->attr(href => URI->new_abs($link, $url)->as_string);
    });
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
        # TODO: Remove the escaping when the DB is fixed.
        disambiguation => $dtext =~ s{\\}{\\\\}gr,
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
    $links{$article->{url}} = $title;
    $self->articles->{$title} = $article;
}

sub entry {
    my ( $self, %article ) = @_;
    my ($title, $text, $url, $related) = @article{qw(title text url related)};
    $text =~ s{\\}{\\\\}g;
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

sub without_punct {
    $_[0] =~ s/\p{Punct}//gr;
}

sub make_aliases {
    my ($title, @aliases) = @_;
    my @valid_aliases = grep { $_ ne $title } @aliases;
    map { { new => $_, orig => $title } } @valid_aliases;
}

my $default_text_selector = 'p, pre';

# Produce the 'abstract' text content from the given Mojo::DOM spec.
sub text_from_selector {
    my ($dom, $spec) = @_;
    $spec //= $default_text_selector;
    return $dom->children($spec)->join();
}

sub ul_list_parser {
    my %options = (
        link => sub { $_[0]->find('a')->first->{name} },
        text => sub { text_from_selector($_[0]) },
        aliases => sub { () },
        uls => [],
        is_empty => sub { !($_[0]->find('p')->each) },
        force_redirect => sub { undef },
        disambiguation => sub { undef },
        related => sub { [] },
        categories => sub { [] },
        @_,
    );
    return sub {
        my ($self, $dom) = @_;
        my (@articles, @aliases, @uls, @disambiguations);
        if (my $s = $options{selector_main}) {
            @uls = ($dom->at($s)->following('ul')->first);
        } elsif (ref $options{uls} eq 'CODE') {
            @uls = $options{uls}->($dom);
        } else {
            @uls = @{$options{uls}};
        }
        foreach my $ul (@uls) {
            my @lis = $ul->children('li')->each;
            my @col = collate_li($options{is_empty}, @lis);
            foreach my $lit (@col) {
                my @items = @$lit;
                my $item = $items[$#items];

                my $link = $options{link}->($item);
                my $title = $options{title}->($item);
                my $text = $options{text}->($item);
                my @secondary_titles = map { $options{title}->($_) }
                    @items[0..$#items-1];
                my @titles = ($title, @secondary_titles);
                @aliases = (@aliases,
                    make_aliases($title, @secondary_titles),
                );
                foreach my $subt (@titles) {
                    @aliases = (@aliases,
                        make_aliases(
                            $title,
                            $options{aliases}->($item, $subt)
                        ),
                    );
                }
                my $article = {
                    title  => $title,
                    anchor => $link,
                    text   => $text,
                };
                my $categories = $options{categories}->($item, $article);
                $article->{categories} = $categories;
                my $related = $options{related}->($item, $article);
                $article->{related} = $related;
                if (my $disambiguation = $options{disambiguation}->($item, $article)) {
                    push @disambiguations, $disambiguation;
                    next;
                }
                if (my $redir = $options{force_redirect}->($item, $article)) {
                    @aliases = (@aliases, make_aliases($redir, $title));
                    next;
                }
                push @articles, $article;
            }
        }
        return {
            articles => \@articles,
            aliases  => \@aliases,
            disambiguations => \@disambiguations,
        };
    }
}

# If you have:
# - a
# - b
# - c
#   description for all
# Then use this to produce a list [a, b, c]
# (From a list of @li, this will produce a list of the above form for
# each group).
sub collate_li {
    my ($is_empty, @lis) = @_;
    my @res;
    my @r;
    foreach my $li (@lis) {
        push @r, $li;
        next if $is_empty->($li);
        push @res, [@r];
        @r = ();
    }
    return @res;
}

#######################################################################
#                       Normalize Parse Results                       #
#######################################################################

sub normalize_article {
    my ($article) = @_;
    my $text = $article->{text};
    $text =~ s/\n/ /g;
    # Okay, the parser *really* hates links...
    my $dom = Mojo::DOM->new->parse($text);
    $dom->find('a')->map(tag => 'span');
    $text = $dom->to_string;
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
    return $dom;
}

sub parse_page {
    my ( $self, $page ) = @_;
    my $fullpath = $page->{full_path};
    my $url = $page->{full_url};
    my $parser = $page->{parser};
    my @parsed;
    foreach my $parser (@{$page->{parsers}}) {
        push @parsed, $self->$parser(dom_for_parsing($url, $fullpath));
    }
    foreach my $parsed (@parsed) {
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

sub text_for_disambiguation {
    my ($abstract) = @_;
    return $abstract;
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
        if (my $force_redirect = $self->force_redirect->{$article->{title}}) {
            $self->alias($article->{title}, $force_redirect);
            next;
        }
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

    my %indices = %{$self->indexer->build_indices};
    foreach my $index ( sort keys %indices ) {
        foreach my $page ( sort keys %{$indices{ $index }} ) {
            $self->parse_page( $indices{ $index }{ $page } );
        }
    }

    $self->resolve_articles;
    $self->resolve_aliases;
    $self->resolve_disambiguations;
}

1;
