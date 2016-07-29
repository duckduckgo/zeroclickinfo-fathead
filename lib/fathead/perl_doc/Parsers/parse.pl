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
       index-overview
       index-functions
       index-pragmas
       index-utilities
       index-internals
       index-platforms
    / );

    for ( @index_pages ) {
        $indices->{$_} = $self->links_from_index( $_ );
    }
    return $indices;
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

sub dom_for_file { Mojo::DOM->new( io($_[0])->all ); }

sub doc_fullpath {
    my ( $self, @parts ) = @_;
    $parts[-1] = $parts[-1] . '.html';
    File::Spec->catfile( $self->docs_dir, @parts );
}

sub doc_fullurl {
    my ( $self, $part ) = @_;
    URI->new(
        sprintf( '%s%s', $self->perldoc_url, $part )
    )->canonical
}

# Parsers for the 'index-*' keys will run on *all* files produced from parsing
# links in the index files.
# Parsers for other keys (basenames) will only run on the matching file.
my %parser_map = (
    'index-faq'       => ['parse_faq'],
    'index-language'  => ['parse_faq'],
    'index-functions' => ['parse_functions'],
    'index-module'    => ['get_synopsis'],
    'index-default'   => ['get_anchors'],
    'perldiag'        => ['parse_diag_messages'],
    'perlpod'         => ['parse_pod_formatting_codes'],
    'perlpodspec'     => ['parse_pod_commands'],
    'perlre'          => [
        'parse_regex_modifiers',
    ],
    'perlrun'         => ['parse_cli_switches'],
    'perlvar'         => ['parse_multiheaders'],
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

sub links_from_index {
    my ( $self, $index ) = @_;
    my $path = $self->doc_fullpath( $index );
    return unless ( -f $path );
    my $links;

    my $index_link_parser = link_parser_for_index($index);

    my $dom = dom_for_file( $path );

    my $content = $dom->find('ul')->[4];

    my @links = $self->$index_link_parser($dom);

    foreach my $link (@links) {
        my $name     = $link->content;
        my $filename = $link->attr('href');
        my $basename = $filename =~ s/\.html$//r;
        my @parsers = get_parsers($index, $basename);

        $links->{ $name }->{ basename } = $basename;
        $links->{ $name }->{ filename } = $filename;
        $links->{ $name }->{ parsers } = \@parsers;
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
    return warn "No text for '$title'" unless $text;
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
#                               Helpers                               #
#######################################################################

my $default_text_selector = 'p, pre';

# Produce the 'abstract' text content from the given Mojo::DOM spec.
sub text_from_selector {
    my ($dom, $spec) = @_;
    $spec //= $default_text_selector;
    return $dom->children($spec)->join();
}

sub each_sub_li {
    my ($dom, $selector) = @_;
    return $dom->at($selector)->following('ul')->first->children('li')->each;
}

sub ul_list_parser {
    my %options = (
        link => sub { $_[0]->find('a')->first->{name} },
        text => sub { text_from_selector($_[0]) },
        aliases => sub { () },
        @_,
    );
    return sub {
        my ($self, $dom) = @_;
        my @articles;
        my @aliases;
        foreach my $item (each_sub_li($dom, $options{selector_main})) {
            my $link = $options{link}->($item);
            my $title = $options{title}->($item);
            my $text = $options{text}->($item);
            push @aliases, map { { new => $_, orig => $title } }
                $options{aliases}->($item, $title);
            push @articles, {
                title  => $title,
                anchor => $link,
                text   => $text,
            };
        }
        return {
            articles => \@articles,
            aliases  => \@aliases,
        };
    }
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
    foreach my $para ($question->following->each) {
        $description .= $para if ( $para->tag eq 'p' || $para->tag eq 'pre' );
        last if ( $para->tag eq 'a' && $para->attr->{name} );
    }
    $description =~ s/\n/ /g;
    return $description;
}

sub parse_faq {
    my ($self, $dom) = @_;
    my %parsed;
    my @articles;
    my @aliases;

    foreach my $faq_title ($dom->find('h2')->each) {
        my $link = get_faq_link($faq_title);
        my $title = $faq_title->text;
        my $title_without_index = $title =~ s/.*[0-9]\s*:\s*//r;
        if ( $title_without_index ne $title ) {
            push @aliases, {
                new  => $title_without_index,
                orig => $title
            };
        }
        my $description = build_description($faq_title);
        next unless $link;
        push @articles, {
            anchor => $link,
            title  => $title,
            text => $description,
        };
    }
    return {
        articles => \@articles,
        aliases  => \@aliases
    };
}

#######################################################################
#                              Functions                              #
#######################################################################

# TODO: Some functions (e.g., 'xor') are documented in 'perlop' so do not
# receive a good description from this parser.
my $skip = qr/To get the best experience. |Please note: Many features/;
sub parse_functions {
    my ($self, $dom) = @_;

    my $title = $dom->at('title')->text;
    $title =~ s/\s-\s.*//;

    my $hint = $dom->at('b')->text;

    $dom = $dom->at('div[id="content_body"]');

    my $description;
    foreach my $n ($dom->find('p, code')->each){
        next unless $n->content;
        next if $n->content =~ /$skip/;
        $description .= $n->content;
    }
    return unless $description;
    $description = trim_abstract($description, 100);
    $description = "<code><br>$hint<br></code><br>". $description;
    return {
        articles => [
            { title => $title, text => $description }
        ],
    };
}

sub parse_index_functions_links {
    my ($self, $dom) = @_;
    return @{$dom->find('a[href^="functions"]')->to_array};
}

#######################################################################
#                         Regular Expressions                         #
#######################################################################

sub aliases_regex_modifiers {
    my ($modifier) = @_;
    $modifier =~ s./..;
    my @aliases = (
        "$modifier modifier",
        "/$modifier modifier",
        "/$modifier",
    );
    return @aliases;
}

sub parse_regex_modifiers {
    ul_list_parser(
        selector_main => 'a[name="Modifiers"]',
        link => sub {
            my $name = $_[0]->find('a')->first->{name};
            return "*$name*";
        },
        title => sub {
            my $start = $_[0]->at('b')->text;
            return "/$start regular expression modifier";
        },
        aliases => sub {
            my ($modifier) = $_[1] =~ /^\/(.+) regular expression modifier$/;
            return aliases_regex_modifiers($modifier);
        },
    )->(@_);
}

#######################################################################
#                                 POD                                 #
#######################################################################

sub aliases_pod_formatting_codes {
    my ($code, $descr) = @_;
    return (
        "$code formatting code",
        "$code pod code",
        "pod $code",
        "pod $descr",
        "$descr pod",
    );
}

# TODO: Support ignoring certain 'fluff' words, eg., `pod hyperlink` instead
# of `pod a hyperlink`.
sub parse_pod_formatting_codes {
    ul_list_parser(
        selector_main => 'a[name="Formatting-Codes"]',
        title => sub {
            Mojo::Util::xml_escape($_[0]->at('code')->all_text =~ s/\s</</r)
        },
        aliases => sub {
            my ($code) = $_[1] =~ /^(\w+)/;
            my $descr = $_[0]->find('b')->first->text =~ s/-- //r;
            return aliases_pod_formatting_codes($code, $descr);
        },
    )->(@_);
}

sub aliases_pod_commands {
    my ($title) = @_;
    my @aliases;
    foreach my $command (split /\s*,\s*/, $title) {
        push @aliases, (
            "$command pod",
            "pod $command",
            "$command pod command",
            "pod $command command",
        );
    }
    return @aliases;
}

sub parse_pod_commands {
    ul_list_parser(
        selector_main => 'a[name="Pod-Commands"]',
        title => sub { $_[0]->at('b')->text =~ s/"//gr },
        aliases => sub { aliases_pod_commands($_[1]) },
    )->(@_);
}

#######################################################################
#                            Command-Line                             #
#######################################################################

sub aliases_cli_switches {
    my ($switch) = @_;
    $switch =~ s/\s.+$//;
    my @switches = $switch =~ /^-\[(\w+)\]$/
        ? (map { "-$_" } split '', $1)
        : ($switch);
    return map { (
        "$_",
        "$_ option",
        "$_ flag",
        "$_ switch",
    ) } @switches;
}

sub parse_cli_switches {
    ul_list_parser(
        selector_main => 'a[name="Command-Switches"]',
        title => sub { $_[0]->find('b')->first->all_text },
        aliases => sub { aliases_cli_switches($_[1]) },
    )->(@_);
}

#######################################################################
#                             Diagnostics                             #
#######################################################################

sub aliasas_diag_messages {
    my ($message) = @_;
    $message =~ s/%\w+//g;
    return ($message);
}

sub parse_diag_messages {
    ul_list_parser(
        selector_main => 'a[name="DESCRIPTION"]',
        title => sub { Mojo::Util::xml_escape($_[0]->find('b')->first->text) },
        aliases => sub { aliasas_diag_messages($_[1]) },
    )->(@_);
}

# For docs like perlfunc, perlvar with multiple headers per entry.
sub parse_multiheaders {
    my ( $self, $dom, $section ) = @_;
    my @mod_sections = $dom->at( sprintf 'a[name="%s"]', $section || "General-Variables" )->following('ul')->each;
    my ( @articles, @aliases, @titles );

    for my $mod_section ( @mod_sections ) {
        for my $li ( $mod_section->child_nodes->each ) {
            next unless $li->find('a[name]')->first;

            my $link = $li->find('a')->[0];
            my $anchor = "*$link->{name}*";


            my $title = $link->following('b')->first->text;
            my $text = text_from_selector($li);

            push @titles, $title;

            if ( $text ) {
                push @articles, {
                    title  => $title,
                    text   => $text,
                    anchor => $anchor,
                };

                my $alias_target = pop @titles;
                for ( @titles ) {
                    push @aliases, {
                        new => $_,
                        orig => $alias_target
                    }
                }

                @titles=();
            }
        }
    }
    return {
        articles => \@articles,
        aliases  => \@aliases
    };
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
    normalize_dom_links($url, $dom);
    return $dom;
}

sub parse_page {
    my ( $self, $page ) = @_;
    my $fullpath = $self->doc_fullpath( $page->{basename} );
    my $url = $self->doc_fullurl( $page->{filename} );
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

            $self->entry( $article->{title}, $article->{text}, $anchored_url );
        }

        for my $alias ( @{ $parsed->{aliases} } ) {
            $self->alias( $alias->{new}, $alias->{orig} );
        }
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
