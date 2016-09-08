#!/usr/bin/env perl

use strict;
use warnings;

binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";

use Cwd qw( getcwd );
use DBI;
use File::Spec;
use IO::All -utf8;
use Mojo::DOM;
use Moo;
use Text::CSV_XS;
use URI;
use Util qw(trim_abstract);
use List::Util qw(first);
use List::MoreUtils qw(uniq);

my %links;

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
       index-functions-by-cat
       index-pragmas
       index-utilities
       index-internals
       index-platforms
    / );

    foreach my $l ('A'..'Z') {
        push @index_pages, "index-modules-$l";
    }

    for ( @index_pages ) {
        $indices->{$_} = $self->links_from_index( $_ );
    }
    return $indices;
}

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
    # TODO: Add explicit parser for language syntax, or generalize the FAQ
    # parser.
    'index-language'  => ['parse_faq'],
    'index-functions' => ['parse_functions'],
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

map { $parser_map{"index-modules-$_"} = ['parse_package'] } ('A'..'Z');

sub get_parsers {
    my ($index, $basename) = @_;
    my $index_parsers = $parser_map{$index};
    my $basename_parsers = $parser_map{$basename};
    return (@{$index_parsers || []}, @{$basename_parsers || []});
}

my %link_parser_for_index = (
    'functions' => 'parse_index_functions_links',
    'functions-by-cat' => 'parse_functions_categories',
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
    if (defined $related && @$related) {
        $related_text = join '\n', map { "[[$_]]" } @$related;
    }
    my $category_text = join '\n', @{$article{categories} || []};
    return warn "No text for '$title'" unless $text;
    $text =~ s/\t/&#09;/g;
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
    return unless $description;
    $description =~ s/\n/ /g;
    return $description;
}

my $faq_start = qr/^(how ((can|do)( i)?|to)|what( is|'s)( (the|a))?) /i;

sub aliases_faq {
    my ($title) = @_;
    return (
        without_punct($title),
        $title =~ s/$faq_start//ri,
        without_punct($title =~ s/$faq_start//ri),
    );
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
            push @aliases, make_aliases($title, $title_without_index);
        }
        push @aliases, make_aliases($title,
            aliases_faq($title),
        );
        my $description = build_description($faq_title) or next;
        next unless $link;
        push @articles, {
            anchor => $link,
            title  => $title,
            text => $description,
            categories => ['Perl FAQs'],
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

# Fallback descriptions for when the page is empty.
my %functions_fallback;

my %function_categories;

sub build_description_functions {
    my ($fname, $syntaxes, $description) = @_;
    my @syntaxes = @$syntaxes;
    $description ||= $functions_fallback{$fname};
    return unless $description;
    my $syntax = Mojo::DOM->new("<pre>$syntaxes[0]</pre>");
    map { $syntax->at('pre')->append_content("<br />$_") }
        @syntaxes[1..$#syntaxes];
    $description = $syntax->to_string . $description;
    return $description;
}

# TODO: Some functions (e.g., 'xor') are documented in 'perlop' so do not
# receive a good description from this parser.
sub parse_functions {
    my ($self, $dom) = @_;

    my $fname = $dom->at('div#from_search + h1')->text;
    my @syntaxes = $dom->find('ul > li > a[name] + b')->map('text')->each;

    my $description = text_from_selector(
        $dom->at('ul:last-of-type > li:last-child > a[name]')->parent
    )->to_string;
    $description = build_description_functions($fname, [@syntaxes], $description)
        or return;

    my $title = "$fname (function)";

    my $article = {
        title => $title,
        text  => $description,
        categories => ['Perl Functions', @{$function_categories{$fname} || []}],
    };
    return {
        articles => [ $article ],
        aliases => [
            make_aliases($title,
                $fname, "$fname function", "$fname func", "$fname sub",
                "$fname routine", "$fname subroutine", "$fname method",
            ),
        ],
    };
}

sub parse_index_functions_links {
    my ($self, $dom) = @_;
    my @fns = $dom->find('a[href^=functions]')->each;
    foreach my $fn (@fns) {
        my ($descr) = $fn->parent->text =~ /- (.+)\s*+$/;
        $descr = ucfirst $descr . '.' if $descr;
        $functions_fallback{$fn->text} = $descr;
    }
    return @fns;
}

sub parse_functions_categories {
    my ($self, $dom) = @_;
    my @fns = $dom->find('a[href^=functions]')->each;
    foreach my $fn (@fns) {
        my $name = $fn->text;
        my $cat = $fn->parent->parent->preceding('h2')->last->text;
        my @cats = @{$function_categories{$name} || []};
        push @cats, "Perl $cat";
        $function_categories{$name} = \@cats;
    }
    return;
}

#######################################################################
#                              Glossary                               #
#######################################################################

sub parse_glossary_definitions {
    ul_list_parser(
        title => sub { $_[0]->find('b')->first->text . ' (definition)' },
        aliases => sub {
            my $term = $_[1] =~ s/ \(definition\)//r;
            return (
                "$term definition",
                "define $term",
                "$term",
            );
        },
        categories => sub { ['Perl Glossary'] },
        uls   => sub { $_[0]->find('h2 ~ ul')->each },
        force_redirect => sub {
            return undef unless $_[1]->{text} =~ qr{^<p>See <b>([^<]+)</b>\.</p>$};
            return $1;
        },
        disambiguation => sub {
            return undef unless $_[1]->{text} =~ qr{^<p>See <b>.*<b>};
            my @disambiguations;
            while ($_[1]->{text} =~ /<b>([^<]+)<\/b>/g) {
                my $see = $1;
                push @disambiguations, {
                    link => $see,
                    description => $_[0]->root->find('li > b')
                        ->grep(sub { $_->text =~ qr/$see/ })->first->parent->find('p')->first->text,
                };
            }
            return {
                title => $_[1]->{title},
                disambiguations => \@disambiguations,
            };
        },
        related => sub {
            return undef unless $_[1]->{text} =~ /See also[^.]*<b>/;
            my $res_text = $_[1]->{text};
            my @related;
            while ($_[1]->{text} =~ /(\(?See also[^.]*<\/b>\.\)?)/g) {
                my $see = $1;
                $res_text =~ s/\Q$see\E//;
                while ($see =~ /<b>([^<]*)<\/b>/g) {
                    push @related, $1;
                }
            }
            $_[1]->{text} = $res_text;
            return \@related;
        },
    )->(@_);
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
        categories => sub { ['Perl Regular Expression Modifiers'] },
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
            $_[0]->at('code')->all_text =~ s/\s</</r
        },
        categories => sub { ['Perl POD Formatting Codes'] },
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
        title => sub { $_[0]->at('b')->text =~ s/"//gr =~ s/\s.+$//r },
        categories => sub { ['Perl POD Commands'] },
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
        categories => sub { ['Perl Command-Line Switches'] },
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
        title => sub { $_[0]->find('b')->first->text },
        categories => sub { ['Perl Diagnostics'] },
        aliases => sub { aliasas_diag_messages($_[1]) },
    )->(@_);
}

#######################################################################
#                              Operators                              #
#######################################################################

my @op_types = ("Binary", "Ternary", "Unary");
my $op_re = join '|', @op_types;

sub aliases_operators {
    my ($operator) = @_;
    my ($op_type, $op_name) = $operator =~ /^($op_re) "([^"]+)"/;
    return (
        "$op_name", "$op_name operator",
        "$op_type $op_name", "$op_type $op_name operator",
    );
}

sub parse_operators {
    my ($self, $dom) = @_;
    my @operators = $dom->find('p')->grep(
        sub { $_->text =~ /^($op_re)/ }
    )->each;
    my @articles;
    my @aliases;
    foreach my $op (@operators) {
        my $text = $op->all_text;
        my ($op_type, $op_name) = $text =~ /^($op_re) ([^\s]+)/;
        my $title = "$op_type $op_name operator";
        my $link = $op->preceding('a[name] ~ h2')
            ->last->preceding('a[name]')->last->{name};
        map { push @aliases, { new => $_, orig => $title } }
            (aliases_operators($title));
        push @articles, {
            anchor => $link,
            text   => $text,
            title  => $title,
            categories => ['Perl Operators'],
        };
    }
    return {
        articles => \@articles,
        aliases  => \@aliases,
    };
}

# For docs like perlfunc, perlvar with multiple headers per entry.

sub parse_variables {
    my ($self, $dom) = @_;
    my @mod_sections = $dom->at('a[name="General-Variables"]')
        ->following('ul')->each;
    ul_list_parser(
        uls => \@mod_sections,
        title => sub { $_[0]->find('b')->first->text . ' (variable)' },
        aliases => sub { $_[1] =~ s/ \(variable\)//r },
        categories => sub {
            my ($item) = @_;
            my $subc = $item->parent->preceding('h2')->last->text;
            return ['Perl Variables', "Perl $subc"]
        },
    )->(@_);
}

#######################################################################
#                              Packages                               #
#######################################################################

sub aliases_package {
    my ($package) = @_;
    my $spaced = $package =~ s/::/ /gr;
    make_aliases("$package (module)",
        "$spaced", "$package",
        map { ("$package $_", "$spaced $_") } (
            'module', 'library', 'package',
        ),
    );
}

sub grab_section {
    my ($section_name, $dom, $match) = @_;
    $match //= '*';
    my $start = $dom->at(qq(a[name="$section_name"] + h1));
    return Mojo::Collection::c() unless $start;
    my $elt = $start;
    my @elts;
    while (1) {
        $elt = $elt->next;
        last unless $elt;
        if ($elt->matches('a[name]') && $elt->attr('name') =~ /^[A-Z]+$/) {
            last;
        }
        next unless $elt->matches($match);
        push @elts, $elt;
    }
    return Mojo::Collection::c(@elts); # Give back a Mojo collection
}

sub parse_package {
    my ($self, $dom) = @_;
    my $package_name = $dom->at('div#from_search + h1')->text;
    my $syns = grab_section('SYNOPSIS', $dom);
    my $synopsis = $syns->join();
    return {} unless $synopsis;
    $synopsis = $synopsis->to_string;
    my $short_desc = $dom->at('a[name="NAME"] + h1 + p') // '';
    $short_desc = $short_desc->to_string . "\n" if $short_desc;
    my $rel = grab_section('SEE-ALSO', $dom, 'p');
    my @related = $rel->map(sub { $_->find('a[href]') })->flatten
        ->map('text')->each;
    my $article = {
        text  => "$short_desc$synopsis",
        title => "$package_name (module)",
        related => \@related,
        categories => ['Perl Standard Modules'],
    };
    return {
        articles => [$article],
        aliases  => [aliases_package($package_name)],
    };
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
    foreach my $index ( sort keys %{$self->indices} ) {
        foreach my $page ( sort keys %{$self->indices->{ $index }} ) {
            $self->parse_page( $self->indices->{ $index }->{ $page } );
        }
    }

    $self->resolve_articles;
    $self->resolve_aliases;
    $self->resolve_disambiguations;
}

main->new->parse;
