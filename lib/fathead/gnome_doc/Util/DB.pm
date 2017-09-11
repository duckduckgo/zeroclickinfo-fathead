package Util::DB;

use strict;
use warnings;

binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";

use DBI;
use Mojo::DOM;
use Moo;
use Text::CSV_XS;
use List::MoreUtils qw(uniq);
use Path::Tiny;

my %links;

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

has output_txt => ( is => 'lazy' );
sub _build_output_txt {
    open my $fh, '>:encoding(UTF-8)', 'output.txt';
    return $fh;
};

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
    $self->disambiguations->{$disambiguation->title} = $disambiguation;
}

sub insert_disambiguation {
    my ($self, $disambiguation) = @_;
    my @disambiguations = map {
        my ($t, $a) = ($_->title, $_->abstract);
        "*[[$t]], $a.";
    } @{ $disambiguation->articles };
    my $dtext = join '\n', @disambiguations;
    $self->insert({
        type => 'D',
        title => $disambiguation->title,
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
    my $title = $article->title;
    warn "Duplicate article with title '$title' detected\n" and return
        if exists $self->articles->{$title};
    $links{$article->url} = $title;
    $self->articles->{$title} = $article;
}

# Ensure abstract doesn't have anything that *can't* go into the DB.
sub normalize_abstract {
    my ($abstract) = @_;
    $abstract =~ s/\n/&#13;/g;
    $abstract =~ s/\t/&#09;/g;
    $abstract =~ s/\\/&#92;/g;
    return $abstract;
}

sub entry {
    my ($self, $article) = @_;
    my ($title, $related) = map { $article->$_ } (
        qw(title related)
    );
    my $related_text = '';
    if (defined $related && @$related) {
        $related_text = join '\n', map { "[[$_]]" } @$related;
    }
    my $category_text = join '\n', @{$article->categories};
    return warn "No text for '$title'" unless $article->abstract;
    $self->insert({
        abstract => normalize_abstract($article->abstract),
        categories => $category_text,
        title => $article->title,
        type  => 'A',
        related  => $related_text,
        sourceurl => $article->url,
    });
}

sub retrieve_entry {
    my ($self, $title) = @_;
    return $self->articles->{$title} // $self->disambiguations->{$title};
}

#######################################################################
#                       Normalize Parse Results                       #
#######################################################################

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
    my $links = \%links;
    foreach my $article (values %articles) {
        $article->normalize($links);
        $self->entry($article);
    }
}

sub resolve_disambiguations {
    my ($self) = @_;
    foreach my $d (values %{$self->disambiguations}) {
        $self->insert_disambiguation($d);
    }
}

# Ensure we don't already have an output.txt
sub _clean_working_directory {
    path('output.txt')->remove;
}

# Resolve articles and build the output database
sub build_output {
    my ($self) = @_;
    _clean_working_directory;
    $self->resolve_articles;
    $self->resolve_aliases;
    $self->resolve_disambiguations;
}

1;
