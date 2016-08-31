package Util::Index;

use Cwd qw( getcwd );
use Moo;

use Util::Page;

has docs_dir => ( is => 'lazy' );
sub _build_docs_dir {
    File::Spec->catdir( $_[0]->working_dir, qw/ .. download / );
}

has working_dir => ( is => 'lazy' );
sub _build_working_dir {
    getcwd;
}

has index_url => (
    is       => 'ro',
    required => 1,
    doc      => "Page to be used for indexing - " .
        "takes the same form as 'url' in Util::Page",
);

has _index_page => (
    is => 'lazy',
);

sub _build__index_page {
    my ($self) = @_;
    return Util::Page->new(url => $self->index_url);
}

has index_function => (
    is       => 'ro',
    required => 1,
    doc      => <<EOF
Function to be used for producing the index.
Will be passed a Mojo::DOM object representing the page, and expects an
ARRAY reference containing the page URLs to be parsed. These should be in the
same format as 'url' for Util::Page.
EOF
);

sub doc_fullurl {
    my ( $self, $part ) = @_;
    URI->new(
        sprintf( '%s%s', $self->perldoc_url, $part )
    )->canonical
}

sub _merge_index_url {
    my ($index_url, $url) = @_;
    return Mojo::URL->new($index_url)->path($url);
}

sub build_indices {
    my ( $self ) = @_;
    my $dom = $self->_index_page->dom;
    my $res = $self->index_function->($dom);
    return [map { Util::Page->new(
        url => '' . _merge_index_url($self->index_url, $_)
    ) } @$res];
}

sub doc_fullpath {
    my ( $self, @parts ) = @_;
    $parts[-1] = $parts[-1] . '.html';
    File::Spec->catfile( $self->docs_dir, @parts );
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

        $links->{$name} = Util::Page->new(
            basename  => $basename,
            filename  => $filename,
            parsers   => \@parsers,
            full_path => $self->doc_fullpath($basename),
            full_url  => $self->doc_fullurl($filename),
        );
    }

    return $links;
}

1;
