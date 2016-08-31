package Util::Index;

use Moo;

use Util::Page;

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

1;
