package Util::Index;

use List::MoreUtils qw(uniq);
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

has assign_parsers => (
    is       => 'ro',
    required => 1,
    doc      => <<EOF
Function to be used for assigning parsers to pages.
Passed the page as a Util::Page object, and expects an ARRAY reference of
subroutines that match the following criteria to be returned:

    1. The routine must accept a Mojo::DOM object as its first argument.
    2. The routine must return a HASH reference (possibly empty) in the
       following format:
        {
           articles        => [Util::Article],
           disambiguations => [Util::Disambiguation],
        }
EOF
);

sub parsers_for {
    my ($self, $page) = @_;
    return $self->assign_parsers->($page);
}

sub _merge_index_url {
    my ($index_url, $url) = @_;
    return Mojo::URL->new($index_url)->path($url);
}

has pages => (
    is => 'lazy',
);

sub _build_pages {
    my ( $self ) = @_;
    my $dom = $self->_index_page->dom;
    my $res = $self->index_function->($dom);
    return [map { Util::Page->new(
        url => '' . _merge_index_url($self->index_url, $_)
    ) } uniq @$res];
}

1;
