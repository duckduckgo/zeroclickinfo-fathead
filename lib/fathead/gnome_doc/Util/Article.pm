package Util::Article;

use Mojo::URL;
use Moo;

has [qw(title abstract page)] => (
    is       => 'ro',
    required => 1,
);

has [qw(aliases categories)] => (
    is      => 'ro',
    default => sub { [] },
);

has anchor => (
    is => 'ro',
);

has url => (
    is  => 'lazy',
    doc => 'full page URL including anchor',
);

sub _build_url {
    my ($self) = @_;
    '' . Mojo::URL->new($self->page->url)->fragment($self->anchor);
}

1;
