package Util::Article;

use Mojo::URL;
use Moo;

has title => (
    is       => 'ro',
    required => 1,
);

has page => (
    is => 'rwp',
);

has abstract => (
    is       => 'rwp',
    required => 1,
);

has [qw(aliases categories related)] => (
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

sub _normalize_links {
    my ($links, $dom) = @_;
    $dom->find('a[href]')->map(sub {
        my $link = $_->attr('href');
        if (my $point = $links->{$link}) {
            $_->attr(href => "/?q=$point&ia=about");
        }
    });
    return $dom;
}

# Normalize the article for the DB - account for weird parser issues.
sub normalize {
    my ($self, $links) = @_;
    my $abstract = $self->abstract;
    $abstract =~ s/\n/ /g;
    # Okay, the parser *really* hates links...
    my $dom = Mojo::DOM->new->parse($abstract);
    $dom = _normalize_links($links, $dom);
    $dom->find('a')->map(tag => 'span');
    $abstract = $dom->to_string;
    # The internal DB doesn't like escapes...
    $abstract =~ s{\\}{\\\\}g;
    $self->_set_abstract($abstract);
}

1;
