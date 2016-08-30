package Util::Page;
# ABSTRACT: Page along with parse and DOM information.

use Moo;
use Mojo;

my $ua = Mojo::UserAgent->new;

has url => (
    is       => 'ro',
    doc      => 'full URL to the page',
    required => 1,
);

has [qw(basename filename parsers full_path full_url)] => (
    is => 'ro',
);

has dom => (
    is      => 'ro',
    lazy    => 1,
    builder => 1,
);

sub _build_dom {
    my ($self) = @_;
    my $url = $self->url;
    my $res = $ua->get($url)->success
        or warn "Unable to fetch DOM for '$url'\n";
    return $res->dom if $res;
}

1;
