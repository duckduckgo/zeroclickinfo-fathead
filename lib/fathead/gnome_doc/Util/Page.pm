package Util::Page;
# ABSTRACT: Page along with parse and DOM information.

use Moo;
use Mojo;
use Path::Tiny;

my $ua = Mojo::UserAgent->new;

has url => (
    is       => 'ro',
    doc      => q{full URL to the page. Prefix with 'file://' to use a local file},
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
    if (my $path = $self->_local_path) {
        warn "No such file '$path'" and return unless $path->exists;
        my $dom = Mojo::DOM->new($path->slurp_utf8)
            or warn "Unable to parse DOM for '$path'\n" and return;
        return $dom;
    } else {
        my $res = $ua->get($url)->success
            or warn "Unable to fetch DOM for '$url'\n";
        return $res->dom if $res;
    }
}

sub _local_path {
    my ($self) = @_;
    return unless my ($path) = $self->url =~ q{^file://(.+)$};
    return path($path);
}

1;
