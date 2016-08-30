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
        _dom_from_file($path);
    } else {
        if (my $dom = $self->_retrieve_from_cache) {
            return $dom;
        }
        my $res = $ua->get($url)->success
            or warn "Unable to fetch DOM for '$url'\n";
        my $dom = $res->dom if $res;
        $self->_cache($dom) if $dom;
        return $dom;
    }
}

sub _local_path {
    my ($self) = @_;
    return unless my ($path) = $self->url =~ q{^file://(.+)$};
    return path($path);
}

sub _url_cache_path {
    my ($url) = @_;
    return path('download/.cache', Mojo::URL->new($url)->path);
}

sub _cache {
    my ($self, $dom) = @_;
    my $path = _url_cache_path($self->url);
    $path->touchpath;
    $path->spew_utf8($dom->to_string);
}

sub _retrieve_from_cache {
    my ($self) = @_;
    my $path = _url_cache_path($self->url);
    return unless $path->exists;
    _dom_from_file($path)
        or $path->remove; # Maybe it'll correct itself in a future parse.
}

sub _dom_from_file {
    my ($path) = @_;
    warn "No such file '$path'" and return unless $path->exists;
    my $dom = Mojo::DOM->new($path->slurp_utf8);
    warn "Unable to parse DOM for '$path'\n" and return unless $dom->[0];
    return $dom;
}

1;
