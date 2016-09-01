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

has dom => (
    is      => 'ro',
    lazy    => 1,
    builder => 1,
);

sub _normalize_dom {
    my ($url, $dom) = @_;
    $dom->find('a')->map(sub {
        my $link = $_[0]->attr('href') or return;
        $_[0]->attr(href => URI->new_abs($link, $url)->as_string);
    });
    # Current back-end doesn't like 'strong' tags
    $dom->find('strong')->map('strip');
    return $dom;
}

sub _build_dom {
    my ($self) = @_;
    my $url = $self->url;
    if (my $path = $self->_local_path) {
        _normalize_dom($url, _dom_from_file($path));
    } else {
        if (my $dom = $self->_retrieve_from_cache) {
            return $dom;
        }
        my $res = $ua->get($url)->success
            or die "Unable to fetch DOM for '$url'\n";
        my $dom = _normalize_dom($url, $res->dom) if $res;
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
    my $u = Mojo::URL->new($url);
    return path('download/.cache', $u->host, $u->path);
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
    _dom_from_file($path);
}

sub _dom_from_file {
    my ($path) = @_;
    die "No such file '$path'" and return unless $path->exists;
    my $dom = Mojo::DOM->new($path->slurp_utf8);
    do {
        $path->remove; # Maybe it'll correct itself in a future parse.
        die "Unable to parse DOM for '$path'\n";
    } unless $dom->[0];
    return $dom;
}

1;
