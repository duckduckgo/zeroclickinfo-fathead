#!/usr/bin/env perl

use strict;
use warnings;
use v5.10.0;

use Carp 'croak';
use Mojo::UserAgent;
use Mojo::URL;

=begin
This script extracts data from Mozilla CSS Reference
https://developer.mozilla.org/en-US/docs/Web/CSS/Reference and is called by the
fetch.sh script since it is more efficient to fetch and parse the DOM using
something other than BASH to ease development and maintenance in the future.
=cut

my $ua = Mojo::UserAgent->new()->max_redirects(4);
my $reference_url =
  Mojo::URL->new('https://developer.mozilla.org/en-US/docs/Web/CSS/Reference');
my $tx = $ua->get($reference_url);

my @keyword_urls;

if ( $tx->success ) {

=begin
  We extract the collection of links to keywords from the DOM. The links wanted
  are found from this part of the DOM:
  <div class="index">
    <span>A</span>
    <ul>
      <li>
        <a href="/en-US/docs/Web/CSS/:active"><code>:active</code></a>
      </li>
    </ul>
    ...
  </div>
=cut

    my $divs = $tx->res->dom->find('div.index, div.column-half');
    for my $div ( $divs->each ) {
        for my $ul ( $div->find('ul')->each ) {
            my $relative_link = $ul->at('a')->attr('href');
            my $absolute_link = $reference_url->path($relative_link);
            say "--> $absolute_link";
            push @keyword_urls, $absolute_link;
        }
    }
}
elsif ( my $error = $tx->error ) {
    croak sprintf "Error: %d %s while fetching %s", $error->{code} || 0,
      $error->{message},
      $tx->req->url;
}
