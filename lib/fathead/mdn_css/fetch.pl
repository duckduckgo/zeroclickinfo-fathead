#!/usr/bin/env perl

use strict;
use warnings;
use v5.10.0;

use Carp 'croak';
use File::Spec::Functions;
use Mojo::UserAgent;
use Mojo::Util 'spurt';
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

my %urls;    #hash used to remove duplicate urls

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
            $ul->find('li')->map(
                sub {
                    my $li = shift;
                    my $relative_url =
                      Mojo::URL->new( $li->at('a')->attr('href') );
                    my $absolute_url = $relative_url->to_abs( $tx->req->url );
                    $urls{$absolute_url} = 1;
                }
            );
        }
    }
}
elsif ( my $error = $tx->error ) {
    croak sprintf "Error: %d %s while fetching %s", $error->{code} || 0,
      $error->{message},
      $tx->req->url;
}

#downloaded file names will be named 1.html, 2.html ....
my $file_number = 1;

my $current_active_connections = 0;
my $maximum_active_connections = 4;

#save the urls with fragments to a text file called
#fragments.txt so that parse.pl can use this information
#to extract extra information about the fragments
my @keyword_urls        = map  { Mojo::URL->new($_) } keys %urls;
my @urls_with_fragments = grep { $_->fragment } @keyword_urls;
if (@urls_with_fragments) {
    open( my $fh, '>:encoding(UTF-8)', catfile 'download', 'fragments.txt' )
      or die $!;
    for my $urls_with_fragment (@urls_with_fragments) {
        say $fh $urls_with_fragment;
    }
}

#see http://mojolicious.org/perldoc/Mojo/IOLoop#recurring
Mojo::IOLoop->recurring(
    0 => sub {

        #fetch 4 at a time
        for ( $current_active_connections + 1 .. $maximum_active_connections ) {
            return ( $current_active_connections or Mojo::IOLoop->stop )
              unless my $url = shift @keyword_urls;

            ++$current_active_connections;
            $ua->get(
                $url => sub {
                    my ( undef, $tx ) = @_;

                    --$current_active_connections;
                    if ( $tx->success ) {
                        say sprintf "%s %s", $tx->res->message, $tx->req->url;
                        spurt $tx->res->body, catfile 'download',
                          "$file_number.html";
                        ++$file_number;
                    }
                    elsif ( my $error = $tx->error ) {

                        #TODO: Should we push this url into
                        #@keyword_urls so that it is
                        #retried or we just warn or we croak?
                        #warn for now
                        warn sprintf "Error: %s %s", $error->{message},
                          $tx->req->url;
                    }
                }
            );
        }
    }
);
Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
