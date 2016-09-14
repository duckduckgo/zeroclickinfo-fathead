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
my $webkit_extensions_url = Mojo::URL->new(
    'https://developer.mozilla.org/en-US/docs/Web/CSS/Webkit_Extensions');
my $mozilla_css_extensions_url = Mojo::URL->new(
    'https://developer.mozilla.org/en-US/docs/Web/CSS/Mozilla_Extensions');

my @fetch_links =
  ( $reference_url, $webkit_extensions_url, $mozilla_css_extensions_url );
my @txs = map { $ua->get($_) } @fetch_links;
my %urls;    #hash used to remove duplicate urls

for my $tx (@txs) {
    if ( $tx->success ) {
        my $divs = $tx->res->dom->find('div.index, div.column-half');
        for my $div ( $divs->each ) {
            for my $ul ( $div->find('ul')->each ) {
                $ul->find('li')->map(
                    sub {
                        my $a = $_->at('a');
                        if ($a){
                          my $relative_url =
                            Mojo::URL->new( $a->attr('href') );
                          my $absolute_url =
                            $relative_url->to_abs( $tx->req->url );
                          $urls{$absolute_url} = 1;
                        }
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
}

#downloaded file names will be named 1.html, 2.html ....
my $file_number = 1;

my $current_active_connections = 0;
my $maximum_active_connections = 4;

my @keyword_urls;

=begin
    save the urls with fragments to a text file called
    fragments.txt so that parse.pl can use this information
    to extract extra information about the fragments
=cut

open(
    my $fragments_fh,   '>:encoding(UTF-8)',
    catfile 'download', 'fragments.txt'
) or die $!;

for my $url ( keys %urls ) {
    $url = Mojo::URL->new($url);
    if ( $url->fragment ) {

=begin
    For fragment in urls past this path /transform-function/ we can get
    the link for the actual fragment in the following way.
    Take for example this fragment #matrix()
    In order to get the actual link, take the fragment, matrix(), remove
    the brackets, and append it to the original path so that we get the link
    https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/matrix
=cut

        if ( $url =~ qr/transform-function/ ) {
            my $clone =
              Mojo::URL->new( sprintf "%s://%s", $url->protocol, $url->host );

            #trailing / needed at the end so that when we add a
            #fragment at the end it does not replace the former last
            #part of path
            $clone->path( $url->path . '/' );
            my $fragment = $url->fragment;
            $fragment =~ s/\(\)//g;
            $clone->path($fragment);
            push @keyword_urls, $clone;
        }
        elsif ( $url->fragment =~ /The_url/ ) {

            #It is this: https://developer.mozilla.org/en-U/docs/Web/CSS/
            #/url#The_url()_functional_notation
            #We get rid of the fragment part so that it can be downloaded
            #and parsed by parse.pl because its format is like the others
            my $clone =
              Mojo::URL->new( sprintf "%s://%s", $url->protocol, $url->host );
            $clone->path( $url->path );
            push @keyword_urls, $clone;
        }
        else {
            #we will deal with other types of urls later in parse.pl
            say $fragments_fh $url;
        }
    }
    else {
        push @keyword_urls, $url;
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
