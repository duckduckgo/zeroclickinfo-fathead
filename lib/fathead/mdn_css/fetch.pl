#!/usr/bin/env perl

use strict;
use warnings;
use v5.10.0;

use Carp 'croak';
use File::Spec::Functions;
use Mojo::UserAgent;
use Mojo::Util 'spurt';
use Mojo::URL;
use YAML::XS 'DumpFile';

my $ua = Mojo::UserAgent->new()->max_redirects(4);

open(
    my $fragments_fh,   '>:encoding(UTF-8)',
    catfile 'download', 'fragments.txt'
) or die $!;

my @transactions = map { $ua->get($_) } (
    'https://developer.mozilla.org/en-US/docs/Web/CSS/Reference',
    'https://developer.mozilla.org/en-US/docs/Web/CSS/Mozilla_Extensions',
    'https://developer.mozilla.org/en-US/docs/Web/CSS/Webkit_Extensions'
);
my %urls;    #hash used to remove duplicate urls
for my $transaction (@transactions) {
    process_transaction($transaction);
}

#downloaded file names will be named 1.html, 2.html ....
my $file_number                = 1;
my $current_active_connections = 0;
my $maximum_active_connections = 4;
my @keyword_urls;

#hash to store properties whose urls redirect to other properties
my %redirect_map;

queue_urls_for_download();

#see http://mojolicious.org/perldoc/Mojo/IOLoop#recurring
Mojo::IOLoop->recurring(
    0 => sub {
        for ( $current_active_connections + 1 .. $maximum_active_connections ) {
            my $url = shift @keyword_urls;
            if(!$url) {
                DumpFile('redirect_urls.yml', { redirects => {%redirect_map} } );
                return ( $current_active_connections or Mojo::IOLoop->stop );
            }

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
                        
                        #if the keyword URL redirects to another URL, write the keywords to a file
                        if($url ne $tx->req->url) {
                            my $keyword  = substr ($url, rindex($url, "/") + 1);
                            my $redirect = substr ($tx->req->url, rindex($tx->req->url, "/") + 1);
                            $redirect_map{$redirect} = $keyword;
                        }
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

sub process_transaction {
    my $tx = shift;

    if ( $tx->success ) {
        my $divs = $tx->res->dom->find('div.index, div.column-half');
        for my $div ( $divs->each ) {
            for my $ul ( $div->find('ul')->each ) {
                $ul->find('li')->map(
                    sub {
                        my $li = shift;
                        if ( $li->at('a') ) {
                            my $relative_url =
                              Mojo::URL->new( $li->at('a')->attr('href') );
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

sub queue_urls_for_download {

    for my $url ( keys %urls ) {
        $url = Mojo::URL->new($url);
        if ( $url->fragment ) {
            if ( $url =~ qr/transform-function/ ) {

=begin
    For fragment in urls past this path /transform-function/ we can get the
    actual link by taking the fragment, say #matrix(), removing the brackets,
    and append it to the original path so that we get the link
    https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/matrix
=cut

                my $clone = Mojo::URL->new( sprintf "%s://%s",
                    $url->protocol, $url->host );

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
                my $clone = Mojo::URL->new( sprintf "%s://%s",
                    $url->protocol, $url->host );
                $clone->path( $url->path );
                push @keyword_urls, $clone;
            }
            else {

=begin
    save the urls with fragments that cannot be reconciled to actual links
    to fragments.txt so that parse.pl can use this information
    to extract extra information about the fragments
=cut

                say $fragments_fh $url;
            }
        }
        else {
            push @keyword_urls, $url;
        }
    }
}
