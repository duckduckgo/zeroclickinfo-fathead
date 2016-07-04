#!/usr/bin/env perl

use v5.10.0;
use strict;
use warnings;

use Carp 'croak';
use Mojo::DOM;
use Mojo::Util 'slurp';

=begin
This script extracts data from html files under the
dowloads folder
=cut

my %titles_dedup;    #keys will be used to remove duplicates in titles
open( OUT, ">", 'output.txt' ) or croak $!;
for my $html_file (<download/*.html>) {
    say "Processing $html_file";
    my $dom = Mojo::DOM->new( slurp $html_file);

=begin
  Name of CSS tag is found here:
  <!-- document-specific social tags -->
  <meta property="og:title" content=":right">

  and the description is from here:
  <meta property="og:description"
  content="The :right CSS page pseudo-class matches any right page when
  printing a page. It allows to describe the styling of right-side page.">

  The link is found in a this section
  <link rel="canonical"
  ref="https://developer.mozilla.org/en-US/docs/Web/CSS/:right" >
=cut

    my $title;
    my $description;

    #return first element for which the anonymous sub returns true
    my $link = $dom->find('link')->first(
        sub {
            my $lnk = shift;
            return 1 if $lnk->attr('rel') and $lnk->attr('rel') =~ /canonical/;
        }
    );
    $link = $link->attr('href') if $link;

    for my $meta ( $dom->find('meta')->each ) {
        next unless $meta->attr('property');
        if ( $meta->attr('property') =~ /og\:title/ ) {
            $title = $meta->attr('content');
        }
        elsif ( $meta->attr('property') =~ /og\:description/ ) {
            $description = $meta->attr('content');
        }

        #no need to keep looping if we have both of these
        last if $title and $description;
    }
    if ( $title and $link and $description ) {
        say $title       if $title;
        say $link        if $link;
        say $description if $description;
        say '';

        unless ( exists $titles_dedup{$title} ) {
            $titles_dedup{$title} = 1;
            my @data = (
                $title, 'A', '', '', '', '', '', '', '', '', '', $description,
                $link
            );
            say OUT join "\t", @data;
        }
    }
}
