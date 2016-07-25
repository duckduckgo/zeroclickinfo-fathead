#!/usr/bin/env perl

use v5.10.0;
use strict;
use warnings;

use Carp 'croak';
use Mojo::DOM;
use Mojo::Util 'slurp';
use Text::Trim;
use HTML::Strip;
use HTML::Entities;    # Used by HTML::Strip
use Data::Printer return_value => 'dump';

=begin
This script extracts data from html files under the
dowloads folder
=cut

# Keep track of unique keys
my %seen;

open( my $fh, ">", 'output.txt' ) or croak $!;

foreach my $html_file ( glob 'download/*.html' ) {

    say "\n----------------------------------------\n";
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

    my ( $title, $link, $description, @entries );

    # Get canonical link to article
    $link = $dom->find('link')->first(
        sub {
            my $lnk = shift;
            return 1 if $lnk->attr('rel') and $lnk->attr('rel') =~ /canonical/;
        }
    );
    $link = $link->attr('href') if $link;

    # Get article title and description
    for my $meta ( $dom->find('meta')->each ) {
        next unless $meta->attr('property');
        if ( $meta->attr('property') =~ /og\:title/ ) {
            $title = lc $meta->attr('content');
        }
        elsif ( $meta->attr('property') =~ /og\:description/ ) {
            $description = $meta->attr('content');
        }

        #no need to keep looping if we have both of these
        last if $title and $description;
    }

    # Clean title and check if article already processed
    # # TODO
    #   Some links point to page anchors!
    #   Need to account this and find correct page content
    #   E.g. units of <length>: px, em, cm,
    #   https://developer.mozilla.org/en-US/docs/Web/CSS/length#px
    #
    #   Need to grab <dt id=$title>
    my $title_clean = clean_string($title);
    if ( exists $seen{$title_clean} ) {
        say "SKIPPING: $title_clean!";
        next;
    }

    $seen{$title_clean} = 1;

    # Get syntax code snippet
    my $code;

    # TODO
    # Preserve HTML and use the same lib as Mozilla
    # (http://prismjs.com/) to render code?

    my $hs = HTML::Strip->new( emit_spaces => 0 );
    if ( my $pre = $dom->at('#Syntax ~ pre') ) {

        if ( $pre->child_nodes->first->matches('code') ) {
            $code = $pre->child_nodes->first->text;
        }
        else {
            $code = $hs->parse( $pre->to_string );
            $code =~ tr/ / /s;
        }

        $code = trim($code);
        say '';
        say $code;
        $code =~ s/\r?\n/\\n/g;
    }
    $hs->eof;
    $description = build_abstract( $description, $code );

    next unless $title && $link && $description;

    say '';
    say "TITLE: $title";
    say "LINK: $link";
    say "DESCRIPTION: $description";

    push @entries, make_article( $title_clean, $description, $link );

    # Check for CSS Functions
    # e.g. "not()"
    # Replace "()" with "function" in title
    # e.g. "not()" - > "not function"
    if ( $title_clean =~ m/\(\)$/ ) {
        say "Found Function: $title_clean";
        my $temp = $title_clean;
        $temp =~ s/\(\)$/ function/;
        push @entries, make_redirect( $temp, $title );
        $temp = $title_clean;
        $temp =~ s/\(\)$//;
        push @entries, make_redirect( $temp, $title );
    }

    foreach my $entry (@entries) {
        say $fh $entry;
    }
}

# PRIVATE FUNCTIONS

sub clean_string {
    my $input = shift;
    my $clean = $input;

    # Capture content inside parentheses
    # Check if identical or not
    # E.g. "::before (:before)"
    my $paren_re = qr/\((.+)\)/;

    $clean =~ s/[:<>@]//g;

    if ( $clean ne $input ) {
        say "Input: $input";
        say "Cleaned: $clean";
    }

    if ( $clean =~ m/$paren_re/ ) {
        say "Has parens!";
        my $paren_text = trim($1);
        say "Paren Text: $paren_text";
        $clean =~ s/$paren_re//;
        trim($clean);
        say "Clean Now: $clean";
        unless ( $clean eq $paren_text ) {
            say "Paren Text is Different!";
            $clean = "$clean $paren_text";
        }
        say "Clean Final: $clean";
    }
    return $clean;
}

sub build_abstract {
    my ( $description, $code ) = @_;
    say "NO DESCRIPTION!" if $description eq "";
    my $out;
    $out .= "<p>$description</p>" if $description;
    $out .= "<br><pre><code>$code</code></pre>" if $code;
    return $out;
}

sub make_article {
    my ( $title, $description, $link ) = @_;
    my @data =
      ( $title, 'A', '', '', '', '', '', '', '', '', '', $description, $link );
    return join "\t", @data;
}

sub make_redirect {
    my ( $title, $redirect ) = @_;
    my @data =
      ( $title, 'R', $redirect, '', '', '', '', '', '', '', '', '', '' );
    return join "\t", @data;
}
