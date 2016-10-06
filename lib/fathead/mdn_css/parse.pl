#!/usr/bin/env perl

use v5.10.0;
use strict;
use warnings;

use Carp 'croak';
use File::Spec::Functions;
use feature 'state';
use Mojo::DOM;
use Mojo::URL;
use Mojo::Util 'slurp';
use Text::Trim;
use HTML::Strip;
use HTML::Entities;    # Used by HTML::Strip
use Data::Printer return_value => 'dump';
use YAML::XS 'LoadFile';

=begin
This script extracts data from html files under the downloads folder
=cut

# Keep track of unique keys
my %SEEN;

# Map categories to redirect keywords
# These are used to create additional redirects
my %redirect_map = (
    'css pseudo elements' => 'pseudo element',
    'css pseudo classes'  => 'pseudo class',
    'css properties'      => 'property',
    'css functions'       => 'function',
    'css data types'      => 'data type',
    'css at rules'        => 'at rule',
);

# Inverse css_categories.yml
#  - The per-category structure is easier read/edit
#  - The inverse mapping is used to lookup titles as we create articles
my ( $categories, $extra_categories, $units ) = LoadFile('css_categories.yml');
my %titles;
my %units;

# Map titles to categories
while ( my ( $category, $array ) = each %{$categories} ) {
    foreach my $title ( @{$array} ) {
        $titles{$title}{categories} = [];
        push @{ $titles{$title}{categories} }, $category;
    }
}

# Map titles to extra categories
while ( my ( $category, $array ) = each %{$extra_categories} ) {
    foreach my $title ( @{$array} ) {
        $titles{$title}{extra_categories} = [];
        push @{ $titles{$title}{extra_categories} }, $category;
    }
}

# Map titles to units
while ( my ( $unit, $array ) = each %{$units} ) {
    foreach my $title ( @{$array} ) {
        $units{$title} = $unit;
    }
}

# p(%redirect_map);
# p(%titles);
# p(%units);

=begin
Process fragment data like matrix3d() in
https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function#matrix3d()
whenever a url matches one of these regexes.
Each fragment shall go to its own line in the output.txt file
TODO: Confirm this is the correct approach
=cut

my $fragments_file = catfile 'download', 'fragments.txt';

# keys are urls, values are fragments
my %url_fragment;
if ( -e $fragments_file ) {
    open( my $fh, '<:encoding(UTF-8)', $fragments_file ) or die $!;
    while ( my $url = <$fh> ) {
        chomp $url;
        $url = Mojo::URL->new($url);
        my $url_clone = $url->clone;
        my $fragment  = $url->fragment;
        $url = Mojo::URL->new( sprintf "%s://%s",
            $url_clone->protocol, $url_clone->host )->path( $url_clone->path );
        push @{ $url_fragment{$url} }, $fragment;
    }
}

open( my $fh, ">", 'output.txt' ) or croak $!;

###############
# PARSING LOOP
###############

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

    my ( $title, $link, $description, $initial_value, @entries );

    # Get canonical link to article
    $link = $dom->find('link')->first(
        sub {
            my $lnk = shift;
            return 1 if $lnk->attr('rel') and $lnk->attr('rel') =~ /canonical/;
        }
    );
    $link = $link->attr('href') if $link;
    chomp $link;
    $link = Mojo::URL->new($link);

    if ( exists $url_fragment{$link} ) {
        say "*** Parsing fragment for $link ***";
        parse_fragment_data( $link, $dom );
    }

    # Get article title
    my $meta_with_title = $dom->find('meta')->first(
        sub {
            my $meta     = $_;
            my $property = $meta->attr('property');
            $property && $property =~ /og\:title/;
        }
    );
    $title = lc $meta_with_title->attr('content') if $meta_with_title;

    my $h2 = $dom->at('h2#Summary');
    if ($h2) {
        my $p = $h2->next;
        if ($p) {
            $description = $p->all_text;
            my $next = $p->next;
            if ( $next && $next->tag eq 'ul' ) {
                my $li = $next->at('li');
                my $a = $li->at('a') if $li;
                if ( $a && $a->text =~ /Initial value/ ) {
                    $initial_value = $li->text;
                    $initial_value =~ s/://;
                    $initial_value = _build_initial_value($initial_value);
                }
                else {
                    $description .=
                      $next->find('li')->map('all_text')->join(', ');
                }
            }
        }
    }
    else {
        my $wiki_article = $dom->at('article#wikiArticle');
        if ($wiki_article) {
            my $div = $wiki_article->at('div');
            if ($div) {
                my $next_element = $div->next;
                if ( $next_element && $next_element->tag eq 'p' ) {
                    $description = $next_element->all_text;
                }
            }
        }
    }

    # Check if article already processed
    if ( exists $SEEN{$title} ) {
        say "SKIPPING: $title!";
        next;
    }

    $SEEN{$title} = 1;

    # Get syntax code snippet
    my $code;

    # TODO
    # Preserve HTML and use the same lib as Mozilla
    # (http://prismjs.com/) to render code?

    if ( my $pre = $dom->at('#Syntax ~ pre') ) {

        if ( $pre->child_nodes->first->matches('code') ) {
            $code = $pre->child_nodes->first->text;
        }
        else {
            $code = $pre->to_string;
        }

        $code = trim($code);

        # say '';
        # say $code;
    }

    #initial value is found in the table properties
    my $table_properties = $dom->at('table.properties');
    if ($table_properties) {
        $initial_value = parse_initial_value($table_properties);
    }
    $description = create_abstract( $description, $code, $initial_value );

    next unless $title && $link && $description;

    create_article( $title, $description, $link );
    if ( $title =~ m/[():<>@]/ || exists $titles{$title} ) {
        create_redirects($title);
    }
}

####################
# HELPER FUNCTIONS
####################

sub create_abstract {
    my ( $description, $code, $initial_value ) = @_;
    if ($description) {
        $description = trim($description);
        $description =~ s/\r?\n+/\\n/g;
    }
    else {
        say "NO DESCRIPTION!";
    }
    $initial_value =~ s/\r?\n+/\\n/g if $initial_value;
    $code = _clean_code($code) if $code;
    my $out;
    $out .= "<p>$description</p>"           if $description;
    $out .= "<p>$initial_value</p>"         if $initial_value;
    $out .= "<pre><code>$code</code></pre>" if $code;
    return $out;
}

# Build HTML string containing Initial Value data
sub parse_initial_value {
    my ($table_properties) = @_;

    #first <tr> Contains the initial value
    my $tr = $table_properties->find('tr')->first;
    my $th = $tr->at('th');
    my $initial_value;
    if ($th) {
        my $a = $th->at('a');
        if ( $a && $a->text =~ /Initial value/ ) {
            my $td = $tr->at('td');
            if ( $td->at('ul') ) {
                for my $element ( $td->find('code, a, br')->each ) {
                    $element->replace( $element->all_text );
                }
                $initial_value .= $td->content;
            }
            else {
                $initial_value = trim( $td->all_text );
            }
            $initial_value = trim($initial_value);
            $initial_value = _build_initial_value($initial_value);
        }
    }
    return $initial_value;
}

# Create Article and Redirects as needed
# Write to output files
sub create_article {
    my ( $title, $description, $link ) = @_;
    my @data;

    my $categories = '';
    my $lookup     = _category_title($title);

    if ( exists $titles{$lookup} ) {
        say "CATEGORY MATCH: ";
        p( $titles{$lookup} );
        my @cats = @{ $titles{$lookup}->{categories} };
        p(@cats);
        $categories = join '\\n', @cats;
    }

    push @data, _build_article( $title, $categories, $description, $link );
    _write_to_file(@data);
}

sub create_redirects {

=begin
    Check for CSS Functions like not() or for titles
    beginning with @ like @page and replace "()"
    with "function" in title e.g. "not()" -> "not function".
    remove @ and create output entry with redirect field
=cut

    my $title       = shift;
    my $title_clean = _clean_string($title);
    my $lookup      = _category_title($title);
    my @data;
    my $postfix;
    my $outputline;

    if ( exists $titles{$lookup} ) {

       #TODO If multiple categories per article exist, improve redirect creation
        my $category = @{ $titles{$lookup}->{categories} }[0];
        $postfix = $redirect_map{$category};
        say "POSTFIX: $postfix";
    }

    # Capture content inside parentheses
    # E.g. "::before (:before)"
    if ( $title =~ m/(.+) \((:.+|-webkit.+)\)/ ) {
        say "Has parens!";
        my $outer = $1;
        my $inner = $2;
        say "OUTER: $outer";
        say "INNER: $inner";
        my $inner_clean = _clean_string($inner);
        push @data, _build_redirect( $outer,       $title );
        push @data, _build_redirect( $inner,       $title );
        push @data, _build_redirect( $inner_clean, $title );

        if ($postfix) {
            push @data, _build_redirect( "$inner $postfix",       $title );
            push @data, _build_redirect( "$outer $postfix",       $title );
            push @data, _build_redirect( "$inner_clean $postfix", $title );
        }
    }
    elsif ( $title_clean ne $title ) {
        push @data, _build_redirect( $title_clean,            $title );
        push @data, _build_redirect( "$title_clean $postfix", $title )
          if $postfix;
    }
    elsif ($postfix) {
        push @data, _build_redirect( "$title $postfix", $title );
    }
    _write_to_file(@data);
}

####################
# PRIVATE FUNCTIONS
####################

# Build HTML string containing Initial Value data
sub _build_initial_value {
    my ($content) = @_;
    my $initial_value = "<b>Initial Value: </b><em>$content</em>";
    return $initial_value;
}

# Clean up code blocks
# - strip HTML content
# - collapse spaces
# - escape newlines
sub _clean_code {
    my ($code) = @_;
    my $hs = HTML::Strip->new( emit_spaces => 0 );
    $code = $hs->parse($code);
    $code =~ tr/ / /s;
    $code =~ s/\r?\n/\\n/g;
    $hs->eof;
    return $code;
}

# Clean up title for lookup in Categories hash
sub _category_title {
    my $title = shift;
    $title = $1 if $title =~ m/(::.+) \((:.+)\)/;
    $title =~ s/\(\)$//;
    say "CATEGORY TITLE: $title";
    return $title;
}

# Remove certain non-alphanumeric characters
sub _clean_string {
    my $input = shift;
    say "Input: '$input'";
    $input =~ s/[:<>@()]//g;
    trim($input);
    say "Cleaned: '$input'";
    return $input;
}

# Build Article string for given title, description, and link
sub _build_article {
    my ( $title, $categories, $description, $link ) = @_;
    say '';
    say "ARTICLE: $title";
    say "LINK: $link";
    say "CATEGORIES: $categories";

    # say "DESCRIPTION: $description" if $description;
    return join "\t",
      (
        $title, 'A', '', '', $categories, '', '', '', '', '', '', $description,
        $link
      );
}

sub _build_redirect {
    my ( $title, $redirect ) = @_;
    say "REDIRECT: $title =========> $redirect";
    return join "\t",
      ( $title, 'R', $redirect, '', '', '', '', '', '', '', '', '', '' );
}

sub _write_to_file {
    my @parts = @_;
    for my $part (@parts) {
        say $fh $part;
    }
}

####################
# PARSING FRAGMENTS
####################

sub parse_fragment_data {
    my ( $link, $dom ) = @_;
    state %already_processed;
    return if exists $already_processed{$link};
    $already_processed{$link} = 1;
    if ( $link =~ qr/font-variant/ ) {

        #TODO: Implement parsing when the page is available. Currently
        #it generates 404 not found response
        say "Font Variant $link";
    }
    elsif ( $link =~ qr/src/ ) {

        #TODO: Implement parsing when the page is available. Currently
        #it generates 404 not found response
        say "src $link";
    }
    elsif ( $link =~ qr/color_value/ ) {
        for my $fragment ( @{ $url_fragment{$link} } ) {
            my $h3 = $dom->at("h3#$fragment");
            next unless $h3;
            my $title = $fragment;
            my $url   = $link->clone->fragment($title);
            my $description;
            my $next_element = $h3->next;

=begin
            Collect all the description and code for the
            fragment in all the nodes following the current h3.
            When another h3 is encountered, write the previous h3 data
            and start over again until all h3s are processed
=cut

            my $paragraphs;
            my $code_fragment;
            do {
                next unless $next_element;
                if ( $next_element->tag eq 'p' ) {
                    $paragraphs .= $next_element->all_text;
                }
                else {
                    $code_fragment .= $next_element->to_string;
                }
                $next_element = $next_element->next;
            } while ( $next_element && $next_element->tag ne 'h3' );
            $description = create_abstract( $paragraphs, $code_fragment );
            create_article( $title, $description, $url );
        }
    }
    elsif ( $link =~ qr/filter/ ) {
        for my $fragment ( @{ $url_fragment{$link} } ) {

=begin
            Some h3 elements have a variant form of the fragment
            as the id like <h3 id="blur()_2"> instead of <h3 id="blur()">
            so regex will be used to match fragment with its correct h3
=cut

            my $h3 = $dom->find('h3')->first(
                sub {
                    $_->attr('id') =~ qr/$fragment/;
                }
            );
            next unless $h3;
            my $title = $fragment;
            my $url   = $link->clone->fragment( $h3->attr('id') );
            my $description;
            my $next_element = $h3->next;

=begin
            Collect all the description and code for the
            fragment in h3 until we encounter a div which
            means we have reached the end of all the data for
            our current h3
=cut

            my $paragraphs;
            my $code_fragment;
            do {
                next unless $next_element;
                if ( $next_element->tag eq 'p' ) {
                    $paragraphs .= $next_element->all_text;
                }
                else {
                    $code_fragment .= $next_element->to_string;
                }
                $next_element = $next_element->next;
            } while ( $next_element && $next_element->tag ne 'div' );
            $description = create_abstract( $paragraphs, $code_fragment );
            create_article( $title, $description, $url );
        }
    }
    elsif ( $link =~ qr/length/ ) {
        my $dl_collection = $dom->find('dl');
        for my $dl ( $dl_collection->each ) {
            for my $dt ( $dl->find('dt')->each ) {
                my $title = $dt->all_text;

                #TODO: Add building of external urls?
                my $url = $link->clone->fragment( $dt->attr('id') );
                my $dd  = $dt->next;
                my $description;
                if ($dd) {
                    $description = create_abstract( $dd->all_text );
                }
                create_article( $title, $description, $url );
            }
        }
    }
    elsif ( $link =~ qr/time|frequency|angle/ ) {
        my $h2 = $dom->at('h2#Summary');
        my $ul_containing_fragments =
          $h2->following->first( sub { $_->tag eq 'ul' } );
        if ($ul_containing_fragments) {

            for my $li ( $ul_containing_fragments->find('li')->each ) {
                my $title = $li->at('a')->text;
                my $url   = $link->clone->fragment($title);

=begin
              Taking 's' and 'ms' fragments for /time/ as examples
              their current description appears as below:
              s which represents a time in seconds...
              ms which represents a time in milliseconds...

              We remove the 'which' so that the description appears as:
              s represents a time in seconds...
              Which sounds right.
=cut

                my $paragraph = $li->all_text;
                $paragraph =~ s/\s{1}which//;

                #for /frequency/ and /angle/ the example code
                #is contained in a table
                #for /time/ it is contained in a <pre>
                my $node_with_code = $link =~ /time/ ? 'pre' : 'table';
                my $code_dom = $dom->at('h2#Examples')->following->first(
                    sub {
                        $_->tag eq $node_with_code;
                    }
                ) if $dom->at('h2#Examples');
                my $code;
                if ($code_dom) {
                    if ( $code_dom->tag eq 'pre' ) {
                        $code = $code_dom->all_text;
                    }
                    else {
                        my $tbody = $code_dom->at('tbody');
                        my $trs   = $tbody->find('tr');
                        for my $tr ( $trs->each ) {
                            my $tds = $tr->find('td');
                            $tds->each(
                                sub {
                                    my ( $td, $num ) = @_;
                                    my $td_text = trim( $td->all_text );
                                    $code .= $td_text if $num == 1;
                                    $code .= " $td_text\n" if $num == 2;
                                }
                            );
                        }
                    }
                }
                my $description = create_abstract( $paragraph, $code );
                create_article( $title, $description, $url );
            }
        }
    }
    else {
        warn "Unmatched $link in parse_fragment_data()";
    }
}
