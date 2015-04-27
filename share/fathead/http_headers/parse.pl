#!/usr/bin/env perl
use warnings;
use strict;
use utf8;
use Encode;
use JSON;
use Web::Query;

main();

sub main {
    my %headers = (
        index_hash_by_header(
            split_sections(
                    load_contents('download/request.json')
                  . load_contents('download/response.json')
            )
        )
    );

    write_output('output.txt', %headers);
}

sub write_output {
    my ($filename, %headers) = @_;

    open my $fh, '>', $filename or die $!;

    print $fh join "\n", format_output(%headers);

    close $fh or die $!;
}

sub format_output {
    my (%headers) = @_;
    my @lines;

    for my $title (sort keys %headers) {
        my %header_types = %{ $headers{$title} };

        if (keys %header_types == 1) {
            push @lines, format_article($title, values %header_types);
        }
        else {
            push @lines, format_disambiguation($title, %header_types);
        }

        push @lines, format_redirects($title);
    }

    return @lines;
}

sub format_redirects {
    return redirect_space2dash(@_),
           redirect_headers_suffix(@_);
}

sub redirect_headers_suffix {
    my ($name) = @_;

    my @header_suffix = (
        "$name header",         # title
        'R',                    # type
        $name,                  # redirect
        '',                     # ignore
        '',                     # categories
        '',                     # ignore
        '',                     # related topics
        '',                     # ignore
        '',                     # external links
        '',                     # ignore
        '',                     # image
        '',                     # abstract
        '',                     # source_url
    );

    my @headers_suffix = (
        "$name headers",        # title
        'R',                    # type
        $name,                  # redirect
        '',                     # ignore
        '',                     # categories
        '',                     # ignore
        '',                     # related topics
        '',                     # ignore
        '',                     # external links
        '',                     # ignore
        '',                     # image
        '',                     # abstract
        '',                     # source_url
    );

    return join("\t", @header_suffix),
           join("\t", @headers_suffix);
}

sub redirect_space2dash {
    my ($name) = @_;

    my $with_dashes = $name;

    my $with_spaces = $name;
    $with_spaces =~ s/-/ /g;

    return if $with_spaces eq $with_dashes;

    my @fields = (
        $with_spaces,           # title
        'R',                    # type
        $with_dashes,           # redirect
        '',                     # ignore
        '',                     # categories
        '',                     # ignore
        '',                     # related topics
        '',                     # ignore
        '',                     # external links
        '',                     # ignore
        '',                     # image
        '',                     # abstract
        '',                     # source_url
    );

    return join("\t", @fields);
}

sub format_article {
    my ($name, $value) = @_;

    my @fields = (
        $name,                  # title
        'A',                    # type
        '',                     # redirect
        '',                     # ignore
        '',                     # categories
        '',                     # ignore
        '',                     # related topics
        '',                     # ignore
        '',                     # external links
        '',                     # ignore
        '',                     # image
        format_abstract($value->{description}, $value->{example}),  # abstract
        'https://en.wikipedia.org/wiki/List_of_HTTP_header_fields', # source_url
    );

    return join("\t", @fields);
}

sub format_disambiguation {
    my ($name, %types) = @_;

    my @fields = (
        $name,                  # title
        'D',                    # type
        '',                     # redirect
        '',                     # ignore
        '',                     # categories
        '',                     # ignore
        '',                     # related topics
        '',                     # ignore
        '',                     # external links
        format_each_disambiguation(%types),  # disambig
        '',                     # image
        '',                     # abstract
        'https://en.wikipedia.org/wiki/List_of_HTTP_header_fields', # source_url
    );

    return join("\t", @fields);
}

sub format_each_disambiguation {
    my (%types) = @_;
    my @disamb;

    for my $t (keys %types) {
        my $description = $types{$t}{description};
        my $example     = $types{$t}{example};

        if (substr($description, -1) ne '.') {
            $description .= '.';
        }

        # Remove "See HTTP Compression", or "see below", or just "(below)"
        $description =~ s/[,\.]\s+see\s+[a-zA-Z ]+\././ig;
        $description =~ s/\s\(below\)//g;

        # Capitalize the first word
        $description = ucfirst($description);

        my $str_example = join "\\n\\n", @$example;

        push @disamb, "*[[$t]],$description<br><pre><code>$str_example</code></pre>\\n";
    }

    return join '', @disamb;
}

sub format_abstract {
    my ($description, $example) = @_;

    if (substr($description, -1) ne '.') {
        $description .= '.';
    }

    # Remove "See HTTP Compression", or "see below", or just "(below)"
    $description =~ s/[,\.]\s+see\s+[a-zA-Z ]+\././ig;
    $description =~ s/\s\(below\)//g;

    # Capitalize the first word
    $description = ucfirst($description);

    my $str_example = join "\\n\\n", @$example;

    return "$description<br><pre><code>$str_example</code></pre>";
}

sub split_sections {
    my ($content) = @_;
    my %sections;
    my $current;

    wq("<body>$content</body>")->contents->each(sub {
        my ($i, $el) = @_;

        if ($el->tagname =~ m/^h\d$/) {
            $current = $el->find('.mw-headline')->text;
        }
        elsif ($el->tagname eq 'table') {
            $sections{$current} = html_table2hash($el);
        }
        else {
            # skip any other elements
        }
    });

    return %sections;
}

sub index_hash_by_header {
    my (%sections) = @_;
    my %result;

    for my $section (keys %sections) {
        my $new_name = $section;
        $new_name =~ s/field(s)?//;
        $new_name = trim($new_name);
        for my $header (keys %{ $sections{$section} }) {
            $result{$header} //= {};
            $result{$header}{$new_name} = $sections{$section}{$header};
        }
    }

    return %result;
}

sub html_table2hash {
    my ($table) = @_;

    my %result;
    my @headers;

    $table->find('th')->each(sub {
        my ($i, $el) = @_;
        push @headers, lc trim($el->text);
    });

    $table->find('tr')->each(sub {
        my ($i, $row) = @_;
        my %current;

        $row->find('td')->each(sub {
            my ($j, $cell) = @_;
            $cell->find('.reference-text,.reference')->remove();

            if ($headers[$j] eq 'example') {
                $current{$headers[$j]} = parse_example($cell);
            }
            else {
                $current{$headers[$j]} = trim(join '', $cell->text);
            }
        });

        if (my $name = fix_name(\%current)) {
            $result{$name} = \%current;
        }
    });

    return \%result;
}

sub parse_example {
    my ($example) = @_;

    my @result;

    $example->find('p')->each(sub {
        my ($i, $p) = @_;

        push @result, trim($p->text);
        $p->remove;
    });

    $example->find('li')->each(sub {
        my ($i, $li) = @_;
        my $t = $li->text;
        $t =~ s/Example \d://g;

        push @result, trim($t);
        $li->remove;
    });

    $example->find('ul,ol')->remove();

    my $text = trim($example->text || '');

    if ($text) {
        unshift @result, $text;
    }

    return \@result;
}

sub load_contents {
    my ($filename) = @_;

    local $/;
    open my $fh, '<', $filename or die $!;
    my $contents = <$fh>;
    close $fh or die $!;

    my $json = decode_json($contents);

    my ($page) = keys %{ $json->{query}{pages} };

    return encode_utf8($json->{query}{pages}{$page}{revisions}[0]{'*'});
}

sub fix_name {
    my ($ref) = @_;
    my ($name) = grep { m/\bname$/ } keys %$ref;

    return wq('<div>' . delete($ref->{$name}) . '</div>')->text if $name;
}

sub trim {
    my ($text) = @_;

    $text =~ s/^\s+//;
    $text =~ s/\s+$//;

    return $text;
}
