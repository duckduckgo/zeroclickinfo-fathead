use 5.010;
use strictures 1;
use autodie;
use Encode;
use HTML::Entities;
use HTML::Parser;
use URI::Escape;
binmode STDOUT, ':encoding(UTF-8)';
chdir 'download/doc.perl6.org/routine';
opendir my $dh, '.';

sub duck_escape(_) {
    my ($string) = @_;

    # &#10; is valid HTML and it works in <pre> blocks
    $string = encode_entities( $string, "<>&\t\\" );
    $string =~ s/ \n /<br>/gmsx;
    $string;
}

sub duck_line {
    join "\t", @_;
}

my %functions;

# Only files count, magical directories like '.' shouldn't
for my $file ( grep {-f} readdir $dh ) {
    my @tags;
    my $current_field;
    my $description;
    my $p;
    $functions{$file} = [];
    my $parser = HTML::Parser->new(
        api_version => 3,

        # Broken text could make parsing harder than it should be.
        unbroken_text => 1,
        utf8_mode     => 1,
        start_h       => [
            sub {
                my ($tagname) = @_;
                push @tags, $tagname;
                if ( $p && $tagname eq 'p' ) {
                    $description = q[];
                }
            },
            'tagname'
        ],
        text_h => [
            sub {
                my ($dtext) = @_;
                $dtext = decode 'UTF-8', $dtext;
                if ( @tags > 2 ) {

                    # <h1> stores name of class.
                    if ( $tags[-2] eq 'h1' ) {
                        $current_field = { class => $dtext };
                        push $functions{$file}, $current_field;
                    }

                    # First paragraph after <h2> is description.
                    elsif ( $tags[-2] eq 'h2' ) {
                        $p = 1;
                    }

                    # <pre> stores method prototype.
                    elsif ( $tags[-1] eq 'pre' && $current_field->{class} ) {
                        $current_field->{prototype} ||= $dtext;
                    }

                    # In <p> mode, every text is part of description.
                    elsif ($p) {
                        $description .= $dtext;
                    }
                }
            },
            'dtext'
        ],
        end_h => [
            sub {

                # If current tag is <p> then turn off <p> mode.
                if ( pop @tags eq 'p' && $p ) {
                    $current_field->{description} = $description;
                    undef $description;
                    $p = 0;
                }
            },
        ],
    )->parse_file($file);
}

for my $function ( keys %functions ) {
    my @definitions = @{ $functions{$function} };
    for (@definitions) {
        my %definition = %$_;
        my $code
            = $definition{prototype}
            ? '<pre><code>'
            . duck_escape( $definition{prototype} )
            . '</pre></code>'
            : q[];
        say duck_line(
            "$definition{class}.$function",
            'A',
            q[],
            q[],
            "Perl 6 $definition{class}",
            q[],
            q[],
            q[],
            q[],
            q[],
            q[],
            $code . duck_escape $definition{description},
            'http://doc.perl6.org/type/'
                . uri_escape_utf8( $definition{class} ) . '#'
                . uri_escape_utf8($function),
        );
    }
    if ( @definitions == 1 ) {
        my %definition = %{ $definitions[0] };
        say duck_line( $function, 'R', "$definition{class}.$function",
            q[], q[], q[], q[], q[], q[], q[], q[], q[], );
    }
    else {
        say duck_line( $function, 'D', q[], q[], q[], q[], q[], q[], q[],
            join( '\n', map {"[[$_->{class}.$function]]"} @definitions ),
            q[], q[], q[], );
    }
}
