use 5.010;
use strictures 1;
use autodie;
use Encode;
use HTML::Entities;
use HTML::Parser;
use URI::Escape;
binmode STDOUT, ':encoding(UTF-8)';
chdir 'download/doc.perl6.org/type';
opendir my $dh, '.';

sub duck_escape(_) {
    my ($string) = @_;

    # &#10; is valid HTML and it works in <pre> blocks
    $string = encode_entities( $string, "<>&\t\\" );
    $string =~ s/ \n /\\n/gmsx;
    $string;
}

my %functions;
my %types;
my %methods;

# Only files count, magical directories like '.' shouldn't
my @files = grep {-f && / \A [\w:]+ \z /msx} readdir $dh;
my %files = map { $_ => 1 } @files;
for my $file (@files) {
    my @tags;
    my $current_field;
    my $p;
    my $pre_level = 0; # >0 if we're within a <pre>, == 0 otherwise
    my $need_prototype;
    my $class;
    my $parser = HTML::Parser->new(
        api_version => 3,

        # Broken text could make parsing harder than it should be.
        unbroken_text => 1,
        utf8_mode     => 1,
        start_h       => [
            sub {
                my ($tagname) = @_;
                push @tags, $tagname;
                if ( $tagname eq 'pre' ) {
                    $pre_level++;
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
                    if (   $tags[-2] eq 'h1'
                        || $tags[-2] eq 'h2'
                        || $tags[-3] eq 'h2'
                        || ( $tags[-2] eq 'pre' && !$class ) )
                    {
                        $dtext =~ m{
                            \A
                            (?: Methods | Operators )
                            .* \s+ ( \S+ ) \z
                        }msx;
                        $class = $1 // $class // $file;

                        # Declare new field to store data
                        if ( $tags[-2] ne 'h1' || $1 ) {
                            $current_field = { class => $class };
                            push @{ $types{$file} }, $current_field;
                            $need_prototype = 1;
                            $p = 1;
                        }
                    }

                    # <pre> stores method prototype.
                    if ($need_prototype && $pre_level) {
                        $current_field->{prototype} .= $dtext;
                    }

                    # First paragraph after <h2> is method name.
                    elsif ( $tags[-2] eq 'h2' || $tags[-3] eq 'h2' ) {
                        $dtext =~ s/^.* \s+ ( \S+ ) \z/$1/msx;
                        $current_field->{method} = $dtext;
                        $methods{$class}{$dtext} = $current_field;
                    }

                    # In <p> mode, every text is part of description.
                    elsif ($p) {
                        $dtext =~ s/ \n //gmsx;
                        $current_field->{description} .= $dtext;
                    }

                }
            },
            'dtext'
        ],
        end_h => [
            sub {
                # If current tag is <p> then turn off <p> mode.
                my $ended_tag = pop @tags;
                if ( $ended_tag eq 'p' && $p ) {
                    $p = 0;

                    # Trim description to a single sentence. That is: Find the
                    # first period that is followed by a blank, or the end of
                    # line, but avoid matching “i.e.”, “e.g.”, “..”.
                    $current_field->{description} =~ s/^(.*?(?<!(i\.e|e\.g|..\.))\.)( .*|$)/$1/ms;

                    # Since <pre>s after the first <p> are not prototypes, but
                    # code examples, we force-stop to looking for a prototype
                    # after the first <p>. That way, we avoid picking up an code
                    # example snippets as prototype for prototype-less methods
                    # (like IO's dd).
                    $need_prototype = 0;
                } elsif ( $ended_tag eq 'pre' && $pre_level > 0 ) {
                    if ( $need_prototype && $pre_level == 1 ) {
                        $current_field->{prototype} =~ s/ \n $//msx;
                        $need_prototype = 0;
                    }
                    $pre_level--;
                }
            },
        ],
    )->parse_file($file);
}

# Prepare disambig table
for my $type ( keys %types ) {
    my @definitions = @{ $types{$type} };
    for (@definitions) {
        my %definition = %$_;
        if ( $definition{method} && $definition{method} !~ / \s /msx ) {
            $functions{ $definition{method} }{ $definition{class} }++;
        }
    }
}
for my $type ( sort keys %types ) {
    for ( @{ $types{$type} } ) {
        my %function = %$_;
        # Skip field if field doesn't seem valid.
        next
            if !$function{description} && !$function{prototype}
            || lc $function{description} eq 'methods'
            || $function{class} ne $type && !$function{method}
            || $function{method} && !$functions{ $function{method} };

        my $functions
            = $function{method}
            ? keys %{ $functions{ $function{method} } }
            : 0;

        if ( $function{class} eq $type ) {
            my @line = (q[]) x 13;
            $line[0] = duck_escape $function{class}
                . ( $function{method} ? ".$function{method}" : q[] );
            $line[1]  = 'A';
            $line[4]  = "Perl 6 " . duck_escape $function{class};
            $line[11] = '<section class="prog__container">' . (
                $function{prototype}
                ? '<pre><code>'
                    . duck_escape( $function{prototype} )
                    . '</code></pre>'
                : q[]
            ) . '<p>' . duck_escape $function{description} . '</p></section>';
            $line[12]
                = 'http://doc.perl6.org/type/'
                . uri_escape_utf8($type)
                . (
                $function{method}
                ? '#' . uri_escape_utf8 $function{method}
                : q[]
                );
            say join "\t", @line;

            if ( $function{method} && !$types{ $function{method} } ) {
                if ( $functions == 1 ) {
                    my @redirect = (q[]) x 13;
                    $redirect[0] = duck_escape $function{method};
                    $redirect[1] = 'R';
                    $redirect[2]
                        = duck_escape "$function{class}.$function{method}";
                    say join "\t", @redirect;
                }
                elsif ( $functions > 1 ) {
                    my @disambig = (q[]) x 13;
                    $disambig[0] = duck_escape $function{method};
                    $disambig[1] = 'D';
                    $disambig[9] = join '\n', map {
                        my $description = $methods{$_}{$function{method}}{description};
                        $description =~ s/ \n | (?<= [.!?] \s ) .*? \z //gmsx;
                        duck_escape "*[[$_.$function{method}]], "
                            . lcfirst $description
                    } sort keys %{ $functions{ $function{method} } };
                    say join "\t", @disambig;
                    $functions{ $function{method} } = {};
                }
            }
        }
        else {
            my @redirect = (q[]) x 13;
            $redirect[0] = duck_escape "$type.$function{method}";
            $redirect[1] = 'R';
            $redirect[2] = duck_escape "$function{class}.$function{method}";
            say join "\t", @redirect;
        }
    }
}
