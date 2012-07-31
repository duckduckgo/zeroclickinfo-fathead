use 5.006;
use strictures 1;
use autodie;
use Encode;
use HTML::Parser;
use URI::Escape;
binmode STDOUT, ':encoding(UTF-8)';
chdir 'download/doc.perl6.org/routine';
opendir my $dh, '.';

sub duck_escape {
    my %replaces = (
        '\\' => '\\\\',
        "\n" => '\n',
        "\t" => '\t',
    );
    my ($string) = @_;
    # I don't know how exactly escaping works, but I hope that this
    # trick won't do too much damage with tricky data (not like such
    # data is planned, but it's better to be prepared for that).
    $string =~ s{ ( [\n\t] | \\ (?= [\\\n\t] ) ) }{$replaces{$1}}gmsx;
    $string;
}

my @fields;

# Only files count, magical directories like '.' shouldn't
for my $file ( grep {-f} readdir $dh ) {
    my @tags;
    my $current_field;
    my $description;
    my $p;
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
                my ( $dtext ) = @_;
                $dtext = decode 'UTF-8', $dtext;
                if ( @tags > 2 ) {

                    # <h1> stores name of class.
                    if ( $tags[-2] eq 'h1' ) {
                        $current_field = {class => $dtext, method => $file};
                        push @fields, $current_field;
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
            }
        ],
    )->parse_file($file);
}

for my $field (@fields) {
    my %field = %$field;
    print duck_escape($field{class}), '.', duck_escape($field{method}),
          " (Perl 6)\t\thttp://doc.perl6.org/type/",
          uri_escape_utf8($field{class}), '#',
          uri_escape_utf8($field{method}), "\t",
          duck_escape($field{description} || q[]), "\t",
          duck_escape($field{prototype} || q[]), "\t\t\t\n";
}
