use 5.010;
use strictures 1;
use autodie;
use Encode;
use HTML::Entities;
use HTML::Parser;
use URI::Escape;
use HTML::TagParser;

binmode STDOUT, ':encoding(UTF-8)';
chdir 'download/docs.perl6.org/type';
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

my @body_tags = ("pod-body\n\n", "pod-body\n no-toc\n");
my $func;

$func->{"pod-body\n\n"} = sub {
    my ($file, $pod_body) = @_;
    my $preprocess;
    my $class = $file;
    my $index = -1;
    for my $elem (@{ $pod_body->childNodes() }) {
	if ($elem->tagName eq 'h1' && defined($elem->innerText)) {
	    $elem->innerText =~ m{
                            \A
                            (?: Methods | Operators )
                            .* \s+ ( \S+ ) \z
                        }msx;
	    $class = $1 // $class // $file;
	}
	if ($elem->tagName eq 'h2') {
	    $index++;
	    $preprocess->[$index]->{class} = $class;
	    $preprocess->[$index]->{anchor} = $elem->id;
	    $preprocess->[$index]->{method} = $elem->innerText;
	    $preprocess->[$index]->{method} =~ s/^.* \s+ ( \S+ ) \z/$1/msx;
	}
	if ($elem->tagName eq 'pre' && $index >= 0) {
	    if (not exists($preprocess->[$index]{prototype})) {
		$preprocess->[$index]->{prototype} = $elem->innerText;
		$preprocess->[$index]->{prototype} =~ s/ \n $//msx;
	    }
	}
	if ($elem->tagName eq 'p' && $index >= 0) {
	    next if ($elem->innerText =~ / Defined \s+ as \: /ix);
	    next if (exists($preprocess->[$index]->{description}));
	    $preprocess->[$index]->{description} .= $elem->innerText;
	    $preprocess->[$index]->{description} =~ s/ \n //gmsx;
	    $preprocess->[$index]->{description} =~ s/^(.*?(?<!(i\.e|e\.g|..\.))\.)( .*|$)/$1/ms;
	}
    }
    return $preprocess;
};

$func->{"pod-body\n no-toc\n"} = sub {
    my ($file, $pod_body) = @_;
    my $preprocess;
    my $index = 0;
    for my $elem (@{ $pod_body->childNodes() }) {
	$preprocess->[$index]->{class} = $file;
	$preprocess->[$index]->{anchor} = $file;
	$preprocess->[$index]->{method} = $file;
	$preprocess->[$index]->{method} =~ s/^.* \s+ ( \S+ ) \z/$1/msx;
	if ($elem->tagName eq 'pre') {
	    if (not exists($preprocess->[$index]{prototype})) {
		$preprocess->[$index]->{prototype} = $elem->innerText;
		$preprocess->[$index]->{prototype} =~ s/ \n $//msx;
	    }
	}
	if ($elem->tagName eq 'p') {
	    next if ($elem->innerText =~ / Defined \s+ as \: /ix);
	    next if (exists($preprocess->[$index]->{description}));
	    $preprocess->[$index]->{description} .= $elem->innerText;
	    $preprocess->[$index]->{description} =~ s/ \n //gmsx;
	    $preprocess->[$index]->{description} =~ s/^(.*?(?<!(i\.e|e\.g|..\.))\.)( .*|$)/$1/ms;
	}
    }
    return $preprocess;
};

for my $file (@files) {
    my $html = HTML::TagParser->new($file);
    for my $body_tag (@body_tags) {
	my $pod_body = $html->getElementsByClassName($body_tag);
	next if (not defined($pod_body));
	my $preprocess = $func->{$body_tag}->($file, $pod_body);
	for my $elem (@{ $preprocess }) {
	    $methods{$elem->{class}}{$elem->{method}} = { class => $elem->{class},
							  method => $elem->{method},
							  prototype => $elem->{prototype},
							  description => $elem->{description},
							  anchor => $elem->{anchor}};
	    push @{ $types{$file} }, $methods{$elem->{class}}{$elem->{method}};
	}
    }
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
                = 'https://docs.perl6.org/type/'
                . uri_escape_utf8($type) . '.html'
                . (
                $function{method}
                ? '#' . uri_escape_utf8 $function{anchor}
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
