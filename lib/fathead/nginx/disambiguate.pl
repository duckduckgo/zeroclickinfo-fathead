use strict;
use warnings;
use 5.010;

# We buffer up lines that describe the same nginx directive
my @buffer = ();

my $previous_directive = '';

# Dumps an array of lines that have matching directives.
# If there are more than one line in the buffer, individual lines are
# disambiguated by prepending the nginx module name, and a
# disambiguation line is inserted.
sub dump_buffer {
    if (scalar @buffer == 1) {
        # Single line in the buffer. No disambiguation is necessary
        print $buffer[0];
    } elsif (scalar @buffer > 1) {
        # At least two lines in the buffer. We first disambiguate ...

        my $directive;
        my @fields = ();
        my @disambiguation_elements = ();

        foreach my $line (@buffer) {
            @fields = split /	/, $line;

            $directive = $fields[0];

            $fields[11] =~ m!</pre>(.*)$! or
                die "Could not extract description for $line";
            my $description = lcfirst $1;

            $fields[12] =~ m!/ngx_http_([^/]*)_module.html#[^/#]*$! or
                die "Could not extract module for $line";
            my $module = $1;

            # print the disambiguated line
            print $module, ".", $line;

            # preparing content for disambiguation
            push @disambiguation_elements, "*[[$module.$directive]] $description";
        }

        # ... and finally add a disambiguation page.
        my @disambiguation = (q[]) x 13;
        $disambiguation[0] = $directive;
        $disambiguation[1] = 'D';
        $disambiguation[9] = join '\n', sort @disambiguation_elements;
        say join "\t", @disambiguation;
    }

    @buffer = ();
}

# Reading from stdin, and buffering up as long as the directive match.
# Once directives don't match, dump buffer, and start buffering for
# current directive.
while (<>) {
    my $current_directive = (split /	/) [0];

    if ($current_directive ne $previous_directive) {
        dump_buffer;
    }
    push @buffer, $_;

    $previous_directive = $current_directive;
}

# Final dumping, to also get the last directive (not yet dumped) to stdout.
dump_buffer