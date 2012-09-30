#!/usr/bin/env perl

use strict;
use warnings;

use HTML::TokeParser;
use feature qw{ switch };

my @files = <download/*>;
foreach my $file (@files) {
    print "\n$file ----------------\n\n";
    my ($manufacturer, $model, $protocol, $username, $password);
    my $html = HTML::TokeParser->new($file);
    my $i = 0;
    while ($html->get_tag("td")) {
        given ($i) {
            when (0) { $manufacturer = $html->get_trimmed_text("/td"); }
            when (1) { $model = @html->get_tag("/td")[0]; }
            when (2) { $protocol = $html->get_trimmed_text("/td"); }
            when (3) { $username = $html->get_tag("/td"); }
            when (4) { $password = $html->get_trimmed_text("/td"); }
        }
        if ($i == 4) {
            print "manufacturer: $manufacturer\n";
            print "model: $model\n";
            print "protocol: $protocol\n";
            print "username: $username\n";
            print "password: $password\n\n";
            $manufacturer = $model = $protocol = $username = $password = '';
            $i = 0;
        } else { $i++; }
    }

}
