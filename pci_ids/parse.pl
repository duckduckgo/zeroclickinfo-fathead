#!/usr/bin/env perl

use warnings;
use strict;

open my $fh, '<', 'download/pci.ids';

my (%vendors, %devices, %subdevices) = ();
my %key = (
    0 => sub { s/^(....)\ //; $vendors{$1} = $_; },
    1 => sub { s/^(....)\ //; $devices{$1} = $_; },
    2 => sub {
        s/^(....)\ (....)\ //;
        return if not $2;
        $subdevices{"2"} = {
            "subvendor" => $1,
            "subsystem_name" => $_ };
    },
);
while (<$fh>) { 
    next if /^#|^\s*$/;
    $key{length $1}->(s/^(\t{0,2})//);
    foreach my $vendor (keys %vendors) {
        print "$vendor -> $vendors{$vendor}";
    }
}
