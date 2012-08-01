#!/usr/bin/env perl

use warnings;
use strict;

open my $fh, '<', 'download/pci.ids';

my (@vendors, @devices, @subdevices) = [];
my %key = (
    0 => sub { s/^(....)\ //; push @vendors, { "id" => $1, "name" => $_ } },
    1 => sub { s/^(....)\ //; push @devices, { "id" => $1, "name" => $_ } },
    2 => sub {
        s/^(....)\ (....)\ //;
        return if not $2;
        push @subdevices, {
            "subvendor" => $1,
            "subsystem_name" => $_ };
    },
);
while (<$fh>) {
    next if m/^#|^\s+$/;
    $key{length $1}->(s/^(\t{0,2})//)
}
my $line = 0;
foreach my $vendor (@vendors) {
    next if ++$line == 1;
    print %{$vendor}->{id} . " ";
    print %{$vendor}->{name} . "\n";
}
