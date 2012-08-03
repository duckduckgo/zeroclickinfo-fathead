#!/usr/bin/env perl

use warnings;
use strict;

open my $fh, '<', 'download/pci.ids';

my (@vendors, @devices, @subdevices) = ();
my %key = (
    0 => sub {
        s/^(....)\ //;
        push @vendors, {
            "id" => $1,
            "name" => $_,
            "devices" => [] }
    },
    1 => sub {
        s/^(....)\ //;
        push %{$vendors[-1]}->{devices}, {
            "id" => $1,
            "name" => $_,
            "subdevices" => [] }
    },
    2 => sub {
        s/^(....)\ (....)\ //;
        return if not $2;
        push %{$vendors[-1]}->{devices}[-1]{subdevices}, {
            "subvendor" => $1,
            "subdevice" => $2,
            "subsystem_name" => $_ };
    },
);
while (<$fh>) {
    next if m/^#|^\s+$/;
    $key{length $1}->(s/^(\t{0,2})//);
}
foreach my $vendor (@vendors) {
    if (scalar @{%{$vendor}->{devices}} > 0) {
        foreach my $device (@{%{$vendor}->{devices}}) {
            print %{$vendor}->{id} . " $device->{id}"
                . (chomp $device->{name}) . " " . %{$vendor}->{name};
        }
    } else {
        print %{$vendor}->{id} . " " . %{$vendor}->{name};
    }
}
