#!/usr/bin/env perl

use warnings;
use strict;

open my $fh, '<', 'download/pci.ids';

my (@vendors, @devices, @subdevices) = ();
my %queries = ();
my %key = (
    0 => sub {
        s/^(....)\s+//; chomp;
        push @vendors, {
            "id" => $1,
            "name" => $_,
            "devices" => [] }
    },
    1 => sub {
        s/^(....)\s+//; chomp;
        push $vendors[-1]{devices}, {
            "id" => $1,
            "name" => $_,
            "subdevices" => [] }
    },
    2 => sub {
        s/^(....)\ (....)\s+//; chomp;
        return if not $2;
        push $vendors[-1]{devices}[-1]{subdevices}, {
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
#    print "$vendor->{id} $vendor->{name}\n";
    $queries{$vendor->{id}} = "Vendor: $vendor->{name}";
    if (scalar @{$vendor->{devices}} > 0) {
        foreach my $device (@{$vendor->{devices}}) {
#            print "$vendor->{id} $device->{id}"
#                . "$device->{name} $vendor->{name}\n";
            $queries{"$vendor->{id} $device->{id}"} =
                "Vendor: $vendor->{name}<br>"
                . "Device: $device->{name}";
            if (scalar @{$device->{subdevices}} > 0) {
                foreach my $subdevice (@{$device->{subdevices}}) {
#                    print "$vendor->{id} $device->{id} "
#                        . "$subdevice->{subvendor} "
#                        . "$subdevice->{subdevice} "
#                        . "$vendor->{name} $device->{name} "
#                        . "$subdevice->{subsystem_name}\n";
                    $queries{"$vendor->{id} $device->{id}"
                            . "$subdevice->{subvendor} "
                            . "$subdevice->{subdevice} "} =
                                "Vendor: $vendor->{name}<br>"
                                . "Device: $device->{name}<br>"
                                . "Subdevice/subvendor: "
                                . "$subdevice->{subsystem_name}";
                }
            }
        }
    }
}

open my $output, '>', 'output.txt';
map {
    print $output join "\t", (
        "$_",                               # title
        "A",                                # type
        "",                                 # redirect
        "",                                 # otheruses
        "",                                 # categories
        "",                                 # references
        "",                                 # see_also
        "",                                 # further_reading
        "",                                 # external_links
        "",                                 # disambiguation
        "",                                 # images
        "$queries{$_}",                     # abstract
        "http://pciids.sourceforge.net/\n"    # source_url
    );
} keys %queries;

