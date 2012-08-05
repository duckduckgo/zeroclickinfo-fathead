#!/usr/bin/env perl

use warnings;
use strict;

use Data::Dumper;

open my $fh, '<', 'download/pci.ids';

my @vendors = ();
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
    my $category = scalar @{$vendor->{devices}} > 10 ? 1 : 0;
    $queries{$vendor->{id}} = {
        "abstract" =>"<i>Vendor</i>: $vendor->{name}",
    };
    if (scalar @{$vendor->{devices}} > 0) {
        foreach my $device (@{$vendor->{devices}}) {
            $queries{"$vendor->{id} $device->{id}"} = {
                "abstract" => "<i>Device</i>: $device->{name}<br>"
                            . "<i>Vendor</i>: $vendor->{name}",
                "category" => $category ? "PCI vendor ID $vendor->{id}"
                                : ""
            };
            if (scalar @{$device->{subdevices}} > 0) {
                foreach my $subdevice (@{$device->{subdevices}}) {
                    $queries{"$vendor->{id} $device->{id} "
                            . "$subdevice->{subvendor} "
                            . "$subdevice->{subdevice}"} = {
                                "abstract" => "<i>Subdevice/subvendor</i>: "
                                    . "$subdevice->{subsystem_name}<br>"
                                    . "<i>Device</i>: "
                                    . "$device->{name}<br>"
                                    . "<i>Vendor</i>: $vendor->{name}<br>",
                                "category" => $category ? "PCI vendor ID "
                                             . "$vendor->{id}" : ""
                    };
                }
            }
        }
    }
}

open my $output, '>', 'output.txt';
map {
    print $output join "\t", (
        $_,                                 # title
        "A",                                # type
        "",                                 # redirect
        "",                                 # otheruses
        exists $queries{$_}->{category} ?
            $queries{$_}->{category} : "",  # categories
        "",                                 # references
        "",                                 # see_also
        "",                                 # further_reading
        "",                                 # external_links
        "",                                 # disambiguation
        "",                                 # images
        $queries{$_}->{abstract},           # abstract
        "http://pciids.sourceforge.net/\n"  # source_url
    );
} keys %queries;

