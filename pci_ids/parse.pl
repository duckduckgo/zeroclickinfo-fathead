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
my $i = 0;
foreach my $vendor (@vendors) {
    print ++$i . "\n";
#    print "$vendor->{id} $vendor->{name}\n";
    $queries{$vendor->{id}} = {
        "abstract" =>"<i>Vendor</i>: $vendor->{name}",
        "category" => "<i>PCI vendor ID</i>: $vendor->{id}"
    };
    if (scalar @{$vendor->{devices}} > 0) {
        foreach my $device (@{$vendor->{devices}}) {
#            print "$vendor->{id} $device->{id}"
#                . "$device->{name} $vendor->{name}\n";
            $queries{"$vendor->{id} $device->{id}"} = {
                "abstract" => "<i>Vendor</i>: $vendor->{name}<br>"
                            . "<i>Device</i>: $device->{name}",
                "category" => "PCI vendor ID $vendor->{id}"
            };
            if (scalar @{$device->{subdevices}} > 0) {
                foreach my $subdevice (@{$device->{subdevices}}) {
#                    print "$vendor->{id} $device->{id} "
#                        . "$subdevice->{subvendor} "
#                        . "$subdevice->{subdevice} "
#                        . "$vendor->{name} $device->{name} "
#                        . "$subdevice->{subsystem_name}\n";
                    $queries{"$vendor->{id} $device->{id}"
                            . "$subdevice->{subvendor} "
                            . "$subdevice->{subdevice}"} = {
                                "abstract" => "<i>Vendor</i>:"
                                    . "$vendor->{name}<br>"
                                    . "<i>Device</i>: $device->{name}<br>"
                                    . "<i>Subdevice/subvendor</i>: "
                                    . "$subdevice->{subsystem_name}",
                                "category" => "PCI vendor ID "
                                             . "$vendor->{id}"
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
        $queries{$_}->{category},           # categories
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

