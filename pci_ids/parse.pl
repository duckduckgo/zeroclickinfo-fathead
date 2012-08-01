#!/usr/bin/env perl

use warnings;
use strict;

open my $fh, '<', 'download/pci.ids';

my (%vendors) = ();
my (@devices, @subdevices) = [];
my %key = (
    0 => sub { s/^(....)\ //; $vendors{$1} = $_; },
    1 => sub { s/^(....)\ //; push @devices, { $1 => $_ } },
    2 => sub {
        s/^(....)\ (....)\ //;
        return if not $2;
        push @subdevices, {
            "subvendor" => $1,
            "subsystem_name" => $_ };
    },
);
while (<$fh>) { $key{length $1}->(s/^(\t{0,2})//); }
foreach my $vendor (keys %vendors) {
    print $vendor . " ";
    print keys shift @devices;
    print " -> $vendors{$vendor}";
##        print "\n$vendor ";
##        if (scalar @devices == 1) {
##            print keys @devices;
#        print "device";
#        print scalar @devices;
#        if (scalar @devices == 2) {
#            my $device = shift @devices;
#            print $device;
#        } else { print "\n"; }
##        }
#    #next if scalar @devices == 0;
}
