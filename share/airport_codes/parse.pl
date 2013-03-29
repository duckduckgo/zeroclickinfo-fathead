#!/usr/bin/perl

use strict;
use autodie;

use JSON qw( decode_json );

open my $fout, ">", "output.txt";
binmode $fout, ':encoding(UTF-8)';

open my $fdata, "<", "download/data";
binmode $fdata, ":encoding(UTF-8)";
# has only 1 line
my $json = <$fdata>;
close $fdata;

my $decdata = decode_json( $json );

foreach my $e_ref ( @{ $decdata } ) {
	#say $e_ref->{code}." - Airport ".$e_ref->{name}.", ".$e_ref->{location};
	print $fout "Airport Code\tA\t\t\tAirport Codes\\nIATA\\n\t\t\t\thttps://en.wikipedia.org/wiki/List_of_airports_by_IATA_code Wikipedia Page\t\t\t";
	print $fout $e_ref->{code}." - ".$e_ref->{name}.", ".$e_ref->{location}."\thttps://github.com/jbrooksuk/JSON-Airports/\n";
}

close $fout;
