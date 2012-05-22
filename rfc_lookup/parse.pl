#!/usr/bin/perl

use strict;
use warnings;

use XML::Simple;
use Data::Dumper; # testing only

my $hr_docs = XMLin('-', ForceArray => qr/^p$/); # read stdin, let the shell do the IO, since it's there
my @rfcs = @{$hr_docs->{ 'rfc-entry' }};

foreach my $hr_rfc ( @rfcs ) {

	my $id = $hr_rfc->{ 'doc-id' };
	my $number = substr( $id, 3 ) + 0;
	my $printable = "RFC $number";
	
	my $type = 'A';
	
	my $details = $hr_rfc->{'title'} . ", released "
					 . $hr_rfc->{'date'}->{'month'} ." ". $hr_rfc->{'date'}->{'year'};
	# TODO this parsing is fast/loose and doesn't handle missing/extra values
	# needs to check each part more carefully!
	
	$details .= $hr_rfc->{'abstract'}->{'p'}->[0];
	
	# TODO, incorporate "obsoleted by" as further reading link
	
	my $url = "http://www.rfc-editor.org/rfc/rfc$number.txt";
	
	print "$printable\t$type\t\t\t\t\t\t\t\t\t\t$details\t$url\n";

}
# and that's all the news.
