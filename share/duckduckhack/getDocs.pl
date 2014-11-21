#!/usr/bin/env perl
use strict;
use warnings;
use IO::All;
package GetSummary;
use base "HTML::Parser";
binmode STDOUT, ":utf8";

my $p = new GetSummary;

my $links = IO::All->new('links.txt');

sub start{
	my($self, $tag, $attr, $attrseq, $origtext) = @_;

	# get the side bar links
	if($tag =~ /^a/i && $attr->{'class'}){
		if($attr->{'class'} =~ /sub-item__link/){
			 $links->print("$attr->{'href'}\n");
		}
	}
}

while(<>) {
	$p->parse($_);
}
