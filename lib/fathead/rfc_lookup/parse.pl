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
        my $categories = "RFCs from " . $hr_rfc->{'date'}->{'year'};
	
        my $dateString = $hr_rfc->{'date'}->{'month'} . " of " . $hr_rfc->{'date'}->{'year'};
        my $titleString = "\"" . $hr_rfc->{'title'} . "\"";
        
        my @authors = ($hr_rfc->{'author'}->{'name'}) ? $hr_rfc->{'author'}->{'name'} : keys %{$hr_rfc->{'author'}};
        my $authorString = "";
       
        # For debugging
        
        # print Dumper($hr_rfc->{'author'}, @authors);

        my $i = 0;
        
        foreach my $author (@authors) {
            if (!$i) {
                $authorString = $author;
            }
            elsif ($i == @authors - 1) {
                $authorString .= ", and " . $author;
            }
            else {
                $authorString .= ", " . $author;
            }
            $i++;
        }

        my $obsoletes = substr($hr_rfc->{'obsoletes'}->{'doc-id'}, 3) + 0 if $hr_rfc->{'obsoletes'}->{'doc-id'};
        my $obsoletedBy = substr($hr_rfc->{'obsoleted-by'}->{'doc-id'}, 3) + 0 if $hr_rfc->{'obsoleted-by'}->{'doc-id'};
        
        my $obsString = "";

        if ($obsoletes && $obsoletedBy) {
            $obsString = "It obsoletes <a href=\"/?q=RFC+$obsoletes\">RFC $obsoletes</a> and is obsoleted by <a href=\"/?q=RFC+$obsoletedBy\">RFC $obsoletedBy</a>. ";
        }
        elsif ($obsoletes) {
            $obsString = "It obsoletes <a href=\"/?q=RFC+$obsoletes\">RFC $obsoletes</a>. ";
        }
        elsif ($obsoletedBy) {
            $obsString = "It is obsoleted by <a href=\"/?q=RFC+$obsoletedBy\">RFC $obsoletedBy</a>. "
        }


	my $type = 'A';

	my $details = "$titleString was released in $dateString";

        $details .= " by $authorString" if $authorString;

        $details .= ". ";

        $details .= $obsString;

        
        # TODO this parsing is fast/loose and doesn't handle missing/extra values
	# needs to check each part more carefully!
	
	$details .= $hr_rfc->{'abstract'}->{'p'}->[0];
	
        # Hack to remove "\0B" from RFC 6403. It causes problems in the database
        # and probably shouldn't be there in the first place
        $details =~ s/\\0B//g if $number == 6403;

	my $url = "http://www.rfc-editor.org/rfc/rfc$number.txt";
	
	print "$number\t$type\t\t\t$categories\t\t\t\t\t\t\t$details\t$url\n";

}
# and that's all the news.
