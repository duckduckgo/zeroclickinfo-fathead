#!/usr/bin/env perl
package Util;
use strict;
use warnings;
use Data::Dumper;
use Exporter qw/import/;
our @EXPORT_OK = qw/get_row trim_abstract/;

# return tab formatted row for fathead output.txt file
sub get_row {
    my ($title, $abs, $url, $type) = @_;
    $type = 'A' unless $type;
    return "$title\t$type\t\t\t\t\t\t\t\t\t\t$abs\t$url";
}

sub trim_abstract {
    my($full_abs, $len) = @_;
    
    my @split_abs = split /\s/, $full_abs;
    my @final_abs;
    if(scalar @split_abs >= 249){
        @final_abs = splice(@split_abs, 0, 249);
        
        foreach my $word (@split_abs){
            my $last;
            
            if($word =~ /\.$|\?$|\!$/){
                $last = 1;
            }
            push(@final_abs, $word);
            
            last if $last;
        }
    }else{
        @final_abs = @split_abs;
    }

    return join ' ', @final_abs;
}
1;
