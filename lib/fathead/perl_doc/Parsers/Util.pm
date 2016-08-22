#!/usr/bin/env perl
package Util;
use strict;
use warnings;
use Data::Dumper;
use Exporter qw/import/;
our @EXPORT_OK = qw/get_row trim_abstract/;

sub trim_abstract {
    my($full_abs, $len) = @_;
    
    my @split_abs = split /\s/, $full_abs;
    my @final_abs;
    if(@split_abs >= ($len-1)){
        @final_abs = splice(@split_abs, 0, ($len-1));
        
        foreach my $word (@split_abs){
            my $last;
            
            if($word =~ /[.?!]$/){
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
