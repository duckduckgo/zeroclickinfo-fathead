#!/usr/bin/env perl

use Text::CSV_XS;
use URI::Escape;
use Algorithm::Combinatorics 'variations';
#use Data::Dumper;

use strict;

# can be overridden on command-line
my $verbose = 0;
my $data = 'download/vehicles.csv.zip';
my $output_file = 'output.txt';

parse_argv();

my $csv = Text::CSV_XS->new() or
    die 'Cannot create Text::CSV_XS parser: ' . Text::CSV_XS->error_diag ();
open my $dfh, "unzip -cq $data |" or die "Failed to open $data: $?";

# wanted columns
my @wanted_cols = qw'city08 highway08 year make model displ cylinders trany tCharger sCharger fuelType1 drive trans_dscr';

# Parse the source file and group all of the configurations for each year/make/model
my (%arts, %rdrs, %dsmb, %hdrs);
while(my $r = $csv->getline($dfh)){
    if(%hdrs){
		my ($city, $hwy, $yr, $make, $model, @specs) = @$r[@hdrs{@wanted_cols}];
        for ($make, $model){
            s/\s*\/\s*/\//g; # remove irregular spaces around alternate models with "/"
            tr/ //s; #remove duplicate spacing
        }
        my $vkey = join(' ', $yr, $make, $model);
		$vkey =~ s/[)(]/"/og;

        if(exists $rdrs{$vkey}){ # e.g. "1991 BMW 325i" and "1991 BMW 325i/325is"
            warn "DUPE: vkey $vkey exists as a redirect. Removing!\n" if $verbose;
            delete $rdrs{$vkey};
        }
        unless(exists $arts{$vkey}{src}){ # the search for the model
            $arts{$vkey}{src} = "http://www.fueleconomy.gov/feg/PowerSearch.do?action=noform&path=1&year1=$yr&year2=$yr&make="
                . uri_escape($make) . '&model=' . uri_escape($model) . '&srchtyp=ymm';
        }
        push @{$arts{$vkey}{city}}, $city;
        push @{$arts{$vkey}{hwy}}, $hwy;

        # basic model configuration info...unique *most* of the time
        my $vconfig = "$specs[0] L, $specs[1] cyl, $specs[2]";
        if($specs[3] eq 'T'){ $vconfig .= ', Turbo'; }
        elsif($specs[4] eq 'S'){ $vconfig .= ', Supercharger'; }

        # The site itself has duplicate descriptions, e.g. see the 1993 Chevy C1500.  Only drilling down
        # into the data further will you find that there is some distinguishing feature; for example,
        # the drive or transmission type.  The latter is probably unintelligible for the average 
        # consumer.  Anyhow, we'll carry these values forward for disambiguation if necessary.
        push @{$arts{$vkey}{configs}{$vconfig}}, [$city, $hwy, @specs[5..7]]; 

        # validate potential redirects
		my $tmpm = $model;
		$tmpm =~ s/[)(]/ /og; # Can't just remove, e.g. 993 Rolls-Royce Turbo R/Turbo R"lwb"
		$tmpm =~ tr/ //s; #remove duplicate spacing we might have created

        # alternate names for models, trims, or completely different models contain a "/"
        # There are several ways this is used.  Since these are just redirects, we can cover
        # all permutations without displaying erroneous models

		my @variations;
        if($model =~ m{/}o){
            my $altmods = generate_altmods($tmpm);
			@variations = @$altmods;
        }
		else{
			@variations = ($tmpm);
		}

		my $min_terms = 2; # minimum number of vehicle terms in a search
		for my $v (@variations){
			my @p = split /\s+/, $v;

			# model variations, allow a single term
			for(my $x = 1;$x <= @p;++$x){ 
				my $iter = variations(\@p, $x);
				while(my $c = $iter->next){
					my $mdl = join(' ', @$c);
					my @p2 = ($mdl, $yr, $make);

					# yr/make/model variations, require two terms
					for(my $y = $min_terms;$y <= @p2;++$y){ 
						my $iter2 = variations(\@p2, $y);
						while(my $c2 = $iter2->next){
							my $r = join(' ', @$c2);
							next unless $r =~ /\b$yr\b/; # force inclusion of year
							if(exists $dsmb{$r}){
								++$dsmb{$r}{$vkey};
							}
							elsif(exists $rdrs{$r}){
								if($rdrs{$r} ne $vkey){
									warn "Ambiguous redirect $r points to multiple vehicles: $rdrs{$r} and $vkey\n" if $verbose;
									warn "DUPE: $r and $vkey are both searchable\n" if $verbose && ($r eq $vkey);
									my $prev = delete $rdrs{$r};
									++$dsmb{$r}{$_} for $prev, $vkey;
								}
							}
							else{ 
								if(exists $arts{$r}){
									warn "DUPE: redirect $r exists for searchable vehicle $vkey\n" if $verbose;
									next;
								}
								$rdrs{$r} = $vkey 
							}
						}
					}
				}
			}
		}
    }
    elsif($. == 1){
        for(my $i = 0;$i< @$r;++$i){
            $hdrs{$r->[$i]} = $i;
        }    

        my $verified_cols = 1;
        # verify the columns are there
        for my $h (@wanted_cols){
            unless(exists $hdrs{$h}){
                warn "Column $h not found";
                $verified_cols = 0;
            }
        }
        die 'Column headers seem to have changed. Verify manually' unless $verified_cols;
    }
    else{
        die 'Failed to extract headers';
    }
}

open my $output, ">$output_file" or die "Failed to open $output_file: $!";

# Output the articles
while(my ($v, $data) = each %arts){
    my ($city, $hwy, $configs, $src) = @$data{qw(city hwy configs src)};
    my $summary;
    # Give city/hwy ranges for multiple configurations
    if((keys %$configs) > 1){ 
        my ($cmin, $cmax) = (sort {$a <=> $b} @$city)[0,-1];
        my ($hmin, $hmax) = (sort {$a <=> $b} @$hwy)[0,-1];
        if( ($cmin == $cmax) && ($hmin == $hmax) ){
            $summary = "$cmin city, $hmin hwy.";    
        }
        else{
            $summary = ($cmin != $cmax ? "$cmin-$cmax" : $cmin) . ' city, ' .
                       ($hmin != $hmax ? "$hmin-$hmax" : $hmin) . ' hwy depending on configuration.';
        }
    }
    else{ $summary = $city->[0] . ' city, ' . $hwy->[0] . ' hwy.' }
    my $rec = "MPG: $summary<br />";

    # add details for configurations
    for my $config (sort keys %$configs){
        my $specs = $configs->{$config};
        my @display_configs;
        if(@$specs > 1){
            for(my $i = 2;$i < @{$specs->[0]};++$i){
                my %feature;
                for my $s (@$specs){
                    ++$feature{$s->[$i]};
                }    
                if( (keys %feature) > 1){ #feature is different for *some* configurations
                    for(my $s = 0;$s < @$specs;++$s){
                        if(defined $display_configs[$s]){
                            $display_configs[$s] .= ', ' . $specs->[$s][$i];
                        }
                        else{
                            push @display_configs, $config . ', ' . $specs->[$s][$i];
                        }
                    }    
                }
            }    
            @display_configs = sort @display_configs;
        }
        else{
            @display_configs = ($config);
        }
        for(my $x = 0;$x < @display_configs;++$x){
            $rec .= '<br />' . $display_configs[$x] . ': ' . $specs->[$x][0] . ' city / ' . $specs->[$x][1] . ' hwy';
        }
    }
    print $output "$v\tA\t\t\t\t\t\t\thttp://www.fueleconomy.gov/ FuelEconomy.gov\\n\t\t\t$rec\t$arts{$v}{src}\n";
}

# Output redirects
while(my ($r, $a) = each %rdrs){
    print $output "$r\tR\t$a\t\t\t\t\t\t\t\t\t\t\n";
}

# Output disambiguations (TODO?)

#warn scalar(keys %dsmb), " ambigous redirects\n", Dumper(\%dsmb);

# Generate the sub/alternate models usually indicated by "/" in the model name.
# May produce a few false positives (probably harmless?)
sub generate_altmods{
    my $m = shift;
	
    my (%altmods, @vars, $pre, $post);
    if( (my @alts = split m{\s*/\s*}, $m) > 1){ # e.g. GS 300/GS 400
		++$altmods{$_} for @alts;
    }

    my @parts = split /\s+/o, $m;
    for my $p (@parts){
        if( (my @v = split '/', $p) > 1){
            if($pre && ($parts[0] =~ /^$v[1]/)){ # e.g. GS 300/GS 400 above 
                warn "Skipping $m\n" if $verbose;
				last;
            }
            if( (my ($l) = $v[0] =~ /^([a-z])+\d+$/oi) && ($v[1] =~ /^\d+/o)){ # e.g. G15/25 Rally 2WD
                @vars = (shift @v);
                for my $n (@v){
                    if($n =~ /^\d+/o){ # cover G15/25/30
                        push @vars, "$l$n";
                    }
                    else{ # cover things like G15/20/25/G30
                        push @vars, $n;
                    }    
                }
            }
            else{ # e.g. TVR 280i/350i Convertible
                @vars = @v;
            }
        }
        elsif(@vars){
          $post .= " $p";
        }
        else{
          $pre .= "$p ";
        }
    }
    for my $v (@vars){
        ++$altmods{"$pre$v$post"};
    }
    return [keys %altmods];
}

# command-line options
sub parse_argv {
    my $usage = <<ENDOFUSAGE;

    *******************************************************************
        USAGE: parse.pl [-data path/to/data] [-output path/to/output]
               [-v]

        -data: (optional) path to the downloaded zip file
        -output: (optional) path to output.txt file
        -v: (optional) Turn on some parse warnings

    *******************************************************************

ENDOFUSAGE

    for(my $i = 0;$i < @ARGV;$i++) {
        if($ARGV[$i] =~ /^-data$/o) { $data = $ARGV[++$i] }
        elsif($ARGV[$i] =~ /^-output$/o) { $output_file = $ARGV[++$i] }
        elsif($ARGV[$i] =~ /^-v$/o) { $verbose = 1; }
    }
}
