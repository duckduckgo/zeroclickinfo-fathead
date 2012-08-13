#!/usr/bin/env perl
use Cwd qw(chdir);


# for each man page file, grab Name and Synopsis if possible

use encoding "utf8";
chdir downloads;
@cmdlist = `ls`;


foreach $item (@cmdlist) 
{
	my $data_file = $item;
	open DATA, "$data_file" or die "can't open $data_file $!";
	# print the name. if there's a synopsis, print that too. stop at the next <h2> 
	while ($line = <DATA>)
	{
		if ($line =~ m/<h2>Name/) {
			$nextline = <DATA>;
			if (!($nextline =~ /^[\s]*$/)) {
				$nextline =~ s/<[a-zA-Z]*>//g;
				$nextline =~ s/<\/[a-zA-Z]*>//g;	
				$nextline =~ s/[\s]+[-]+[\s]+/\t/g;	
				$name = $nextline;
			} else {
				while ($nextline =~ /^[\s]*$/) {
					$nextline = <DATA>;
				}
				$nextline =~ s/<[a-z]*>//g;
				$nextline =~ s/<\/[a-z]*>//g;	
				$nextline =~ s/[\s]+[-]+[\s]+/\t/g;	
				$name = $nextline;
			}	       
		}

		if ($line =~ m/<h2>Synopsis/) {
			$nextline = <DATA>;
			if (!($nextline =~ /^[\s]*$/)) {
				chomp($nextline);
				$nextline =~ s/[\n\t]//g;
				$nextline =~ s/<[a-zA-Z\s"'!-]*>//g;
				$nextline =~ s/<[0-9a-zA-Z:_"=\/-;\s!.]*>//g;
				$nextline =~ s/<[!a-zA-Z_=";0-9.\/:\s-]*//g;
				$nextline =~ s/[a-zA-Z_=";0-9.\/:\s-]*>//g;
				$nextline =~ s/<\/[a-z]*>//g;	
				$nextline =~ s/google.*//g;
				@synopsis = ($nextline);
				$nextline = <DATA>;
				$max = 0;
				while (!($nextline =~ m/<h2>/)) {
					last if ($max > 5);
					chomp($nextline);
					$nextline =~ s/[\t\n]//g;
					$nextline =~ s/<[a-z]*>//g;
					$nextline =~ s/<[0-9a-zA-Z:_"=\/-;\s!.]*>//g;
					$nextline =~ s/<[!a-zA-Z_=";0-9.\/:\s-]*//g;
					$nextline =~ s/[a-zA-Z_=";0-9.\/:\s-]*>//g;
					$nextline =~ s/google.*//g;
					$nextline =~ s/<\/[a-z]*>//g;	
					push(@synopsis, $nextline);
					$nextline = <DATA>;
					$max++;
					
				}
			} else {
				while ($nextline =~ /^[\s\t]*$/) {
					$nextline = <DATA>;
				}
				chomp($newline);
				$nextline =~ s/[\t\n]//g;
				$nextline =~ s/<[a-zA-Z\s"'!-\/_;=`]*>//g;
				$nextline =~ s/<[0-9a-zA-Z:_"=\/-;\s!.]*>//g;
				$nextline =~ s/<[!a-zA-Z_=";0-9.\/:\s-]*//g;
				$nextline =~ s/[a-zA-Z_=";0-9.\/:\s-]*>//g;
				$nextline =~ s/<\/[a-zA-Z]*>//g;	
				$nextline =~ s/google.*//g;
				@synopsis = ($nextline);
				$nextline = <DATA>;
				$max = 0;
				while (!($nextline =~ m/<h[2-3]>/)) {
					last if ($max > 5);
					chomp($nexline);
					$nextline =~ s/<[a-z]*>//g;
					$nextline =~ s/[\n\t]//g;
					$nextline =~ s/<[0-9a-zA-Z:_"=\/-;\s!.]*>//g;
					$nextline =~ s/<[!a-zA-Z_=";0-9.\/:\s-]*//g;
					$nextline =~ s/[a-zA-Z_=";0-9.\/:\s-]*>//g;
					$nextline =~ s/<\/[a-z]*>//g;	
					$nextline =~ s/google.*//g;
					push(@synopsis, $nextline);
					$nextline = <DATA>;
					$max++;
					
				}
				
			} 
		}
			
	}
	chomp($name);
	$name =~ s/^[\s\t\n]+//g;
	$name =~ s/[\s\t\n]+$//g;
	#for ($i = 0; $i < $#synopsis + 1; $i++) {	
	#	if (@synopsis[$i] =~ m/^[\s\t]*$/) {
	#		splice(@synopsis,$i,1);
	#		$i = $i - 1;
	#	}
	#	if (@synopsis[$i] =~ m/^$/) {
	#		splice(@synopsis,$i,1);
	#		$i = $i - 1;
	#	}
	#}
	chomp(@synopsis);
	print "$name\t@synopsis\n";
}

close (DATA);
exit 0;
