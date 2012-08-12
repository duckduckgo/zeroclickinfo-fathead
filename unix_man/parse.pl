#!/usr/bin/env perl
use Cwd qw(chdir);
#for each directory, for each file, grab Name and Synopsis
#
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
#		print "NAME:$data_file\n";
			$nextline = <DATA>;
			if (!($nextline =~ /^[\s]*$/)) {
				$nextline =~ s/<[a-z]*>//g;
				$nextline =~ s/<\/[a-z]*>//g;	
				$nextline =~ s/[\s]+[-]+[\s]+/\t/g;	
				print "$nextline";
			} else {
				while ($nextline =~ /^[\s]*$/) {
					$nextline = <DATA>;
				}
				$nextline =~ s/<[a-z]*>//g;
				$nextline =~ s/<\/[a-z]*>//g;	
				$nextline =~ s/[\s]+[-]+[\s]+/\t/g;	
				print "$nextline";
				break;
			}
		}
	}
}
close (DATA);
