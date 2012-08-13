#!/usr/bin/env perl 
use encoding "utf8";
use Cwd qw(chdir);
# for each man page file, grab Name, and Synopsis if possible
chdir downloads;
@cmdlist = `ls`; 

sub parse_name {
	$string = $_[0];
	$string =~ s/<[a-zA-Z]*>//g;
	$string =~ s/<\/[a-zA-Z]*>//g;	
	$string =~ s/[\s]+[-]+[\s]+/\t/g;	
	$string =~ s/[\s\t]*$//g;
	return $string;
}
sub parse_syn {
	$string = $_[0];
	$string =~ s/[\n\t]//g;
	$string =~ s/<[a-zA-Z\s"'!-]*>//g;
	$string =~ s/<[0-9a-zA-Z:_"=\/-;\s!.]*>//g;
	$string =~ s/<[!a-zA-Z_=";0-9.\/:\s-]*//g;
	$string =~ s/[a-zA-Z_=";0-9.\/:\s-]*>//g;
	$string =~ s/<\/[a-z]*>//g;	
	$string =~ s/[\s\t]*$//g;
	$string =~ s/google.*//g; # brute forcing google ads....
	return $string;
}
# for each HTML manpage in downloads/
foreach $cmd (@cmdlist) 
{
	open MANPAGE, "$cmd" or die "Cannot open file: $cmd";
	# print the Name section of each manpage. 
	# if there's a Synopsis, print that too. stop at the next <h2>, or bail if the synopsis is too long. (max 5 lines) 
	# I imposed the 5 line limit for the Synopsis because some commands have a REALLY long one, 
	# and it wouldn't be suitable for a fathead plugin.

	# For each line in the manpage,
	while ($line = <MANPAGE>)
	{
		# If you find the name section...
		if ($line =~ m/<h2>Name/) {
			$nextline = <MANPAGE>;
			if ($nextline =~ /^[\s]*$/) {
				while ($nextline =~ /^[\s\t]*$/) { # skip the blank lines.
					$nextline = <MANPAGE>;
				}
			}
				$name = &parse_name($nextline);
		}

		# Continuing with the same manpage, If you find the Synopsis section....
		if ($line =~ m/<h2>Synopsis/) {
			$nextline = <MANPAGE>;
			$max = 0;
			if ($nextline =~ m/^[\s]*$/) {
				while ($nextline =~ m/^[\s\t]*$/) {
					$nextline = <MANPAGE>;
				}
			}
				@synopsis = (&parse_syn($nextline)); # "initialize" array
				while (!($nextline =~ m/<h[2-3]>/)) {
					last if ($max > 5);
					push(@synopsis, &parse_syn($nextline));
					$nextline = <MANPAGE>;
					$max++;
				}

			} 
	}
	print "$name\t@synopsis\n";
	close (MANPAGE);
}
exit 0;
