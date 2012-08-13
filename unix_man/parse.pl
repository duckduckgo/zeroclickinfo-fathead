#!/usr/bin/env perl 
use encoding 'utf8';
use strict;
use warnings;
use Cwd qw(chdir);
# for each man page file, grab Name, and Synopsis if possible
chdir 'downloads';
my @cmdlist = `ls`; 
my $cmd;
sub parse_name {
	my $str = $_[0];
	for ($str) { 
		s/<[a-zA-Z]*>//g;
		s/<\/[a-zA-Z]*>//g;	
		s/[\s]+[-]+[\s]+/\t/g;	
		s/[\s\t]*$//g;
	}
	return $str;
}
sub parse_syn {
	my $str = $_[0];
	for ($str) {
		s/[\n\t]//g;
		s/<[a-zA-Z\s"'!-]*>//g;
		s/<[0-9a-zA-Z:_"=\/-;\s!.]*>//g;
		s/<[!a-zA-Z_=";0-9.\/:\s-]*//g;
		s/[a-zA-Z_=";0-9.\/:\s-]*>//g;
		s/<\/[a-z]*>//g;	
		s/[\s\t]*$//g;
		s/google.*//g; # brute forcing google ads....
	}
	return $str;
}

# for each HTML manpage in downloads/
foreach $cmd (@cmdlist) 
{
	open MANPAGE, $cmd or die "Cannot open file: $cmd";
	# print the Name section of each manpage. 
	# if there's a Synopsis, print that too. stop at the next <h2>, or bail if the synopsis is too long. (max 5 lines) 
	# I imposed the 5 line limit for the Synopsis because some commands have a REALLY long one, 
	# and it wouldn't be suitable for a fathead plugin.

	# For each line in the manpage,
	my ($line, $nextline, $name, $max, @synopsis);
	$line = $nextline = $name = '';
	$max = 0;
	@synopsis = ();
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
			if ($nextline =~ m/^[\s\t]*$/ || $nextline =~ m/^$/) {
				while ($nextline =~ m/^[\s\t]*$/ || $nextline =~ m/^$/) {
					$nextline = <MANPAGE>;
				}
			}
				while (!($nextline =~ m/<h[2-3]>/)) {
					last if ($max > 5);
					$nextline = &parse_syn($nextline);
					if ($nextline =~ m/^[\s\t]*$/) {
						next;
					}
					push(@synopsis, $nextline);
					$nextline = <MANPAGE>;
					$max++;
				} continue {
					$nextline = <MANPAGE>;
				}
			} 
	}
	print "$name\t@synopsis\n";
	close (MANPAGE);
}
exit 0;
