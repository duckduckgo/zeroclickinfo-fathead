#!/usr/bin/env perl 
use strict;
use warnings;
use encoding 'utf8';
use Cwd qw(chdir);
# for each man page file, grab Name, and Synopsis if possible
chdir 'download/man';

# initialize some loop variables
my ($page, $section, @sections); 
@sections = `ls`;
chomp(@sections);
$page = $section = '';

# Parsing subroutines
sub parse_name {
	my $str = $_[0];
	for ($str) { 
		s/<[a-zA-Z]*>//g;
		s/<\/[a-zA-Z]*>//g;	
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

# for each HTML manpage in downloads/man/<section>
foreach $section (@sections) {
	my @cmdlist = `ls $section`;
	foreach $page (@cmdlist) 
	{
		chomp($page);
		open MANPAGE, "$section/$page" or die "Cannot open file: $section/$page";
		# print the Name section of each manpage. 
		# if there's a Synopsis, print that too. stop at the next <h2>, or bail if the synopsis is too long. (max 5 lines) 
		# I imposed the 5 line limit for the Synopsis because some commands have a REALLY long one, 
		# and it wouldn't be suitable for a fathead plugin.

		my ($line, $nextline, $description, $max, @synopsis);
		$line = $nextline = $description = '';
		$max = 0;
		@synopsis = ();
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
				$description = &parse_name($nextline);
			}

			# Continuing with the same manpage, If you find the Synopsis section....
				if ($line =~ m/<h2>Synopsis/) {
					$nextline = <MANPAGE>;
				if ($nextline =~ m/^[\s\t]*$/ || $nextline =~ m/^$/) {
					while ($nextline =~ m/^[\s\t]*$/ || $nextline =~ m/^$/) {
						$nextline = <MANPAGE>; # skip blank lines
					}
				}
				while (!($nextline =~ m/<h2>/)) {
					last if ($max > 5);
					$nextline = &parse_syn($nextline);
					if ($nextline =~ m/^[\s\t]*$/) {
						next;
					}
					push(@synopsis, $nextline);
#					$nextline = <MANPAGE>;
					$max++;
				} continue {
					$nextline = <MANPAGE>;
				}
			} 
		}
		my $url="http://linux.die.net/man/$section/$page";
		print "$page\t\t$url\t$description\t@synopsis\t\t\t\n";
		close (MANPAGE);
	}
}
exit 0;
