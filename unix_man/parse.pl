#!/usr/bin/env perl 
use strict;
use warnings;
use encoding 'utf8';
# for each man page file, grab Name, and Synopsis if possible

# Parsing subroutines
sub parse_name {
	my $str = $_[0];
	for ($str) { 
		s/<[a-zA-Z]*>//g;
		s/<\/[a-zA-Z]*>//g;	
		s/[\s\t]*$//g;
		s/^[\s\t]*//g;
	}
	return $str;
}
sub parse_syn {
	my $str = $_[0];
	for ($str) {
		s/[\n\t]//g;
		s/<[a-zA-Z\s"'!-]*>//g;
		s/<[0-9a-zA-Z:_"=\/-;\s!.]*>//g;
#		s/<[!a-zA-Z_=";0-9.\/:\s-]*//g;
#		s/[a-zA-Z_=";0-9.\/:\s-]*>//g;
		s/<\/[a-z]*>//g;	
		s/[\s\t]*$//g;
		s/^[\s\t]*//g;
	}
	return $str;
}
my @cmdlist = `ls download`;
chomp(@cmdlist);
# for each HTML manpage in downloads/linuxcommand/
	foreach my $page (@cmdlist) 
	{
		open MANPAGE, "download/$page" or die "Cannot open file: $page";
		# print the Name section of each manpage. 
		# if there's a Synopsis, print that too. stop at the next <h2>, or bail if the synopsis is too long. (max 5 lines) 
		# I imposed the 5 line limit for the Synopsis because some commands have a REALLY long one, 
		# and it wouldn't be suitable for a fathead plugin.
		my ($line, $nextline, $description, $max);
		$max = 0;
		my @synopsis = ();
		$line = $nextline = $description = '';
		my $section = $page;
		$page =~ s/[0-9][.]html//g;
		$section =~ s/[a-z0-9A-Z]*([0-9])[.]html/$1/;
		# For each line in the manpage,
		while ($line = <MANPAGE>)
		{
			# If you find the name section...
			if ($line =~ m/<h2>Name/i) {
				$nextline = <MANPAGE>;
				if ($nextline =~ /^[\s]*$/) {
					while ($nextline =~ /^[\s\t]*$/) { # skip the blank lines.
						$nextline = <MANPAGE>;
					}
				}
				$description = &parse_name($nextline);
			}

			# Continuing with the same manpage, If you find the Synopsis section....
				if ($line =~ m/<h2>Synopsis/i) {
					$nextline = <MANPAGE>;
				if ($nextline =~ m/^[\s\t]*$/ || $nextline =~ m/^$/) {
					while ($nextline =~ m/^[\s\t]*$/ || $nextline =~ m/^$/) {
						$nextline = <MANPAGE>; # skip blank lines
					}
				}
				while (!($nextline =~ m/<h2>/i)) {
					last if ($max > 8);
					$nextline = &parse_syn($nextline);
					if ($nextline =~ m/^[\s\t]*$/ || $nextline =~ m/^$/) {
						next;
					}
					$nextline =~ s/$/<br \/>/;
					push(@synopsis, $nextline);
					$max++;
				} continue {
					$nextline = <MANPAGE>;
				}
				$synopsis[-1] =~ s/<br \/>//g if @synopsis >= 1;
				last;
			}
		}
		my $url="http://linuxcommand.org/man_pages/$page".$section.'.html';
		print "$page\t\t$url\t$description\t@synopsis\t\t\t\n";
		close (MANPAGE);
	}
exit 0;
