#!/usr/bin/env perl 
use strict;
use warnings;
# for each man page file, grab Name, and Synopsis if possible

sub strip {
	for ($_[0]) { 
		s/<[a-zA-Z]*>//g;
		s/<\/[a-zA-Z]*>//g;	
		s/[\s\t]*$//g;
		s/^[\s\t]*//g;
	}
	return $_[0];
}

my @cmdlist = `ls download`;
chomp(@cmdlist);
# for each HTML manpage in download/
foreach my $page (@cmdlist) 
{
	open MANPAGE, "download/$page" or die "Cannot open file: $page";
	# print the Name section of each manpage. 
	# if there's a Synopsis, print that too. stop at the next <h2>, or bail if the synopsis is too long. (max 5 lines) 
	my $max = 0;
	my @synopsis = ();
	my ($line, $nextline,$description) = ('','','');
	my $section = $page;
	$page =~ s/[0-9][.]html//;
	$section =~ s/[a-z0-9A-Z]*([0-9])[.]html/$1/;
	# For each line in the manpage,
	while ($line = <MANPAGE>)
	{
		# If you find the name section...
		if ($line =~ m/<h2>Name/i) {
			$nextline = <MANPAGE>;
			while ($nextline =~ /^[\s\t]*$/ || $nextline =~ /^$/) { # skip the blank lines.
				$nextline = <MANPAGE>;
			}
			$description = &strip($nextline);
		}
		# Continuing with the same manpage, If you find the Synopsis section....
		if ($line =~ m/<h2>Synopsis/i) {
			$nextline = <MANPAGE>;
			while (!($nextline =~ m/<h2>/i)) {
				last if ($max > 8);
				$nextline = &strip($nextline);
				if ($nextline =~ /^[\s\t]*$/ || $nextline =~ /^$/) {
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
	# If output is borked somehow and you need it in unicode, uncomment the next line.
	# binmode(STDOUT, ":utf8");
	print "$page\tA\t\t\t\t\t\t\t\t\t\t$description <pre>@synopsis</pre>\t$url\n";
	# print "$page\t\t$url\t$description\t@synopsis\t\t\t\n";
	close (MANPAGE);
}
exit 0;
