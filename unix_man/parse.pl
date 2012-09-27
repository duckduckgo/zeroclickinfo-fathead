#!/usr/bin/env perl 
use strict;
use warnings;
use encoding 'utf8';
use Encode;
# for each man page file, grab Name, and Synopsis if possible

sub strip {
	my $line = shift;
	$line =~ s/<[a-zA-Z]*>|<\/[a-zA-Z]*>|[\s\t]*$|^[\s\t]*//g;
	return $line; 
}

my @builtins = qw( alias bg bindbreak builtin cd command compgen complete continue declaredirs disown enable eval exec exit export fc getopts hash help history jobs let local local logout popd pushd read readonly return set shift shopt source suspend times trap type typeset ulimit umask unalias unset wait fg ); 
my %builtins;
foreach (@builtins) {
	$builtins{$_} = 1;
}
my @cmdlist = `ls download`;
chomp(@cmdlist);
# for each HTML manpage in download/
foreach my $page (@cmdlist) 
{
	open my $manpage, "download/$page" or die "Cannot open file: $page";
	# print the Name section of each manpage. 
	# if there's a Synopsis, print that too. stop at the next <h2>, or bail if the synopsis is too long. (max 5 lines) 
	my $max = 0;
	my @synopsis = ();
	my ($line, $nextline,$description) = ('','','');
	my $section = $page;
	$page =~ s/[0-9][.]html//;
	$section =~ s/[a-z0-9A-Z]*([0-9])[.]html/$1/;
	if (exists $builtins{$page})	{
		while ($line = encode('utf8', <$manpage>)) {
			if ($line =~ m/^[\s]*<B>$page/ && ($line =~ m/.*\[+/ || $line =~ m/<I>/) ) {
				$line =~ s/^[\s\t]+//;
				chomp($line);
				$line =~ s/$/<br \/>/;
				push(@synopsis, $line);
				$line = <$manpage>;
				while ($line =~ m/<B>$page/ || $line =~ m/\[+/) {
					chomp($line); 
					$line =~ s/^[\s\t]+//;
					$line =~ s/$/<br \/>/;
					push(@synopsis, $line);
					$line = <$manpage>;
				}
				last;

			}
		}
		
	} else {
	# For each line in the manpage,
	while ($line = <$manpage>)
	{
		# If you find the name section...
		if ($line =~ m/<h2>Name/i || $line =~ m/NAME/) {
			$nextline = <$manpage>;
			while ($nextline =~ /^[\s\t]*$/ || $nextline =~ /^$/) { # skip the blank lines.
				$nextline = <$manpage>;
			}
				while ($nextline !~ m/<h2>/i && $nextline !~ m/^[\s\t]+$/ && $nextline !~ m/^$/) {
				$description .= &strip($nextline);
				last if !($nextline = <$manpage>);
				}
		}
		# Continuing with the same manpage, If you find the Synopsis section....
		if ($line =~ m/<h2>Synopsis/i || $line =~ m/SYNOPSIS/) {
			$nextline = <$manpage>;
			while ($nextline =~ /^[\s\t]*$/ || $nextline =~ /^$/) { # skip the blank lines.
				$nextline = <$manpage>;
			}
			while ($nextline !~ m/<h2>/i && $nextline !~ m/^[\s\t]+$/ && $nextline !~ m/^$/) {
				last if ($max > 8);
				$nextline = strip($nextline);
				if ($nextline =~ /^[\s\t]*$/ || $nextline =~ /^$/) {
					next;
				}
				$nextline =~ s/[\s][\s]*/ /g;
				$nextline =~ s/$/<br \/>/;
				push(@synopsis, $nextline);
				$max++;
			} continue {
				$nextline = <$manpage>;
			}
			last;
		}
	}
}
	next if (!$description && !@synopsis);	
	$synopsis[-1] =~ s/<br \/>//g if @synopsis >= 1;
	my $url="http://linuxcommand.org/man_pages/$page".$section.'.html';
	# If output is borked somehow and you need it in unicode, uncomment the next line.
	binmode(STDOUT, ":utf8");
	print "$page\tA\t\t\t\t\t\t\t\t\t\t";
	print "<pre><code>@synopsis</code></pre>" if (@synopsis);
	# print "$page\t\t$url\t$description\t@synopsis\t\t\t\n";
	print "$description\t" if ($description);
	print "$url\n";
	close ($manpage);
}
