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

# there's gotta be a better/regex-y way of doing this...especially for the perldeltas
my @discard = qw ( perlfork perlapio perlartistic perldelta perlfunc perlmodstyle perlvar perl5005delta perl561delta perl56delta perl570delta perl571delta perl5004delta perl572delta perl573delta perl581delta perl582delta perl583delta perl584delta perl585delta perl58delta perlport );

my @builtins = qw( alias bg bind break builtin cd command compgen complete continue declare dirs disown enable eval exec exit export fc getopts hash help history jobs let local local logout popd pushd read readonly return set shift shopt source suspend times trap type typeset ulimit umask unalias unset wait fg ); 

my %discard = map { $_ => 1 } @discard;
my %builtins = map { $_ => 1 } @builtins;
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
	if (exists $discard{$page})	{
		close($manpage);
		next;
	}	       
	if (exists $builtins{$page})	{ 
		while ($line = decode('utf8', <$manpage>, Encode::FB_QUIET)) {
			if ($line =~ m/^[\s]*<B>$page/ && ($line =~ m/.*\[+/ || $line =~ m/<I>/) ) {
				$line =~ s/^[\s\t]+//;
				chomp($line);
				$line =~ s/$/<br \/>/;
				$line =~ s/^[\s]*//;
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
		if ($line =~ m/<h2>Name/i || $line =~ m/NAME[\s\t]*$/) {
			$nextline = <$manpage>;
			while ($nextline =~ /^[\s\t]*$/ || $nextline =~ /^$/) { # skip the blank lines.
				$nextline = <$manpage>;
			}
				while ($nextline !~ m/<h2>/i && $nextline !~ m/^[\s\t]+$/ && $nextline !~ m/^$/) {
				$description .= " " . &strip($nextline);
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
				$nextline =~ s/[\s]*$/<br \/>/;
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
	print "$page\tA\t\t\t\t\t\t\t\t\t\t";
	print "$description" if ($description);
	# print automatically interpolates @synopsis and adds spaces. I don't want spaces, so join() was used.
	print "<pre><code>" . join("", @synopsis) . "</code></pre>" if (@synopsis);
	print "\t$url\n";
	close ($manpage);
}
