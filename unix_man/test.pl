#!/usr/bin/env perl

use Cwd qw(chdir);
chdir downloads;

open DATA, "audit2allow" or die "can't open!";
while ($line = <DATA>)
{
if ($line =~ m/<h2>Name/) {
		$nextline = <DATA>;
		if (!($nextline =~ /^[\s]*$/)) {
			print "before:$nextline";
			$nextline =~ s/<[a-z]*>//g;
			$nextline =~ s/<\/.*>//g;	
			print "after:$nextline";
		} else {
			while ($nextline =~ /^$/) {
				$nextline = <DATA>;
			}
			$nextline =~ s/<[a-z]*>//g;
			$nextline =~ s/<\/.*>//g;	
		#	$nextline =~ s/<\/[a-z]*>//g;	
			print $nextline;
			break;
			}
		}
	}
$string = '<b><p>hey there</b></p>';
$string =~ s/<[a-z]*>//g;
$string =~ s/<\/[a-z]*>//g;
print $string;
