#!/usr/bin/perl

use v5.14;

use strict;
use warnings;
use autodie;
use File::Slurp;

sub getParticleTypes;
sub setCommonNames;
sub getNumberUnitIndex;
sub getNumberToUnit;
sub getComposition;
sub getQuantumNumbers;
sub getLink;
sub setSpecialLinks;

my @data = read_file( "download/mass_width_2012.mcd", binmode => ":encoding(utf-8)");

my %particles;

my @units = qw/eV keV MeV GeV TeV PeV/;

################################################################################
# get particle names and mc numbers
################################################################################
getParticleTypes( "download/pdg_elem.tex", 7 );
getParticleTypes( "download/pdg_meson.tex", 7 );
getParticleTypes( "download/pdg_baryon.tex", 8 );

setCommonNames();
setSpecialLinks();

################################################################################
# get mass and width
################################################################################
foreach ( @data ) {
	next if /^\*/;
	chomp;
	my @entry = unpack( "A1A8A8A8A8xA15xA8xA8xA21" );
	s/^\s+// foreach ( @entry );
	my ( $valname, $mn, $mn2, $mn3, $mn4, $value, $errplus, $errminus, $name ) = @entry;
	($name, my $tmpcharges) = split /\s+/, $name;
	my @charges = split /,/, $tmpcharges;
	push my (@mcnumbers), $mn, $mn2, $mn3, $mn4;

	foreach my $i ( 0..$#charges ) {
		my $errpname = $valname."errplus";
		my $errmname = $valname."errminus";
		my $partref = \%{ $particles{$mcnumbers[$i]} };
		$partref->{charge} = $charges[$i];
		$partref->{$valname} = $value;
		$partref->{name2} = $name;
		$partref->{$errpname} = $errplus =~ s/[\+\-]//r;
		$partref->{$errmname} = $errminus =~ s/[\+\-]//r;
		$partref->{mwentry} = 1;
	}
}

################################################################################
# write to output.txt
################################################################################
open my $fout, ">", "output.txt";
binmode $fout, ":encoding(utf-8)";
foreach my $key ( keys %particles ) {
	my $pr = \%{ $particles{$key} };
	next if not $pr->{name};
	#
	# TODO ?
	# now: accept only particles that have entries in mass_width_2012... other are not yet measured, only postulated, ecc.
	#      EXCEPTION: neutrinos -- mass limit measured though, but not included in data... 
	next unless ( $pr->{mcnumber} ~~ [qw/12 14 16 25 35 36/] || $pr->{mwentry} );
	for( $pr->{type} ) {
		$pr->{shorttpye} = "Diquarks" when /^Diquarks$/;
		$pr->{category} = "Mesons" when /Mesons/;
		$pr->{category} = "Baryons" when /Baryons/;
		$pr->{category} = "Bosons" when /Bosons/;
		$pr->{category} = "Pentaquarks" when /^Pentaquarks$/;
		$pr->{category} = "Susyparticles" when /^supersymmetric/i;
		when (/^Quarks and Leptons$/) {
			for( $pr->{name} ) {
				when (/^[udcstb]$/) { $pr->{category} = "Quarks" }
				default             { $pr->{category} = "Leptons" }
			}
		}
		default { $pr->{category} = "Special particles" };
	}

	# FIXME
	# accept all types later
	next unless $pr->{category} ~~ [qw/Quarks Leptons Bosons Mesons Baryons/];

	$pr->{shorttype} = $pr->{category} =~ s/s$//r;
	
	my $title = $pr->{name};
	# remove latex \ escape
	$title =~ s/\\//g;
	my $categories = "Particles\\n".$pr->{category};
	my $see_also = "[[Particle physics]]";
	my $external_links = "[http://pdg.lbl.gov Particle Data Group]";
	my $dispname = $pr->{cname} ? $pr->{cname} : $pr->{name};
	my $mass;
	if( $pr->{M} ) {
		my ( $massunit, $massstring ) = getNumberUnitIndex( $pr->{M} );
		#my $merror = ($pr->{Merrplus} == $pr->{Merrminus}) ? " +-".$pr->{Merrplus} : " +".$pr->{Merrplus}." -".$pr->{Merrminus};
		my $merror = ($pr->{Merrplus} == $pr->{Merrminus}) ? " \x{00B1} ".getNumberToUnit($pr->{Merrplus}, $massunit) : " + ".getNumberToUnit($pr->{Merrplus}, $massunit)." - ".getNumberToUnit($pr->{Merrminus}, $massunit);
		$mass = $massstring.$merror." ".$units[$massunit].", ";
	} else {
		$mass = "not measured, ";
	}

	################################################################################
	# construct abstract
	my $abstract = "Properties of ".$dispname.":, ";
	$abstract .= "category: ".$pr->{category}.", ";
	$abstract .= "charge: ".$pr->{charge}.", ";
	$abstract .= "mass: ".$mass;
	if( $pr->{category} ne "Quarks" && $pr->{category} ne "Leptons" ) {
		# particle has a width
		my $width = "";
		if( $pr->{W} ) {
			my ( $widthunit, $widthstring ) = getNumberUnitIndex( $pr->{W} );
			my $werror = ($pr->{Werrplus} == $pr->{Werrminus}) ? " \x{00B1} ".getNumberToUnit($pr->{Werrplus}, $widthunit) : " + ".getNumberToUnit($pr->{Werrplus}, $widthunit)." - ".getNumberToUnit($pr->{Werrminus}, $widthunit );
			
			$width = "width: ".$widthstring.$werror." ".$units[$widthunit].", ";
		};
		$abstract .= $width;
	}
	$abstract .= getQuantumNumbers( $pr->{mcnumber}, $pr->{category} ).", ";
	$abstract .= "mcnumber: ".$pr->{mcnumber}.", ";
	
	# get link
	$abstract .= getLink( $pr ).", ";
	
	# get composition
	$abstract .= getComposition( $pr->{mcnumber}, $pr->{category} );

	# abstract done
	################################################################################


	# FIXME
	# is this the correct source_url?
	my $source_url = "http://pdg.lbl.gov/2012/html/computer_read.html";
	print $fout join "\t", (
		$title,
		'A',
		'',
		'',
		$categories,
		'',
		$see_also,
		'',
		$external_links,
		'',
		'',
		$abstract,
		$source_url,
		"\n"
	);
	################
	# redirects
	################
	# particle^{star, charged, ...}
	if( $title =~ m/[\^\{\}]/ ) {
		my $alt = $title =~ s/[\^\{\}]//gr;
		print $fout join "\t", (
			$alt,
			'R',
			$title,
			'',
			'',
			'',
			'',
			'',
			'',
			'',
			'',
			'',
			'',
			"\n"
		);
	}
	# make 1 entry leaving _ in, 1 w/o
	# particle^{star, charged, ...}
	if( $title =~ m/[\^\{\}_]/ ) {
		my $alt = $title =~ s/[\^\{\}_]//gr;
		print $fout join "\t", (
			$alt,
			'R',
			$title,
			'',
			'',
			'',
			'',
			'',
			'',
			'',
			'',
			'',
			'',
			"\n"
		);
	}
	# common names
	print $fout join "\t", (
		$pr->{cname},
		'R',
		$title,
		'',
		'',
		'',
		'',
		'',
		'',
		'',
		'',
		'',
		'',
		"\n"
	) if exists $pr->{cname};
	
	# mcnumbers
	foreach ("mcnumber ", "idhep ") {
		print $fout join "\t", (
			$_.$pr->{mcnumber},
			'R',
			$title,
			'',
			'',
			'',
			'',
			'',
			'',
			'',
			'',
			'',
			'',
			"\n"
		);
	}
}


close $fout;


################################################################################
#                    END OF MAIN                                               #
################################################################################

################################################################################
# getParticleTypes
# ----------------
#         args: filename
#               ncols: number of columns:
#                      meson / elem:  7
#                      baryon:        8
#                      default value: 7
#
sub getParticleTypes
{
	my $fname = shift || die "No filename for getParticleTypes() specified";
	my $ncols = shift || 7;

	my $allowednames = qr(a-zA-Z\-=0-9\\\$, );

	my @dataelems = read_file( $fname, binmode => ":encoding(utf-8)" );
	my @test = grep { /^\\multicolumn\{$ncols\}\{\|c\@\{\\tstrut\}\|\}\{[$allowednames]+\}/.../^\\end\{tabular\}$/ } @dataelems;
 	my @datatypes = split /\\end\{tabular\}\n+/, join "", @test;

	foreach my $line ( @datatypes ) {
		my @tmp = split /\n/, $line;
		my $ptype = $tmp[0] =~ s/^\\multicolumn\{$ncols\}\{\|c\@\{\\tstrut\}\|\}\{([$allowednames]+)\}.*/$1/ir;
		$ptype =~ s/^ +//;
		
		# remove first two lines and last one
		shift @tmp; shift @tmp; #pop @tmp;
		foreach ( @tmp ) {
			chomp;
			s/ *[\\]+ [\\hline ]+$//;
			my ($pname, $p06, $qcontent);
			if( $ncols == 7 ) { 
				( $pname, my ($p96, $p98, $p00, $p02, $p04), $p06 ) = split /\s*&\s*/;
			} elsif( $ncols == 8 ) {
				( $pname, $qcontent, my( $p96, $p98, $p00, $p02, $p04), $p06 ) = split /\s*&\s*/;
			}
			# gluon is 21 (9)... 21 is used in data..., 9 is for special EvtGenerators
			$p06 = "21" if $p06 eq "21 (9)";
			$pname =~ s/\$//g;
			$pname =~ s/\\footnotemark\[[0-9]+\]//g;
			$particles{$p06} = {
				name		=> $pname,
				mcnumber	=> $p06,
				type		=> $ptype
			};
			
			# is it neutrino?
			if( $p06 ~~ [qw/12 14 16/] ) {
				$particles{$p06}{charge} = "0";
			}
		}
	}
}

sub setCommonNames
{
	$particles{2}{cname} = "up";
	$particles{2}{short} = "u";
	$particles{2}{link} = "light-quarks";
	$particles{1}{cname} = "down";
	$particles{1}{short} = "d";
	$particles{1}{link} = "light-quarks";
	$particles{3}{cname} = "strange";
	$particles{3}{short} = "s";
	$particles{3}{link} = "light-quarks";
	$particles{4}{cname} = "charm";
	$particles{4}{short} = "c";
	$particles{4}{link} = "c-quark";
	$particles{5}{cname} = "bottom";
	$particles{5}{short} = "b";
	$particles{5}{link} = "b-quark";
	$particles{6}{cname} = "top";
	$particles{6}{short} = "t";
	$particles{6}{link} = "t-quark";

	$particles{11}{cname} = "electron";
	$particles{11}{link} = "electron";
	$particles{13}{cname} = "muon";
	$particles{13}{link} = "muon";
	$particles{15}{cname} = "tau";
	$particles{15}{link} = "tau";

	$particles{12}{cname} = "electron neutrino";
	$particles{12}{link} = "neutrino-prop";
	$particles{14}{cname} = "muon neutrino";
	$particles{14}{link} = "neutrino-prop";
	$particles{16}{cname} = "tau neutrino";
	$particles{16}{link} = "neutrino-prop";

	$particles{21}{cname} = "gluon";
	$particles{21}{link} = "gluon";
	$particles{22}{cname} = "photon";
	$particles{22}{link} = "photon";
	$particles{23}{cname} = "Z";
	$particles{23}{link} = "z-boson";
	$particles{24}{cname} = "W";
	$particles{24}{link} = "w-boson";

	$particles{25}{cname} = "higgs";
	$particles{35}{cname} = "higgs";
	$particles{36}{cname} = "higgs";
	$particles{25}{link} = "higgs-boson";
	$particles{35}{link} = "higgs-boson";
	$particles{36}{link} = "higgs-boson";

	$particles{211}{cname} = "pion";
	$particles{211}{link} = "pi-plus-minus";
	$particles{111}{cname} = "pion";
	$particles{111}{link} = "pi-zero";
	$particles{221}{cname} = "eta";
	$particles{221}{link} = "eta";
	$particles{213}{cname} = "rho";
	$particles{113}{cname} = "rho";

	$particles{310}{cname} = "K short";
	$particles{310}{link} = "K-zero-S";
	$particles{130}{cname} = "K long";
	$particles{130}{link} = "K-zero-L";
	$particles{311}{cname} = "kaon";
	$particles{311}{link} = "K-zero";
	$particles{321}{cname} = "kaon";
	$particles{321}{link} = "K-plus-minus";

	$particles{443}{cname} = "J/Psi";
	$particles{443}{link} = "J-psi-1S";

	$particles{511}{cname} = "B";
	$particles{511}{link} = "B-zero";
	$particles{521}{link} = "B-plus-minus";

}

sub getNumberUnitIndex
{
	my $number = shift;
	# number is given in GeV
	$number *= 1E9;

	my $c = 0;

	while( $number >= 1000 ) {
		$number /= 1000;
		$c++;
	}
	$c = $#units if $c > $#units;

	return ( $c, $number );
}

sub getNumberToUnit
{
	my $number = shift;
	my $index = shift;

	#my $divisor = ($index > 0) ? 10 ** (3*$index) : 1;
	my $divisor = 10 ** ( 3*$index );

	return $number * 1E9 / $divisor;
}

sub getComposition
{
	my $number = shift;
	my $cat = shift;

	return "" if $cat ne "Mesons" and $cat ne "Baryons";

	if( $cat eq "Mesons" ) {
		my $q1 = substr( $number, -2, 1 );
		my $q2 = substr( $number, -3, 1 );
		return "composition: (".$particles{$q1}{short}.$particles{$q2}{short}."), ";
	} else {
		my $q1 = substr( $number, -2, 1 );
		my $q2 = substr( $number, -3, 1 );
		my $q3 = substr( $number, -4, 1 );
		return "composition: (".$particles{$q1}{short}.$particles{$q2}{short}.$particles{$q3}{short}."), ";
	}
	return "";
}

sub getQuantumNumbers
{
	my $number = shift || 0;
	my $cat = shift || "";

	my $spin = "";
	my $nj = substr( $number, -1, 1 );

	if( $cat eq "Leptons" or $cat eq "Quarks" ) {
		return "spin: 1/2";
	} elsif( $cat eq "Bosons" ) {
		for( $number ) {
			when ( [21..24] ) { $spin = "0" }					# g, gamma, W, Z
			default         { $spin = "not measured" }
		}
		return "spin: ".$spin;	
	} elsif( $cat eq "Mesons"  ) {
		if( length( $number ) == 3 ) {
			#only J:
			$spin = $nj%2 ? ($nj - 1)/2 : ($nj - 1)."/2";
			return "spin: ".$spin;
		} else {
			my $nl = substr( $number, -5, 1 );
			my ( $j, $l, $s, $p, $c ) = ( -1, -1, -1, 0, 0 );
			$j = ($nj - 1)/2;
			for( $nl ) {
				when( 0 ) { $l = $j - 1; 
							$s = 1; 
							$p = -1 ** $j; 
							$c = $p
						};
				when( 1 ) { $l = $j; 
							$s = 0;
							$p = -1 ** ($j+1);
							$c = -$p;
						};
				when( 2 ) { $l = $j; 
							$s = 1;
							$p = -1 ** ($j+1); 
							$c = $p
						};
				when( 3 ) { $l = $j + 1;
							$s = 1 ;
							$p = -1 ** $j; 
							$c = $p
						};
			}
			# special case
			if( $j == 0 ) {
				$l = $nl; $s = $l;
				if( $nl == 0 ) {
					$p = -1;
					$c = 1;
				} elsif( $nl == 1 ) {
					$p = 1;
					$c = 1;
				}
			}
			$spin = $nj%2 ? ($nj - 1)/2 : ($nj - 1)."/2";
			my $ret = "(L,S) = (".$l.",".$s."), J^{PC} = ".$j."^{";
			$ret .= ($p > 0) ? "+" : "-";
			$ret .= ($c > 0) ? "+" : "-";
			$ret .= "}";
			return $ret;
		}
	} elsif( $cat eq "Baryosn" ) {
		my $nj = substr( $number, -1, 1 );
		$spin = $nj%2 ? ($nj - 1)/2 : ($nj - 1)."/2";
		return "spin: ".$spin;
	}
}

sub getLink
{
	my $pr = shift;

	my $link = "link http://pdg.lbl.gov/2012/listings/rpp2012-list-";
	if( $pr->{link} ) {
		$link .= $pr->{link}.".pdf";
	} elsif( $pr->{category} eq "Baryons" ) {
		$link = "";
	}else {
		# particle names follow this pattern...
		# maybe there's an easier way with advanced regex knowledge? ;)
		$pr->{name} =~ m/([a-z]+)(?:_\{?([0-9a-z]+)\}?)?(?:\^\{?(prime)?([ \*0\+\-]+)?\}?)?(?:\(([0-9]+)\))?/i;
	    #                | name ||      subscript     |||    mode       |      up        |||      mass
		my $name = $1 || '';
		my $sub  = defined $2 ? $2 : '';
		my $mod  = defined $3 ? $3 : '';
		my $up   = defined $4 ? $4 : '';
		my $mass = defined $5 ? $5 : '';

		$up =~ s/ //g;
		my $star = "";
		my $charge = "";
		if( substr($up, 0, 1) eq "*" ) {
			$star = "star";
			$up = substr $up, 1;
		}
		if( $up eq "0" ) {
			$charge = "zero";
		} elsif( $up eq "+" ) {
			$charge = "plus-minus";
		}

		$link .= $name.$sub;
		$link .= "-".$mod if $mod;
		$link .= "-".$star if $star;
		$link .= "-".$mass if $mass;
		$link .= "-".$charge if $charge;
		$link .= ".pdf";
	}
	return $link;
}

sub setSpecialLinks
{
	# link names do not follow a certain scheme, sometimes, charged and neutral 
	# versions of the same meson are in the same file, eg B*
	# -> do it manually

	$particles{30113}{link} = "rho-1700";
	$particles{20323}{link} = "K1-1400";
	$particles{10441}{link} = "chi-c0-1P";
	$particles{325}{link} = "K2-star-1430";
	$particles{100313}{link} = "K-star-1410";
	$particles{100441}{link} = "eta-c-2S";
	$particles{523}{link} = "B-star";
	$particles{100323}{link} = "K-star-1410";
	$particles{119}{link} = "a4-2040";
	$particles{100443}{link} = "psi-2S";
	$particles{9010111}{link} = "pi-1800";
	$particles{10213}{link} = "b1-1235";
	$particles{113}{link} = "rho-770";
	$particles{10553}{link} = "h-b-1P";
	$particles{535}{link} = "Bs2-star-5840-zero";
	$particles{30553}{link} = "upsilon-1D";
	$particles{9010213}{link} = "pi1-1600";
	$particles{10211}{link} = "a0-1450";
	$particles{9000113}{link} = "pi1-1400";
	$particles{213}{link} = "rho-770";
	$particles{100113}{link} = "rho-1450";
	$particles{100555}{link} = "chi-b2-2P";
	$particles{445}{link} = "chi-c2-1P";
	$particles{329}{link} = "K4-star-2045";
	$particles{525}{link} = "B2-star-5747-zero";
	$particles{441}{link} = "etac";
	$particles{515}{link} = "B2-star-5747-zero";
	$particles{30323}{link} = "K-star-1680";
	$particles{100211}{link} = "pi-1300";
	$particles{10315}{link} = "K2-1770";
	$particles{30313}{link} = "K-star-1680";
	$particles{323}{link} = "K-star-892";
	$particles{20553}{link} = "chi-b1-1P";
	$particles{10113}{link} = "b1-1235";
	$particles{20213}{link} = "a1-1260";
	$particles{533}{link} = "Bs-star";
	$particles{20443}{link} = "chi-c1-1P";
	$particles{20113}{link} = "a1-1260";
	$particles{513}{link} = "B-star";
	$particles{9010211}{link} = "pi-1800";
	$particles{120553}{link} = "chi-b1-2P";
	$particles{10551}{link} = "chi-b0-1P";
	$particles{9000111}{link} = "a0-980";
	$particles{20313}{link} = "K1-1400";
	$particles{9000553}{link} = "upsilon-10860";
	$particles{215}{link} = "a2-1320";
	$particles{10111}{link} = "a0-1450";
	$particles{10325}{link} = "K2-1770";
	$particles{10313}{link} = "K1-1270";
	$particles{300553}{link} = "upsilon-4S";
	$particles{10115}{link} = "pi2-1670";
	$particles{327}{link} = "K3-star-1780";
	$particles{553}{link} = "upsilon-1S";
	$particles{555}{link} = "chi-b2-1P";
	$particles{335}{link} = "f2-prime-1525";
	$particles{115}{link} = "a2-1320";
	$particles{9000221}{link} = "further-states";
	$particles{20315}{link} = "K2-1820";
	$particles{10323}{link} = "K1-1270";
	$particles{200553}{link} = "upsilon-3S";
	$particles{9000213}{link} = "pi1-1400";
	$particles{315}{link} = "K2-star-1430";
	$particles{10215}{link} = "pi2-1670";
	$particles{217}{link} = "rho3-1690";
	$particles{30213}{link} = "rho-1700";
	$particles{317}{link} = "K3-star-1780";
	$particles{110551}{link} = "chi-b0-2P";
	$particles{313}{link} = "K-star-892";
	$particles{10321}{link} = "K-zero-star-1430";
	$particles{435}{link} = "Ds2-2573-plus-minus";
	$particles{319}{link} = "K4-star-2045";
	$particles{100213}{link} = "rho-1450";
	$particles{9010553}{link} = "upsilon-11020";
	$particles{117}{link} = "rho3-1690";
	$particles{10443}{link} = "hc-1P";
	$particles{9010113}{link} = "pi1-1600";
	$particles{9000211}{link} = "a0-980";
	$particles{100111}{link} = "pi-1300";
	$particles{10311}{link} = "K-zero-star-1430";
	$particles{20325}{link} = "K2-1820";
	$particles{100553}{link} = "upsilon-2S";
	$particles{219}{link} = "a4-2040";
}

