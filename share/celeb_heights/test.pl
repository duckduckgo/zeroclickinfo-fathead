use Math::Calc::Units qw(convert);

my ($value, $units) = convert("72 inches", "meters");
print "$value $units\n";
