use IO::All;

my @in = io('output.txt')->slurp;
my $out = IO::All->new("outputUniq.txt");
my %hash = '';

foreach my $line (@in) {
    $line =~ /(.*)\t\s(.*)/;
    $hash{$1} = $2;
}

foreach my $key (keys %hash){
    if($key){
        "$key\t\t$hash{$key}\n" >> io($out);
    }
}


