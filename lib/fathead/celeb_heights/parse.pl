use warnings;
use strict;
use Web::Scraper;
use File::Slurp;

my $heights = scraper {
    process 'div[class="sAZ2 v11"]', "people[]" => scraper {
        process "//a/text()", name => 'TEXT';
        process '//div/text()', height => 'TEXT';
        process "a", page => '@href';
    };
};

open (OUT, ">output.txt");

foreach my $letter ('A'..'Z') {
    my $file_contents = read_file( "download/all$letter.html" ) ;
    my $res = $heights->scrape($file_contents, "http://www.celebheights.com/s/");
    my %seen;

    foreach my $person (@{$res->{'people'}}) {
        my $name = $person->{'name'};
        my $height = $person->{'height'};
        next unless $name && $height;

        # don't introduce duplicates
        next if(exists $seen{$name});
        $seen{$name} = 1;

        my ($feet, $inches, $centimeters) = $height =~ /(\d+)ft (.+)in \((\d+)cm\)/;
        next unless defined $feet && defined $inches;
        $inches = int($inches + 0.5);
        my $meters = $centimeters/100;

        my $fmt_height = qq|$feet' $inches" ($meters m)|;
        (my $fmt_name = $name) =~ s/^\s+//g;
        $fmt_name =~ s/\s+$//g;

        my $title = "$fmt_name";
        my $type = 'A';
        my $abstract = $fmt_height;
        my $source_url = $person->{'page'};
        next unless $source_url;

        print OUT "$title\t$type\t\t\t\t\t\t\t\t\t\t$abstract\t$source_url\n";
    }

}
