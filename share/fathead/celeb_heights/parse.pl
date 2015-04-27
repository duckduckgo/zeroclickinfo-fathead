use warnings;
use strict;
use Web::Scraper;
use File::Slurp;

my $heights = scraper {
    # Parse all TDs inside 'table[width="100%]"', store them into
    # an array 'authors'.  We embed other scrapers for each TD.
    process 'div[class="starazCol v11"]', "people[]" => scraper {
        process 'div[class="starazCol v11"]', info => 'TEXT';
        process "a", page => '@href';
    };
};

open (OUT, ">output.txt");

foreach my $letter ('A'..'Z') {
    my $file_contents = read_file( "download/$letter.html" ) ;
    my $res = $heights->scrape($file_contents, "http://www.celebheights.com/s/");

    foreach my $person (@{$res->{'people'}}) {
        my ($name, $height) = $person->{'info'} =~ /([^\(]+)\s*\((.*)\)/;
        next unless $name && $height;

        my ($feet, $inches) = $height =~ /(\d+)ft (.+)in/;
        next unless defined $feet && defined $inches;
        $inches = int($inches + 0.5);

        my $meters = int((12*$feet + $inches) * 0.0254*100 + 0.5)/100;

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
