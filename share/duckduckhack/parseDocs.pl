#usr/bin/env perl
use warnings;
binmode STDOUT, ":utf8";
use Mojo::DOM;
use Data::Dumper;
use DBI;
use IO::All -utf8;

my $category = '';
my $textFlag = 0;
my $line = 0;
my $debug = 0;
my $fileName = $ARGV[0];


# Open longtail and fathead output files
my $fathead_out = IO::All->new("output.txt");

# read in page to html string
my $html = do { local $/; <> };

#
# update the DB with a new fathead entry
#
sub writeFathead{
    my ($title,$abstract,$source_url, $image) = @_;
    #"$title\tA\t\t\t\t\t\t\t\t\t$image\t$abstract.\t$source_url\n" >> io("output.txt");
    "$title\tA\t\t\t\t\t\t\t\t\t\t$abstract\t$source_url\n" >> io("output.txt");
}

# remove comments, Mojo won't parse inside a comment
$html =~ s/<!--[\s?]+\/summary[\s?]+-->/<p>\/summary<\/p>/gi;

# read in html to parser and find content div
my $dom = Mojo::DOM->new($html);
$dom = $dom->find('div.content');

my @text; #array of text for each longtail result
my $titles = $dom->find('h2')->text;
my @title = split '\n', $titles; # each title for the longtail
my @names = split '\n', $dom->find('h2')->a->attr('name');
my @nodes = split '\n', $dom->find('h2,h3,h4,p,ul,pre');

#
# Figure out what category to use this makes the titles more relevant
# to what people might search for. i.e. template => spice template
#
my @categories = (
	'spice',
	'goodie',
	'fathead',
	'longtail',
	'duckpan'
);

my $regex = join('|', @categories);
my @match = ($fileName =~ /$regex/ig);

if($match[0]){
	$category = $match[0];
}
else {
	$category = 'DuckDuckHack';
}

# iterate through domArray look for titles
# add everything after a title to the text
# array up to a /summary tag or the next h2 tag
#
$fileName =~ s/\.html//;
my $docNumber = 0;
foreach my $domElement (@nodes){

    #find the title and set textFlag
    if($domElement =~ /name\=\"\Q$names[$docNumber]\E/){
        $docNumber += 1;
        $textFlag = 1;
    }

    # two cases once we set the textFlag
    elsif ($textFlag){
        # stop adding Textt when we find /summary tag
        if($domElement =~ s/\/summary.*//){
            $text[$docNumber-1] .= $domElement;
            $textFlag = 0;
        }
        # stop adding text if we find the next <h2> tag
        else {
            $domElement =~ s/\<\/h2\>//;
            $text[$docNumber-1] .= $domElement;
        }
    }

    if($docNumber > scalar(@title)){
        last;
    }
}

# Create the solr doc and update the fathead DB
for(my $i = 0; $i < scalar(@title); $i++){
	# add moreAt links
    my $moreAt = $title[$i];
    $moreAt =~ s/\_//g;
	$moreAt = $fileName.'#'.$moreAt;
	$moreAt =~ s/\s/\-/g;
    $moreAt =~ s/^download\///;

	# add a category to the title if it doesn't already
	# have one of the category keywords
	if(not ($title[$i] =~ /$category/i)){
		$title[$i] = ($category.' '.$title[$i])
	}

	# find all # links and prepend duck.co to them
	my $duckURL = 'href="http://duck.co/duckduckhack/';
	$duckURL .= ($fileName.'#');
	$text[$i] =~ s/href\=\"\#/$duckURL/g;

	# remove all links for the fathead
    $text[$i] =~ s/\<a(.*?)\>(.*?)\<\/a\>/$2/g;

    $text[$i] =~ s/<!--//g;

	writeFathead( $title[$i], $text[$i], "http://duck.co/duckduckhack/". lc $moreAt);

	if($i % 10){
		print ".";
	}
}

$fathead_out->close;
