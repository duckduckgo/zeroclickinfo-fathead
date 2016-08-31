package Util;

use Util::Article;
use Util::Disambiguation;

BEGIN {
    require Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT_OK = qw(article disambiguation);
}

sub article {
    Util::Article->new(@_);
}

sub disambiguation {
    Util::Disambiguation->new(@_);
}

1;
