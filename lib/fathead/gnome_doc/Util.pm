package Util;

use Util::Article;
use Util::Disambiguation;

BEGIN {
    require Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT_OK = qw(article disambiguation indexer parser);
}

sub article {
    Util::Article->new(@_);
}

sub disambiguation {
    Util::Disambiguation->new(@_);
}

sub indexer {
    Util::Index->new(@_);
}

sub parser {
    Util::Parser->new(@_);
}

1;
