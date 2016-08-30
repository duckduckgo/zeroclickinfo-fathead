package Util::Page;
# ABSTRACT: Page along with parse and DOM information.

use Moo;

has [qw(basename filename parsers full_path full_url)] => (
    is => 'ro',
);

1;
