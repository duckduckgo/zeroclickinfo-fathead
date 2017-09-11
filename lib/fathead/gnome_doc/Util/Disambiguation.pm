package Util::Disambiguation;

use Moo;

has [qw(title articles)] => (
    is       => 'ro',
    required => 1,
);

1;
