use strict;
use warnings;

use Test::More;
use t::lib::TestUtils;

plan skip_all => 'Set DDG_TEST_FATHEAD to a fathead id to run these tests' unless $ENV{DDG_TEST_FATHEAD};
my $utils = t::lib::TestUtils->new( fathead => $ENV{DDG_TEST_FATHEAD} );

ok( $utils->duplicates, "Checking for duplicate titles" );
ok( $utils->types, "Validating types" );
ok( $utils->field_count, "Validating correct number of fields" );
ok( $utils->escapes, "Checking for unescaped chars" );

SKIP_COVERAGE: {
    skip "COVERAGE DATA NOT FOUND", 1 unless $utils->cover_dir;
    ok( $utils->coverage, "Testing language feature coverage" );
}

done_testing;
