use strict;
use warnings;

use Test::More;
use t::lib::TestUtils;


my @files;

if ($ENV{DDG_TEST_FATHEAD}) {
    push @files, $ENV{DDG_TEST_FATHEAD};
}
else {
    @files = File::Find::Rule->file()
                                ->name("output.txt")
                                ->in('lib/fathead/');
}

foreach my $file (@files) {
    warn "Basename:";
    warn File::Basename::dirname($file);
    warn "Fileparse:";
    warn File::Basename::fileparse($file);
    my $utils = t::lib::TestUtils->new( fathead => $ENV{DDG_TEST_FATHEAD} );
    ok( $utils->duplicates, "Checking for duplicate titles" );
    ok( $utils->types, "Validating types" );
    ok( $utils->field_count, "Validating correct number of fields" );
    ok( $utils->escapes, "Checking for unescaped chars" );
    ok( $utils->disambiguations_format, "Checking disambiguation format" );
    ok( $utils->disambiguations_missing, "Checking for disambiguations with missing titles" );

    SKIP: {
        skip "COVERAGE DATA NOT FOUND", 1 unless $utils->cover_dir;
        ok( $utils->coverage, "Testing language feature coverage" );
    }

    SKIP: {
        skip "Trigger words not found", 1 unless $utils->trigger_words;
        ok( $utils->category_clash, "Testing category / title clashes" );
    }
}

done_testing;
