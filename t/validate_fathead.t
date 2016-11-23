use strict;
use warnings;

use Test::More;
use File::Find::Rule;
use File::Basename;
use Path::Tiny;
use Term::ANSIColor qw(:constants);

use t::lib::TestUtils;

my @folders;

if ($ENV{DDG_TEST_FATHEAD}) {
    push @folders, $ENV{DDG_TEST_FATHEAD};
}
else {
    my @files = File::Find::Rule->file()
                                ->name("output.txt")
                                ->in('lib/fathead/');
    @folders = map { basename(dirname($_)) } @files;
}

plan tests => (scalar @folders) * 8;

foreach my $dir (@folders) {
    local $Term::ANSIColor::AUTORESET = 1;
    diag BOLD WHITE "\nChecking $dir...";

    my $utils = t::lib::TestUtils->new( fathead => $dir );
    ok( $utils->duplicates, "Checking for duplicate titles" );
    ok( $utils->types, "Validating types" );
    ok( $utils->field_count, "Validating correct number of fields" );
    ok( $utils->escapes, "Checking for unescaped chars" );

    SKIP: {
        skip "NO DISAMBIGUATIONS TO CHECK", 2 unless $utils->disambiguations;
        ok( $utils->disambiguations_format, "Checking disambiguation format" );
        ok( $utils->disambiguations_missing, "Checking for disambiguations with missing titles" );
    }

    SKIP: {
        skip "COVERAGE DATA NOT FOUND", 1 unless $utils->cover_dir;
        ok( $utils->coverage, "Testing article coverage" );
    }

    SKIP: {
        skip "TRIGGER WORDS NOT FOUND", 1 unless $utils->trigger_words;
        ok( $utils->category_clash, "Testing category / title clashes" );
    }

    diag "\n";
    diag BOLD WHITE "...Finished";
    diag "\n";
}

done_testing();
