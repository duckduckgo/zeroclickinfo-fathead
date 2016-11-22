package t::lib::TestUtils;

use strict;
use warnings;
use open ':std', ':encoding(utf8)';

use Moo;
use IPC::Open3;
use aliased 'File::Spec' => 'f';
use File::chdir;
use FindBin;
use IO::All;
use List::MoreUtils qw/ uniq /;
use Test::More;
use Term::ANSIColor qw(:constants);

has fathead => ( is => 'ro' );

has project_root => ( is => 'lazy' );
sub _build_project_root {
    my ( $self ) = @_;
    my $toplevel;
    {
        local $CWD = "$FindBin::Bin";
        chomp( $toplevel = qx/ git rev-parse --show-toplevel / );
    }
    die "Unable to ascertain project root directory" unless $toplevel;
    return $toplevel;
}

has fathead_dir => ( is => 'lazy' );
sub _build_fathead_dir {
    my ( $self ) = @_;
    my $fdir = f->catdir( $self->project_root, qw/ lib fathead /, $self->fathead );
    die RED "$fdir does not exist" unless -d $fdir;
    return $fdir;
}

has cover_dir => ( is => 'lazy' );
sub _build_cover_dir {
    my ( $self ) = @_;
    my $cdir = f->catdir( $self->fathead_dir, qw/ cover / );
    if ( ! -d $cdir ) {
        diag YELLOW "NO COVERAGE DATA FOUND. " .
            "Please see: https://docs.duckduckhack.com/programming-mission/creating-effective-fatheads.html#full-topic-coverage";
        return;
    }
    return $cdir;
}

has output_txt => ( is => 'lazy' );
sub _build_output_txt {
    my ( $self ) = @_;
    my $otxt = f->catfile( $self->fathead_dir, q'output.txt' );
    die RED "NO OUTPUT FILE FOUND" unless -f $otxt;
    return $otxt;
}

has trigger_words => ( is => 'lazy' );
sub _build_trigger_words {
    my ( $self ) = @_;
    my $ttxt = f->catfile( $self->fathead_dir, q'trigger_words.txt' );
    if ( ! -f $ttxt ) {
        diag YELLOW "NO TRIGGER WORDS FOUND. Will not be able to validate categories";
        return;
    }
    [ grep { $_ } io( $ttxt )->utf8->chomp->getlines ];
}

has cover_titles => ( is => 'lazy' );
sub _build_cover_titles {
    [
        uniq sort map { lc }
        map { io( $_ )->utf8->chomp->getlines }
        glob f->catfile( $_[0]->cover_dir, '*.txt' )
    ];
}

has content => ( is => 'lazy' );
sub _build_content {
    [ io( $_[0]->output_txt )->utf8->slurp ];
}

has titles => ( is => 'lazy' );
sub _build_titles {
    my ( $self ) = @_;
    my $titles;

    while ( my ( $number, $line ) = each @{ $self->content } ) {
        my $title = ( split /\t/, $line )[0];
        $titles->{ $title }->{count}++;
        push @{ $titles->{ $title }->{lines} }, $number + 1;
    }
    return $titles;
}

has lc_titles => ( is => 'lazy' );
sub _build_lc_titles {
    my ( $self ) = @_;
    my @lc_titles = uniq map { lc } keys %{ $self->titles };
    return \@lc_titles;
}

has valid_types => ( is => 'lazy' );
sub _build_valid_types {
    [qw/ A D R /]
}

has categories => ( is => 'lazy' );
sub _build_categories {
    my ( $self ) = @_;
    [
        uniq map { lc }
        map { split /\s*\\n\s*/ }
        grep { $_ }
        map { ( split /\t/ )[4] }
        @{ $self->content }
    ];
}

has disambiguations => ( is => 'lazy' );
sub _build_disambiguations {
    my ( $self ) = @_;

    my @disambiguations = grep { /\tD\t/ } @{ $self->content };
    unless (@disambiguations){
        diag YELLOW "NO DISAMBIGUATIONS FOUND";
        return;
    };
    [
        map { split /\s*\\n\s*/ }
        map { ( split /\t/ )[9] }
        @disambiguations
    ];
}

sub _a_in_b {
    my ( $self, $list_a, $list_b ) = @_;
    my @present;
    my %presence_of =
        map { $_ => 1 }
        @{ $list_b };

    for my $entry ( @{ $list_a } ) {
        push @present, $entry if $presence_of{ $entry };
    }

    return wantarray ? @present : \@present;
}

sub _a_not_in_b {
    my ( $self, $list_a, $list_b ) = @_;
    my @missing;

    my %presence_of =
        map { $_ => 1 }
        @{ $list_b };

    for my $entry ( @{ $list_a } ) {
        push @missing, $entry unless $presence_of{ $entry };
    }

    return wantarray ? @missing : \@missing;
}

sub duplicates {
    my ( $self ) = @_;
    my @dupes = grep { $self->titles->{$_}->{count} > 1 } keys %{ $self->titles };

    if (@dupes){
        my @sorted = sort({ length("$b") <=> length ("$a") } @dupes);
        my $min_width = length($sorted[0]) + 5;
        my $count = scalar @dupes;
        diag YELLOW "\n$count DUPLICATE ARTICLES FOUND:";
        diag sprintf( "%-*s duplicated on lines %s", $min_width, $_, join( ', ', @{ $self->titles->{$_}->{lines} } ) )
        for @dupes;
    }
    return @dupes ? 0 : 1;
}

sub coverage {
    my ( $self ) = @_;
    my $titles = $self->lc_titles;
    my @missing = $self->_a_not_in_b( $self->cover_titles, $titles );

    if (@missing){
        my $count = scalar @missing;
        diag YELLOW "\n$count TITLES MISSING IN OUTPUT FILE";
        diag $_ for @missing;
    }
    return @missing ? 0 : 1;
}

sub types {
    my ( $self ) = @_;
    my @output_types = uniq map { ( split /\t/ )[1] } @{ $self->content };
    my @invalid_types = $self->_a_not_in_b( \@output_types, $self->valid_types );

    if (@invalid_types){
        my $count = scalar @invalid_types;
        diag YELLOW "$count INVALID ARTICLE TYPES FOUND";
        diag $_ for @invalid_types;
    }
    return @invalid_types ? 0 : 1;
}

sub field_count {
    my ( $self ) = @_;
    my $r = 1;
    while ( my ( $number, $line ) = each @{ $self->content } ) {
        my $fields = scalar split /\t/, $line;
        if ( $fields > 13 ) {
            diag sprintf "Line %-5d appears to have %d fields", $number + 1, $fields;
            $r = 0;
        }
    }
    return $r;
}

sub escapes {
    my ( $self ) = @_;
    my $r = 1;
    my @unescaped;
    while ( my ( $number, $line ) = each @{ $self->content } ) {
        my $abstract = ( split /\t/, $line )[11];
        next unless $abstract;
        if ( my @matches = $abstract =~ /([^\\]\\[0-9x]|[\n\r\t])/g ) {
            push @unescaped, sprintf "Line %-5d contains: %s",
                $number + 1, join( ', ', @matches );
            $r = 0;
        }
    }
    if (@unescaped){
        my $count = scalar @unescaped;
        diag YELLOW "\n$count POSSIBLY UNESCAPED CHARACTERS FOUND:";
        diag $_ for @unescaped;
    }
    return $r;
}

sub disambiguations_format {
    my ( $self ) = @_;
    # (*[[\w+]],?\s?\w+\n)+
    my @invalid;
    for my $d ( @{ $self->disambiguations } ) {
        push @invalid, $d unless $d =~ /^\*\[\[.+?\]\],?\s*.+?\.$/;
    }
    if (@invalid){
        my $count = scalar @invalid;
        diag YELLOW "\n$count INVALID DISMABIGUATIONS FOUND:";
        diag $_ for @invalid;
    }

    return @invalid ? 0 : 1;
}

sub disambiguations_missing {
    my ( $self ) = @_;
    my @disambiguation_titles =
        map { /^\*\[\[(.+?)\]\],?\s*.+?\.$/ }
        @{ $self->disambiguations };

    my $titles = keys $self->titles;
    my @missing = $self->_a_not_in_b( \@disambiguation_titles, $titles );

    if (@missing){
        my $count = scalar @missing;
        diag YELLOW "\n$count DISAMBIGUATION TITLES MISSING FROM ARTICLES:";
        diag $_ for @missing;
    }
    return @missing ? 0 : 1;
}

sub category_clash {
    my ( $self ) = @_;
    my $re = join '|', @{ $self->trigger_words };
    my @filtered_categories =
        map { s/^\s+|\s+$//r }
        map { s/\b$re\b//gr }
        @{ $self->categories };

    my @titles = $self->lc_titles;
    my @clash = $self->_a_in_b( \@filtered_categories, \@titles );

    if (@clash){
        my $count = scalar @clash;
        diag YELLOW "$count CATEGORIES NAMES MATCHING ARTICLE TITLES";
        diag $_ for @clash;
    }
    return @clash ? 0 : 1;
}
1;
