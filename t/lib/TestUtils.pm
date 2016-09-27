package t::lib::TestUtils;
use strict;
use warnings;

use Moo;
use IPC::Open3;
use aliased 'File::Spec' => 'f';
use File::chdir;
use FindBin;
use IO::All;
use List::MoreUtils qw/ uniq /;

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
    die "$fdir does not exist" unless -d $fdir;
    return $fdir;
}

has cover_dir => ( is => 'lazy' );
sub _build_cover_dir {
    my ( $self ) = @_;
    my $cdir = f->catdir( $self->fathead_dir, qw/ cover / );
    warn sprintf( "Coverage files for %s not found in %s", $self->fathead, $cdir )
        unless -d $cdir;
    return $cdir;
}

has output_txt => ( is => 'lazy' );
sub _build_output_txt {
    my ( $self ) = @_;
    my $otxt = f->catfile( $self->fathead_dir, q'output.txt' );
    die "$otxt does not exist - please generate it" unless -f $otxt;
    return $otxt;
}

has trigger_words => ( is => 'lazy' );
sub _build_trigger_words {
    my ( $self ) = @_;
    my $ttxt = f->catfile( $self->fathead_dir, q'trigger_words.txt' );
    if ( ! -f $ttxt ) {
        warn "$ttxt does not exist - will not be able to validate categories";
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
    [
        map { split /\s*\\n\s*/ }
        map { ( split /\t/ )[9] }
        grep { /\tD\t/ }
        @{ $self->content }
    ]
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
    warn sprintf( "%s duplicated on lines %s", $_, join( ', ', @{ $self->titles->{$_}->{lines} } ) )
        for @dupes;
    return @dupes ? 0 : 1;
}

sub coverage {
    my ( $self ) = @_;
    my $titles = [ keys %{ $self->titles } ];
    my @missing = $self->_a_not_in_b( $self->cover_titles, $titles );
    warn sprintf "Titles missing coverage in output : %s", join( ', ', @missing ) if @missing;
    return @missing ? 0 : 1;
}

sub types {
    my ( $self ) = @_;
    my @output_types = uniq map { ( split /\t/ )[1] } @{ $self->content };
    my @invalid_types = $self->_a_not_in_b( \@output_types, $self->valid_types );
    warn sprintf "Output contains invalid type fields : %s", join( ', ', @invalid_types ) if @invalid_types;
    return @invalid_types ? 1 : 0;
}

sub field_count {
    my ( $self ) = @_;
    my $r = 1;
    while ( my ( $number, $line ) = each @{ $self->content } ) {
        my $fields = scalar split /\t/, $line;
        if ( $fields > 13 ) {
            warn sprintf "Line %d appears to have %d fields", $number + 1, $fields;
            $r = 0;
        }
    }
    return $r;
}

sub escapes {
    my ( $self ) = @_;
    my $r = 1;
    while ( my ( $number, $line ) = each @{ $self->content } ) {
        my $abstract = ( split /\t/, $line )[11];
        next unless $abstract;
        if ( my @matches = $abstract =~ /([^\\]\\[0-9nrtx])/g ) {
            warn sprintf "Line %d appears to contain unescaped characters : %s",
                $number + 1, join( ', ', @matches );
            $r = 0;
        }
    }
    return $r;
}

sub disambiguations_format {
    my ( $self ) = @_;
    # (*[\w+]\s?\w+\n)+
    my @invalid;
    for my $d ( @{ $self->disambiguations } ) {
        push @invalid, $d unless $d =~ /^\*\[\[[^\]]*\]\],/;
    }
    warn sprintf "The following disambiguation entries appear to be invalid:\n%s\n",
        join( "\n\n", @invalid )
        if @invalid;

    return @invalid ? 0 : 1;
}

sub disambiguations_missing {
    my ( $self ) = @_;
    my @disambiguation_titles =
        map { lc }
        map { /^\*\[\[([^\]]*)/ }
        @{ $self->disambiguations };
    my @titles = uniq map { lc } keys %{ $self->titles };

    my @missing = $self->_a_not_in_b( \@disambiguation_titles, \@titles );

    warn sprintf "Disambiguation titles missing from articles : %s", join( ', ', @missing ) if @missing;
    return @missing ? 0 : 1;
}

sub category_clash {
    my ( $self ) = @_;
    my $re = join '|', @{ $self->trigger_words };
    my @filtered_categories =
        map { s/^\s+|\s+$//r }
        map { s/\b$re\b//gr }
        @{ $self->categories };

    my @titles = uniq map { lc } keys %{ $self->titles };
    my @clash = $self->_a_in_b( \@filtered_categories, \@titles );

    warn sprintf "Categories matching article titles : %s", join( ', ', @clash ) if @clash;
    return @clash ? 0 : 1;
}

1;
