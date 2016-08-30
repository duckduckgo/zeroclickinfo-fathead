package Util::Index;

use Cwd qw( getcwd );
use Moo;

has docs_dir => ( is => 'lazy' );
sub _build_docs_dir {
    File::Spec->catdir( $_[0]->working_dir, qw/ .. download / );
}

has working_dir => ( is => 'lazy' );
sub _build_working_dir {
    getcwd;
}

sub doc_fullurl {
    my ( $self, $part ) = @_;
    URI->new(
        sprintf( '%s%s', $self->perldoc_url, $part )
    )->canonical
}

sub build_indices {
    my ( $self ) = @_;
    my $indices;
    my @index_pages = ( qw/
       index-tutorials
       index-faq
       index-language
       index-overview
       index-functions
       index-pragmas
       index-utilities
       index-internals
       index-platforms
    / );

    for ( @index_pages ) {
        $indices->{$_} = $self->links_from_index( $_ );
    }
    return $indices;
}

sub doc_fullpath {
    my ( $self, @parts ) = @_;
    $parts[-1] = $parts[-1] . '.html';
    File::Spec->catfile( $self->docs_dir, @parts );
}

sub links_from_index {
    my ( $self, $index ) = @_;
    my $path = $self->doc_fullpath( $index );
    return unless ( -f $path );
    my $links;

    my $index_link_parser = link_parser_for_index($index);

    my $dom = dom_for_file( $path );

    my $content = $dom->find('ul')->[4];

    my @links = $self->$index_link_parser($dom);

    foreach my $link (@links) {
        my $name     = $link->content;
        my $filename = $link->attr('href');
        my $basename = $filename =~ s/\.html$//r;
        my @parsers = get_parsers($index, $basename);

        $links->{$name} = {
            basename  => $basename,
            filename  => $filename,
            parsers   => \@parsers,
            full_path => $self->doc_fullpath($basename),
            full_url  => $self->doc_fullurl($filename),
        };
    }

    return $links;
}

1;
