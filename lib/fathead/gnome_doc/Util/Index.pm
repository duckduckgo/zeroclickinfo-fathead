package Util::Index;

use Moo;

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

        $links->{ $name }->{ basename } = $basename;
        $links->{ $name }->{ filename } = $filename;
        $links->{ $name }->{ parsers } = \@parsers;
    }

    return $links;
}

1;
