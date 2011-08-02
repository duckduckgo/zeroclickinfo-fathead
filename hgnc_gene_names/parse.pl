#!/usr/bin/env perl -w

############################################################################################
#
# hgnc_gene_names/parse.pl
#
#    parse the HGNC complete dataset download into format for DuckDuckGo zero-click info
#    
#    Copyright 2011 Joshua C. Randall <jcrandall@alum.mit.edu>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU Affero General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU Affero General Public License for more details.
#
#    You should have received a copy of the GNU Affero General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
############################################################################################

use strict;
use warnings;

# support for compressed data files
use IO::Uncompress::AnyInflate;


# settings
my $trace = 0; # whether or not to output program trace to STDOUT
my $also_name = 1; # whether or not to also generate output entries for approved name
my $also_aka = 1; # whether or not to also generate output entries for previous/synonymous terms (symbols and names)
my $in_field_sep = "\t";
my $out_field_sep = "\t";
my $hgnc_id_header = "HGNC ID";
my $approved_symbol_header = "Approved Symbol";
my $approved_name_header = "Approved Name";;
my $status_header = "Status";
my $locus_type_header = "Locus Type";
my $chromosome_header = "Chromosome";
my $previous_symbols_header = "Previous Symbols";
my $previous_names_header = "Previous Names";
my $name_synonyms_header = "Name Synonyms";
my $synonyms_header = "Synonyms";
my $specialist_dblinks_header = "Specialist Database Links";
my $entrez_gene_id_header = "Entrez Gene ID (mapped data supplied by NCBI)";
# external database link URLs
my $hgnc_gene_symbol_base_url = "http://www.genenames.org/data/hgnc_data.php?hgnc_id=";
my $entrez_gene_base_url = "http://www.ncbi.nlm.nih.gov/gene?term=";
my $ensembl_homo_sapiens_gene_base_url = "www.ensembl.org/Homo_sapiens/Search/Details?species=Homo_sapiens;idx=Gene;end=1;q=";

# print logging
sub trace {
	my $message = shift;
	print $message."\n" if $trace;
}
sub error {
	my $message = shift;
	print STDERR $message."\n";
}


# first argument is input filename
# data file should be downloaded from HUGO Gene Nomenclature Committee
# download link for "complete HGNC dataset" at: http://www.genenames.org/cgi-bin/hgnc_stats.pl
# current download link: http://www.genenames.org/cgi-bin/hgnc_downloads.cgi?title=HGNC+output+data&hgnc_dbtag=on&preset=all&status=Approved&status=Entry+Withdrawn&status_opt=2&level=pri&=on&where=&order_by=gd_app_sym_sort&limit=&format=text&submit=submit&.cgifields=&.cgifields=level&.cgifields=chr&.cgifields=status&.cgifields=hgnc_dbtag
my $infile = shift or die "must specify input file for parse.pl as first argument.\ne.g. parse.pl hgnc_complete_dataset.tsv.gz hgnc_gene_names.tsv\n";

# second argument is output filename
my $outfile = shift or die "must specify output file for parse.pl as second argument.\ne.g. parse.pl hgnc_complete_dataset.tsv.gz hgnc_gene_names.tsv\n";

# open input filehandle (or die trying)
my $infh;
if($infile && ($infile =~ m/gz$/)) {
	trace("opening input file $infile using IO::Uncompress::AnyInflate");
	$infh = new IO::Uncompress::AnyInflate $infile or die "Could not open $infile using AnyInflate for input\n";
} else {
	$infh = new IO::File;
	trace("opening input file $infile using IO::File");
	$infh->open("<$infile") or die "Could not open $infile for input\n";
}
trace("opened input file $infile for reading");


# open output filehandle (or die trying)
my $outfh = new IO::File;
trace("opening output file $outfile using IO::File");
$outfh->open(">$outfile") or die "Could not open $outfile for output\n";
trace("opened output file $outfile for writing");


# parse header line
my $headerline = <$infh>;
chomp $headerline;
$headerline =~ s/\r$//;
my @headers = split /$in_field_sep/,$headerline,-1;
my %headercol;
my %colheader;
my $col=0;
my %have;
foreach my $header (@headers) {
    $have{$col}=1;
    $colheader{$col} = $header;
    $headercol{$header} = $col;
    $col++;
}
my $numcols = $col;
trace("read $numcols header columns from data file");


# ensure we have required columns
trace("checking whether we have all required columns");
die "could not find column header $status_header" unless exists($headercol{$status_header});
die "could not find column header $hgnc_id_header" unless exists($headercol{$hgnc_id_header});
die "could not find column header $approved_symbol_header" unless exists($headercol{$approved_symbol_header});
die "could not find column header $approved_name_header" unless exists($headercol{$approved_name_header});
die "could not find column header $locus_type_header" unless exists($headercol{$locus_type_header});
die "could not find column header $chromosome_header" unless exists($headercol{$chromosome_header});
die "could not find column header $previous_symbols_header" unless exists($headercol{$previous_symbols_header});
die "could not find column header $previous_names_header" unless exists($headercol{$previous_names_header});
die "could not find column header $name_synonyms_header" unless exists($headercol{$name_synonyms_header});
die "could not find column header $synonyms_header" unless exists($headercol{$synonyms_header});
die "could not find column header $specialist_dblinks_header" unless exists($headercol{$specialist_dblinks_header});
die "could not find column header $entrez_gene_id_header" unless exists($headercol{$entrez_gene_id_header});

# parse data lines
trace("parsing data lines");
my $incount = 0;
my $outcount = 0;
while(my $line = <$infh>) {
    $incount++;
    chomp $line;
    $line =~ s/\r$//;
    my @cols = split /$in_field_sep/,$line,-1;
    
    # set output defaults
    my $url = ""; # target URL for more information (required)
    my $description = ""; # goes below description (some combination of description, synopsis, details is required)
    my $synopsis = ""; # gets put in a highlighed box above description
    my $details = ""; # not sure where this would go
    
    # check status
    my $status = $cols[$headercol{$status_header}];
    if(! ($status =~ qr/Approved/i)) {
	# skip if not approved
	next;
    }
    
    # get data needed for text
    my $hgnc_id = $cols[$headercol{$hgnc_id_header}];
    my $approved_symbol = $cols[$headercol{$approved_symbol_header}];
    my $approved_name = $cols[$headercol{$approved_name_header}];
    my $locus_type = $cols[$headercol{$locus_type_header}];
    my $chromosome = $cols[$headercol{$chromosome_header}];
    my $previous_symbols = $cols[$headercol{$previous_symbols_header}];
    my $previous_names = $cols[$headercol{$previous_names_header}];
    my $name_synonyms = $cols[$headercol{$name_synonyms_header}];
    my $synonyms = $cols[$headercol{$synonyms_header}];
    my $specialist_dblinks = $cols[$headercol{$specialist_dblinks_header}];
    my $entrez_gene_id = $cols[$headercol{$entrez_gene_id_header}];
    
    # build target url
    $url = $hgnc_gene_symbol_base_url . $hgnc_id;
    
    # build synopsis
    $synopsis = "$approved_symbol ($approved_name)";
    
    # build description paragraph
    # start with locus type
    # descriptions below based on: http://www.genenames.org/useful/symbol-report-documentation#locus_type
    if($locus_type =~ qr/^gene.*protein/i) {
	# locus_type_data_protein_coding_gene
	$description = "Protein-coding gene";
    } elsif($locus_type =~ qr/^RNA.*cluster/i) {
	# locus_type_data_RNA_cluster
	$description = "Cluster of small non-coding RNA genes";
    } elsif($locus_type =~ qr/^RNA.*long.non.coding/i) {
	# locus_type_data_RNA_lncRNA
	$description = "long non-coding RNA (lncRNA)";
    } elsif($locus_type =~ qr/^RNA.*micro/i) {
	# locus_type_data_RNA_miRNA
	$description = "microRNA (miRNA)";
    } elsif($locus_type =~ qr/^RNA.*ribo/i) {
	# locus_type_data_RNA_rRNA
	$description = "ribosomal RNA (rRNA)";
    } elsif($locus_type =~ qr/^RNA.*small.nuclear/i) {
	# locus_type_data_RNA_snRNA
	$description = "small nuclear RNA (snRNA)";
    } elsif($locus_type =~ qr/^RNA.*small.nucleolar/i) {
	# locus_type_data_RNA_snoRNA
	$description = "small nucleolar RNA (snoRNA)";
    } elsif($locus_type =~ qr/^RNA.*small.cytoplasmic/i) {
	# locus_type_data_RNA_scRNA
	$description = "small cytoplasmic RNA (scRNA)";
    } elsif($locus_type =~ qr/^RNA.*transfer/i) {
	# locus_type_data_RNA_tRNA
	$description = "transfer RNA (tRNA)";
    } elsif($locus_type =~ qr/^RNA.*small.misc/i) {
	# locus_type_data_RNA_misc
	$description = "miscellaneous small non-coding RNA (ncRNA)";
    } elsif($locus_type =~ qr/phenotype/i) {
	# locus_type_data_phenotype_only
	$description = "mapped phenotype (causative gene not yet identified)";
    } elsif($locus_type =~ qr/^pseudogene/i) {
	# locus_type_data_pseudogene
	$description = "pseudogene (similar to protein-coding gene, but does not encode a functional protein)";
    } elsif($locus_type =~ qr/^RNA.*pseudogene/i) {
	# locus_type_data_RNA_pseudogene
	$description = "pseudogene of non-protein coding RNA";
    } elsif($locus_type =~ qr/^complex.*locus.*constituent/i) {
	# locus_type_data_complex
	$description = "transcriptional unit part of a complex locus";
    } elsif($locus_type =~ qr/^endo.*retrovirus/i) {
	# locus_type_data_retrovirus
	$description = "endogenous retroviral gene";
    } elsif($locus_type =~ qr/^fragile/i) {
	# locus_type_data_fragile
	$description = "fragile site";
    } elsif($locus_type =~ qr/^immunoglobulin.*gene/i) {
	# locus_type_data_immunoglobulin_gene
	$description = "immunoglobulin gene";
    } elsif($locus_type =~ qr/^immunoglobulin.*pseudogene/i) {
	# locus_type_data_immunoglobulin_pseudogene
	$description = "immunoglobulin pseudogene";
    } elsif($locus_type =~ qr/^protocadherin/i) {
	# locus_type_data_protocadherin
	$description = "gene segment that constitutes protocadherins";
    } elsif($locus_type =~ qr/readthrough/i) {
	# locus_type_data_readthrough
	$description = "readthrough gene";
    } elsif($locus_type =~qr/region/i) { 
	# locus_type_data_region
	$description = "genomic region";
    } elsif($locus_type =~ qr/t.cell.receptor.gene/i) {
	# locus_type_data_t_cell_receptor
	$description = "T cell receptor gene";
    } elsif($locus_type =~ qr/t.cell.receptor.pseudogene/i) {
	# locus_type_data_t_cell_receptor
	$description = "T cell receptor pseudogene";
    } elsif($locus_type =~ qr/transposable/i) {
	# locus_type_data_transposable
	$description = "Transposable genomic element";
    } elsif($locus_type =~ qr/unknown/i) {
	# locus_type_data_unknown
	$description = "Genomic locus of unknown type";
    } elsif($locus_type =~ qr/virus.*integration/i) {
	# locus_type_data_virus_integration_site
	$description = "Virus integration site";
    } elsif($locus_type =~ qr/withdrawn/i) {
	# locus_type_withdrawn, skip it!
	next;
    } else {
	# unrecognized locus type
	error("parse.pl encountered unrecognized locus type [$locus_type]\n");
	# and skip it!
	next;
    }
    
    # add chromosomal location (if present) to description
    if($chromosome =~ m/^[0-9XY]/) {
	$description .= " on human chromosome $chromosome";
    }
    
    # end intro sentence
    $description .= ".";
    
    # add synonyms and previous names (if present)
    my %aka;
    if($previous_symbols =~ m/[[:alnum:]]+/ 
       || $previous_names =~ m/[[:alnum:]]+/
       || $synonyms =~ m/[[:alnum:]]+/
       || $name_synonyms =~ m/[[:alnum:]]+/) {
	foreach my $name (split /,[[:space:]]*/, $previous_symbols) {
	    $aka{$name} = 1;
	}
	foreach my $name (split /,[[:space:]]*/, $previous_names) {
	    # strip quotes from names
	    $name =~ s/\"//g;
	    $aka{$name} = 1;
	}
	foreach my $name (split /,[[:space:]]*/, $synonyms) {
	    $aka{$name} = 1;
	}
	foreach my $name (split /,[[:space:]]*/, $name_synonyms) {
	    # strip quotes from names
	    $name =~ s/\"//g;
	    $aka{$name} = 1;
	}
	$description .= " Also known as ".join(", ", sort keys %aka).".";
    }
    
    # add general and specialist links
    my %links;
    # add NCBI Entrez Gene link (if Entrez Gene ID present)
    if($entrez_gene_id =~ m/^[[:digit:]]+$/) {
	$links{"Entrez"} = $entrez_gene_base_url . $entrez_gene_id;
    }
    
    # add specialist DB links
    while($specialist_dblinks =~ m/\<a.*?href\=["'](http\:\/\/.*?)["'].*?\>(.*?)\<\/a\>/g) {
	my $url = $1;
	my $db = $2;
	$links{$db} = $url;
    }
    if(keys %links) {
	$details .= "More information from: ".join(", ", map {'<a href="'.$links{$_}.'">'.$_.'</a>' } sort keys %links).".";
    }

    # unused output vars
    my $namespace = "";
    my $type= "";
    my $lang = "";
    
    # output data for each page
    my @page_terms = ($approved_symbol);
    if($also_name) {
	push @page_terms, $approved_name;
    }
    if($also_aka) {
	foreach my $aka_term (keys %aka) {
	    push @page_terms, $aka_term;
	}
    }
    foreach my $page (@page_terms) {
	$outcount++;
	print $outfh join($out_field_sep, ($page, $namespace, $url, $description, $synopsis, $details, $type, $lang) )."\n";
    }
}
trace("processed $incount lines and output $outcount entries");


