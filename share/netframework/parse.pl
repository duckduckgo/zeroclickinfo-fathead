#!/usr/bin/env perl
  use strict;
  use warnings;
  use URI;
  use Web::Scraper;
  binmode STDOUT, ':encoding(UTF-8)';

  my $namespaceScraper = scraper {
      process ".members td > a", "namespaceScraper[]" => { body => 'TEXT', link => '@href' };
  };

  my $namespaces = $namespaceScraper->scrape( URI->new("http://msdn.microsoft.com/en-us/library/gg145045%28v=vs.110%29.aspx") );

  for my $namespace (@{$namespaces->{namespaceScraper}}) {
  	my $codeRes = scrapeClasses($namespace->{link});
  	if(!defined $codeRes->{classScraper}){
  		goDeeper($namespace->{link});
  	}else{
  		for my $code (@{$codeRes->{classScraper}}) {
          printLine($code->{className}, $code->{classDescription}, $namespace->{link});
  		}
  	}
  }

  sub scrapeClasses{
  	my($url) = @_;
  	my $classScraper = scraper {
      	process '//tr[contains(@data, "class")]', "classScraper[]" => scraper {
          	process "td > a", className => 'TEXT';
          	process "td > span", classDescription => 'TEXT';
	      };
    };
    return $classScraper->scrape( URI->new($url) );
  }

  sub goDeeper{
  	my($url) = @_;
		my $namespaceScraper = scraper {
    	  	process 'td > a', "namespaceScraper[]" => { body => 'TEXT', link => '@href' };
		};		
		my $namespaces = $namespaceScraper->scrape( URI->new($url) );
		for my $namespace (@{$namespaces->{namespaceScraper}}) {
			my $codeRes = scrapeClasses($namespace->{link});
			for my $code (@{$codeRes->{classScraper}}) {
        printLine($code->{className}, $code->{classDescription}, $namespace->{link});
      }
		}
  }

  sub printLine{
    my($className, $classDescription, $url) = @_;
    print join "\t", (
            $className, # title
            "A", # type
            "", # redirect
            "", # otheruses
            "", # categories
            "", # references
            "", # see_also
            "", # further_reading
            "", # external_links
            "", # disambiguation
            "", # images
            $classDescription, # abstract
            $url, # source_url
            "\n"
    );
  }
