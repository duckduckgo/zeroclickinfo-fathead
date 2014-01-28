#!/usr/bin/env perl
  use strict;
  use warnings;
  use URI;
  use Web::Scraper;
  binmode STDOUT, ':encoding(UTF-8)';

  my %namespacesPerName;

  my $namespaceScraper = scraper {
      process ".members td > a", "namespaceScraper[]" => { body => 'TEXT', link => '@href' };
  };

  my $namespaces = $namespaceScraper->scrape( URI->new("http://msdn.microsoft.com/en-us/library/gg145045%28v=vs.110%29.aspx") );

  for my $namespace (@{$namespaces->{namespaceScraper}}) {
  	my $classes = scrapeClasses($namespace->{link});
  	if(!defined $classes->{classScraper}){
  		# goDeeper($namespace->{link});
  	}else{
  		for my $class (@{$classes->{classScraper}}) {
          # addLine($class->{name}, $class->{description}, $class->{link});
          addLine($class);
  		}
  	}
    for my $groupedClassName ( keys %namespacesPerName ) {
      printGroupedClassName($groupedClassName);
    }
  }

  sub scrapeClasses{
  	my($url) = @_;
  	my $classScraper = scraper {
      	process '//tr[contains(@data, "class")]', "classScraper[]" => scraper {
          	process "td > a", name => 'TEXT';
            process "td > a", link => '@href';
          	process "td > span", description => 'TEXT';
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
			my $classes = scrapeClasses($namespace->{link});
			for my $class (@{$classes->{classScraper}}) {
        addLine($class->{name}, $class->{description}, $class->{link});
      }
		}
  }

  sub addLine{
    my($class) = @_;
    my $className = $class->{name}
    if($className =~ /(\w+)(<(.*)>)*/){
      # if($2){
      #   print "\n stripped: $className to $1";    
      # }else{
      #   print "\n found simple: $1";    
      # }
      my $strippedClassName = $1;
      if(!exists $namespacesPerName{$strippedClassName}){
        print "\ncreating new array for $strippedClassName";
        $namespacesPerName{$strippedClassName} = [];
      }
      print "\npushing element in $strippedClassName: $className";
      push @{$namespacesPerName{$strippedClassName}}, $class;
    }else{
      # print "\n no dice: " . $className;
    }
    
  }

  sub printGroupedClassName{
    my(@groupedClassName) = @_;
    my $arrSize = @groupedClassName;
    if($arrSize > 1){
      print "\nfound more than one for : $groupedClassName[0]";
    }
    else{
      print "\nfound only one for: $groupedClassName[0]"
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
