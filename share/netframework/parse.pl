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
  		goDeeper($namespace->{link});
  	}else{
  		for my $class (@{$classes->{classScraper}}) {
          addLine($class);
  		}
  	}
  }

  for my $groupedClassName ( keys %namespacesPerName ) {
    printGroupedClassName($groupedClassName, @{$namespacesPerName{$groupedClassName}});
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
        addLine($class);
      }
		}
  }

  sub addLine{
    my($class) = @_;
    my $className = $class->{name};
    if($className =~ /(\w+)(<(.*)>)*/){
      my $strippedClassName = $1;
      if(!exists $namespacesPerName{$strippedClassName}){
        $namespacesPerName{$strippedClassName} = [];
      }
      push @{$namespacesPerName{$strippedClassName}}, $class;
    }
  }

  sub printGroupedClassName{
    my($groupedClassName, @groupedClasses) = @_;
    my $arrSize = @groupedClasses;
    
    if($arrSize > 1){
      printDisambiguation($groupedClassName, @groupedClasses);
    }
    else{
      my $class = $groupedClasses[0];
      printLine($class->{name}, $class->{description}, $class->{link});
    }
  }

  sub printDisambiguation{
    my($name, @disambiguations) = @_;
    printDisambiguationLine($name, @disambiguations);
    for my $class (@disambiguations){
      printDisambiguationSubLine($name, $class->{name}, $class->{description}, $class->{link});
    }
  }

  sub printDisambiguationLine{
    my($name, @disambiguations) = @_;
    my $disambiguationString;
    for my $disambiguation (@disambiguations){
      $disambiguationString = $disambiguationString . "*[[$disambiguation->{name}]] $disambiguation->{description}" . '\n';
    }
    print join "\t", (
            $name, # title
            "D", # type
            "", # redirect
            "", # otheruses
            "", # categories
            "", # references
            "", # see_also
            "", # further_reading
            "", # external_links
            $disambiguationString, # disambiguation
            "", # images
            "", # abstract
            "", # source_url
            "\n"
    );
  }

  sub printDisambiguationSubLine{
    my($disambiguationName, $className, $classDescription, $url) = @_;
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
