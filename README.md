DuckDuckGo Fathead Plugins
=================================

See [DuckDuckHack](http://duckduckhack.com/) for an overview of the DuckDuckGo plugin system.

This repository is for contributing fathead plugins. Each fathead plugin produces a data file that gets used in a fuzzy keyword mapping, e.g. for getting a perl function reference when you search for [perl split](https://duckduckgo.com/?q=perl+split).

Fathead plugins are in beta and both the interface and testing procedure will improve over time. However, you can work away without worrying about what any changes might do to your plugins -- we'll take care of all that.

Contributing
------------

First off, thank you!


### Process

1) Pick [a fathead project](https://duckduckgo.uservoice.com/forums/5168-plugins/category/41839-fathead) (or add one) and comment that you're working on it.

2) Develop your plugin using the Structure below [in a fork](http://help.github.com/fork-a-repo/).

3) Test your plugin via Testing procedure below.

4) Submit a [pull request](http://help.github.com/send-pull-requests/).

Feel free to [ask questions](http://duckduckhack.com/#faq)!



### Structure

Each fathead plugin has its own directory. Some of the directories are in use on the live system, and some are still in development.

Each directory has a structure like this:

```txt

# This shell script is called to fetch the data. 
# Tmp files should go in a directory called download.
plugin/fetch.sh

# This is the script used to parse the data once it has been fetched. 
# .xx can be .pl, .py, .rb or .js depending on what language you use.
plugin/parse.xx

# This shell script is called to run the parser. 
plugin/parse.sh

# Please include any dependencies here,
# or other special instructions for people
# trying to run it.
plugin/README.txt

# This is the output file.
# Generally it should NOT be committed,
# but if it is small (<1MB) it is useful to do so.
plugin/output.txt

# This is an optional pointer to a URL in the cloud somewhere,
# which contains a zip of the data files to process.
plugin/data.url

# This is for testing.
# Put some good queries to test, one per line.
# You can explain them with comments above them.
plugin/queries.txt

# This is a file that gives meta information about the data source. 
plugin/meta.txt
```


### meta.txt format.

```txt
# This is the name of the source as people would refer to it,
# e.g. Wikipedia or PerlDoc -- gets displayed on Web site.
Name: jQuery API

# This is the base domain where the source pages are located.
# Get used to get the favicon.
Domain: api.jquery.com

# This is what gets put in quotes next to the source
# It can be blank if it is a source with completely 
# general info spanning many types of topics like Facebook.
Type: jQuery

# Whether the source is from MediaWiki (1) or not (0).
# Processing happens a bit differently on MediaWiki.
MediaWiki: 1

# Keywords uses to trigger (or prefer) the source over others.
# Can seperate multiple keywords with,
Keywords: jQuery
```

### General data file format

Please name the output file output.txt (tab delimited) but do not store the data file(s) in the repository (as noted above) unless it is under 1MB.

The output format from parse.xx depends on the type of content. In any case, it should be a tab delimited file, with one line per entry. Usually there is no need for newline characters, but if there is a need for some reason, escape them with a backslash like \\n. If you wanta newline displayed, use <br>.

The general output fields are as follows. Check out [https://duckduckgo.com/Perl](https://duckduckgo.com/Perl) for reference, which we will refer to in explaining the fields.


```perl
# REQUIRED: full article title, e.g. Perl.
# This should be unique across the data set.
my $title = $line[0] || '';

# REQUIRED: 
# A for article.
# D for disambiguation page.
# R for redirect.
my $type = $line[1] || '';

# Only for redirects, e.g. 
# an alias for a title such as
# a common misspelling or AKA.
# For example: Duck Duck Go -> DuckDuckGo.
# The format is the full title of the Redirect, e.g. DuckDuckGo.
my $redirect = $line[2] || '';

# Ignore.
my $otheruses = $line[3] || '';

# You can put the article in multiple categories, and category pages will be created automatically.
# E.g.: http://duckduckgo.com/c/Procedural_programming_languages
# You would do: Procedural programming languages\\n
# You can have several categories, separated by an escaped newline.
# Categories should generally end with a plural noun.
my $categories = $line[4] || '';

# Ignore.
my $references = $line[5] || '';

# You can reference related topics here, which get turned into links in the Zero-click Info box.
# On the perl example, e.g. Perl Data Language
# You would do: [[Perl Data Language]]
# If the link name is different, you could do [[Perl Data Language|PDL]]
my $see_also = $line[6] || '';

# Ignore.
my $further_reading = $line[7] || '';

# You can add external links that get put first when this article comes out.
# The canonical example is an official site, which looks like:
# [$url Official site]\\n
# You can have several, separated by an escaped newline though only a few will be used.
# You can also have before and after text or put multiple links in one like this.
# Before text [$url link text] after text [$url2 second link].\\n
my $external_links = $line[8] || '';

# Ignore.
my $disambiguation = $line[9] || '';

# You can reference an external image that we will download and reformat for display.
# You would do: [[Image:$url]]
my $images = $line[10] || '';

# This is the snippet info.
# It should generally be ONE readable sentence, ending in a period.
my $abstract = $line[11] || '';

# This is the full URL for the source.
# If all the URLs are relative to the main domain, 
# this can be relative to that domain.
my $source_url = $line[12] || '';

In all this may look like:

print OUT "$page\tA\t\t\t$categories\t\t$internal_links\t\t$external_links\t\t$images\t$abstract\t$relative_url\n";
```

There is a pre-process script that is run on this output, which:
* drops duplicates (on $title).
* reduces $abstract to one sentence.
* drops records that look like spam.
* normalizes spacing.
* makes sure the $abstract ends in a sentence.


### Programming data file format

For programming references in particular, the fields are a bit different because we like to show code blocks and do some additional transformations to make finding the documentation a bit easier.

```perl
# REQURIED: this is the name of the function.
my $page = $line[0] || '';

# Usually blank unless for something like JavaScript
my $namespace = $line[1] || '';

# REQUIRED: this is the target URL for more information.
my $url = $line[2] || '';

# SOME COMBO OF THESE IS REQUIRED.
# Look at https://duckduckgo.com/?q=perl+split
# The part in grey is the $synopsis and the stuff below is the $description
my $description = $line[3] || '';
my $synopsis = $line[4] || '';
my $details = $line[5] || '';

# usually blank
my $type = $line[6] || '';

# usually blank
my $lang = $line[7] || '';
```

Our programming reference parser then translates the above into the general format by compressing a lot of the fields into the $abstract field in various ways, e.g. synopsis gets put in a code block.


### Notes

1) There should be no duplicates in the $page (first) variable. If you have multiple things named the same things you have a number of options, e.g. a) make disambiguation pages; b) put everything in one snippet; c) pick the most general one.
