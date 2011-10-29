DuckDuckGo ZeroClickInfo FatHeads
=================================

About
-----

See [the contribution wiki](https://github.com/duckduckgo/duckduckgo/wiki) for a general overview on contributing to DuckDuckGo.

This repository is for contributing static, keyword based content to 0-click, e.g. getting a perl function reference when you search for perl split. That is, each fathead project will essentially be a datafile that relates keywords to 0-click content.

We also maintain a list of [requested fathead projects](https://github.com/duckduckgo/duckduckgo/wiki/Fathead), but whatever you want to attempt is welcome.


Contributing
------------

First off, thank you!


### Process

1) Make sure you're in the right place. This repo is for generating data files to be used statically in 0-click. Generally it involves downloading content and then re-formatting it. If you need to do something in real-time from a third-paty API you probably want the [spice repo](https://github.com/duckduckgo/zeroclickinfo-spice). For Perl stand-alone goodies that get generated on the fly based on the query you probably want the [goodies repo](https://github.com/duckduckgo/zeroclickinfo-goodies).

2) Develop project using the Structure below in either a fork or a branch (if a collaborator).

3) Test goodie via Testing procedure below.

4) Submit a pull request.

Feel free to ask questions!



### Structure

Each fathead project has its own directory. Some of the directories are in use on the live system, and some are still in development.

Each directory has a structure like this:

```txt

# This shell script is called to fetch the data. 
# Tmp files should go in a directory called download.
project/fetch.sh

# This is the script used to parse the data once it has been fetched. 
# .xx can be .pl or .py or .js depending on what language you use.
project/parse.xx

# This shell script is called to run the parser. 
project/parse.sh

# Please include any dependencies here,
# or other special instructions for people
# trying to run it.
project/README.txt

# This is the output file.
# Generally it should NOT be committed,
# but if it is small (<1MB) it is useful to do so.
project/output.txt

# This is an optional pointer to a URL in the cloud somewhere,
# which contains a zip of the data files to process.
project/data.url

# This is for testing.
# Put some good queries to test, one per line.
# You can explain them with comments above them.
project/queries.txt

# This is a file that gives meta information about the data source. 
project/meta.txt
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

The output format from parse.xx depends on the type of content. In any case, it should be a tab delimited file, with one line per entry. Usually there is no need for newline characters, but if there is a need for some reason, escape them with a backslash like \\n.

The general output fields are as follows. Check out http://duckduckgo.com/Perl for reference, which we will refer to in explaining the fields.

```perl
# REQUIRED: full article title, e.g. Perl.
my $title = $line[0] || '';

# REQUIRED: A for article.
my $type = $line[1] || '';

# Only for redirects -- ask.
my $redirect = $line[2] || '';

# Ignore.
my $otheruses = $line[3] || '';

# You can put the article in multiple categories, and category pages will be created automatically.
# E.g.: http://duckduckgo.com/c/Procedural_programming_languages
# You would do: Procedural programming languages\\n
# You can have several categories, separated by an escaped newline.
my $categories = $line[4] || '';

# Ignore.
my $references = $line[5] || '';

# You can reference related topics here, which get turned into links in the 0-click box.
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
my $abstract = $line[11] || '';

# This is the full URL for the source.
# If all the URLs are relative to the main domain, 
# this can be relative to that domain.
my $source_url = $line[12] || '';

In all this may look like:

print OUT "$page\tA\t\t\t$categories\t\t$internal_links\t\t$external_links\t\t$images\t$abstract\t$relative_url\n";
```


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