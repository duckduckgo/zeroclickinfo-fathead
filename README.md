DuckDuckGo Fathead Plugins
=================================

See [DuckDuckHack](http://duckduckhack.com/) for an overview of the DuckDuckGo plugin system.

This repository is for contributing fathead plugins. Each fathead plugin produces a data file that gets used in a fuzzy keyword mapping, e.g. for getting a perl function reference when you search for [perl split](https://duckduckgo.com/?q=perl+split).

Fathead plugins are in beta and both the interface and testing procedure will improve over time. However, you can work away without worrying about what any changes might do to your plugins -- we'll take care of all that.

### Example
![hello world example](https://s3.amazonaws.com/ddg-assets/docs/fathead_example.png)

Contributing
------------

First off, thank you!


### Process

1) Pick [a fathead project](https://duckduckhack.uservoice.com/forums/5168-instant-answer-plugin-ideas-for-duckduckgo/category/41839-fathead) (or add one) and comment that you're working on it.

2) Develop your plugin using the Structure below [in a fork](http://help.github.com/fork-a-repo/).

3) Submit a [pull request](http://help.github.com/send-pull-requests/).

Feel free to [ask questions](http://duckduckhack.com/#faq)!



### Structure

Each fathead plugin has its own directory. Some of the directories are in use on the live system, and some are still in development.

Each directory has a structure like this:

```txt

# This is a Perl file that lists some meta information about the plugin.
# See the Meta file section for more information
lib/DDG/Fathead/PlugIn.pm

# This shell script is called to fetch the data.
# Tmp files should go in a directory called download.
share/plugin/fetch.sh

# This is the script used to parse the data once it has been fetched.
# .xx can be .pl, .py, .rb or .js depending on what language you use.
share/plugin/parse.xx

# This shell script is called to run the parser.
share/plugin/parse.sh

# Please include any dependencies here,
# or other special instructions for people
# trying to run it.
share/plugin/README.txt

# This is the output file.
# Generally it should NOT be committed,
# but if it is small (<1MB) it is useful to do so.
share/plugin/output.txt

# This is an optional pointer to a URL in the cloud somewhere,
# which contains a zip of the data files to process.
share/plugin/data.url
```


### Meta file

We use Perl packages to enumerate and work with plugins, so part of the plugin process is including a simple Perl file. You don't actually need to know any Perl for this! Here's an annotated example from the Hello World plugin.

In the interest of keeping the code clean, please remove the annotations in your own file.

```perl
# Replace "HelloWorld" with the name of your plugin using CamelCase
package DDG::Fathead::HelloWorld;

# All plugins should have this line
use DDG::Fathead;

# This is an example search for the main use case.
primary_example_queries "hello world perl";

# This is a list of secondary use cases for the plugin.
secondary_example_queries
    "javascript hello world",
    "hello world in c";

# A brief description of the plugin
description "Hello World programs in many program languages";

# A unique name for the plugin
name "HelloWorld";

# Just leave this blank at first. We'll add it once we've got the plugin working.
# So it should be: icon_url "";
icon_url "/i/www.github.com.ico";

# The name of the source. Appears as "More at {source}" in the ZCI box.
source "GitHub";

# This is kind of meta. It's the link to the main files of this plugin
# i.e. the ones in the share/fathead/plugin/ folder.
code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/hello_world";

# A list of topics that are relevant to this plugin. Choose around 1-3.
# See supported topics:
# https://github.com/duckduckgo/duckduckgo/blob/master/lib/DDG/Meta/Information.pm
topics "geek", "programming";

# The most relevant category for this plugin. Choose only 1.
# See supported categories:
# https://github.com/duckduckgo/duckduckgo/blob/master/lib/DDG/Meta/Information.pm
category "programming";

# Your information so we can give you credit!
# See supported types:
# https://github.com/duckduckgo/duckduckgo/blob/master/lib/DDG/Meta/Information.pm
attribution
    twitter => ['https://twitter.com/jperla', 'jperla'],
    web => ['http://www.jperla.com/blog', 'Joseph Perla'];

1;
```

### General data file format

Please name the output file output.txt (tab delimited) but do not store the data file(s) in the repository (as noted above) unless it is under 1MB.

The output file needs to use UTF-8 encoding so we can process it. Please make sure you write your parse scripts accordingly or we'll probably run into some problems getting it integrated.

The output format from parse.xx depends on the type of content. In any case, it should be a tab delimited file, with one line per entry. Usually there is no need for newline characters, but if there is a need for some reason, escape them with a backslash like \\n. If you wanta newline displayed, use &lt;br&gt;

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
# You would do: Procedural programming languages\n
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
# [$url Official site]\n
# You can have several, separated by an escaped newline though only a few will be used.
# You can also have before and after text or put multiple links in one like this.
# Before text [$url link text] after text [$url2 second link].\n
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

print OUT "$title\t$type\t\t\t$categories\t\t$see_also\t\t$external_links\t\t$images\t$abstract\t$source_url\n";
```

There is a pre-process script that is run on this output, which:
* drops duplicates (on $title).
* reduces $abstract to one sentence.
* drops records that look like spam.
* normalizes spacing.
* makes sure the $abstract ends in a sentence.


#### Code blocks

If you want to include a code snippet or another pre-formatted example in the
abstract, like the [perl](https://duckduckgo.com/?q=perl+open) Fathead, wrap
the code block like this:

```html
<pre><code>code block goes here</code></pre>
```

### Notes

1) There should be no duplicates in the $page (first) variable. If you have multiple things named the same things you have a number of options, e.g. a) make disambiguation pages; b) put everything in one snippet; c) pick the most general one.
