package main

import (
	"bufio"
	"flag"
	"html"
	"io"
	"log"
	"os"
	"regexp"
	"strings"
)

var (
	extensionRE = regexp.MustCompile("!&lt;div id=&quot;.+&quot;&gt; (.+)&lt;/div&gt;")
	linkRE      = regexp.MustCompile(`\[\[(.+\|)?(.+?)\]\]`)
)

var outputFile *string = flag.String("output", "output.txt", "output file")
var wikiFile *string = flag.String("wikiinput", "download/data", "input Wikipedia data")

func main() {
	flag.Parse()
	lines := make(chan string)
	formats := make(chan fileFormat)
	uniqFormats := make(chan fileFormat)

	go readData(lines)
	go parse(lines, formats)
	go uniq(formats, uniqFormats)
	output(uniqFormats)
}

type fileFormat struct {
	ext string
	use []string
}

func output(formats chan fileFormat) {
	file, err := os.OpenFile(*outputFile, os.O_WRONLY|os.O_CREATE, 0644)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	for f := range formats {
                // Redirect ".pdf" to "pdf"
                redirect := "." + f.ext + "\tR\t" + f.ext + "\t\t\t\t\t\t\t\t\t\t"

		out := []string{
			f.ext,           // title (required)
			"A",             // type (required)
			"",              // redirect
			"",              // otheruses (ignore)
			"",              // categories
			"",              // references (ignore)
			"",              // see also
			"",              // further reading (ignore)
			"",              // external links
			"",              // disambiguation (ignore)
			"",              // images
			abstract(f.use), // abstract
			"",              // source url
		}
		_, err = file.WriteString(strings.Join(out, "\t") + "\n" + redirect + "\n")
		if err != nil {
			log.Fatal(err)
		}
	}
}

func abstract(uses []string) string {
	var a string
	// Basic "a" vs "an"; not entirely correct, but close.
	if !strings.HasPrefix(uses[0], "for ") {
		switch uses[0][0] {
		case 'a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U':
			a = "an "
		default:
			a = "a "
		}
	}
	return "A file with this extension may be " + a + strings.Join(uses, ", ") + "."
}

func uniq(formats chan fileFormat, uniqFormats chan fileFormat) {
	uniq := make(map[string]fileFormat)
	for f := range formats {
		if existing, ok := uniq[f.ext]; ok {
			existing.use = append(existing.use, f.use...)
			uniq[f.ext] = existing
		} else {
			uniq[f.ext] = f
		}
	}

	for _, f := range uniq {
		uniqFormats <- f
	}
	close(uniqFormats)
}

func cleanWikiHTML(s string) string {
	return strings.TrimSpace(html.UnescapeString(wikilink(s)))
}

func wikilink(b string) string {
	return linkRE.ReplaceAllString(b, "$2")
}

const (
	parseExtension = iota
	parseDescription
	parseApplication
)

func parse(lines chan string, formats chan fileFormat) {
	state := parseExtension

	// The format we're currently parsing.
	format := fileFormat{}

	for line := range lines {
		switch state {
		case parseExtension:
			// Determine if this line specifies an extension.
			if m := extensionRE.FindStringSubmatch(line); m != nil {
				ext := m[1]

				// Some extensions are used by multiple things. If this is a new extension,
				// send the old one.
				if thisExt := cleanWikiHTML(ext); thisExt != format.ext {
					if format.ext != "" && len(format.use) > 0 {
						formats <- format
					}
					format = fileFormat{ext: thisExt}
				}
				state = parseDescription
			}
		case parseDescription:
			// Collect the possible uses for this extension.
			// This is [1:] to omit a leading "|" on the line.
			if description := line[1:]; len(description) > 0 {
				format.use = append(format.use, cleanWikiHTML(description))
			}
			state = parseApplication
		case parseApplication:
			if app := line[1:]; len(app) > 0 && app != "-" {
				appDescription := "for " + cleanWikiHTML(app)
				// Check if there was a description.
				if last := len(format.use) - 1; last < 0 {
					format.use = append(format.use, appDescription)
				} else {
					format.use[last] += " " + appDescription
				}
			}
			state = parseExtension
		}
	}
	close(formats)
}

func readData(lines chan string) {
	wikiData, err := os.Open(*wikiFile)
	if err != nil {
		log.Fatal(err)
	}
	defer wikiData.Close()

	b := bufio.NewReader(wikiData)

	for {
		line, isPrefix, err := b.ReadLine()
		if err == io.EOF {
			break
		}

		if err != nil {
			log.Fatal(err)
		}

		if !isPrefix {
			lines <- string(line)
		} else {
			log.Panic("abnormally long input line -- check your data")
		}
	}
	close(lines)
}
