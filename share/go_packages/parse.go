package main

import (
	"fmt"
	"os"
	"regexp"
	"log"
	"strings"
	"net/http"
	"code.google.com/p/go.net/html"
)

type Package struct {
	Title string
	Type string
	Redirect string
	Abstract string
	Source string
	Alt string
}

type Attr struct {
	Key string
	Val string
}

const (
	Link = "http://golang.org/pkg/"
	Output = "output.txt"
)

func (pkg *Package) ToArray() []string {
	return []string{
		pkg.Title, 
		pkg.Type, 
		pkg.Redirect,
		"", 
		"", 
		"", 
		"", 
		"", 
		"",
		"", 
		"", 
		pkg.Abstract, 
		pkg.Source,
	}
}

// Make sure that the attributes that we want match with the attributes found in the element.
func MatchAttr(n *html.Node, attr []Attr) bool {
	matches := 0
	for _, pair := range attr {
		for _, a := range n.Attr {
			if pair.Key == a.Key && pair.Val == a.Val {
				matches++
			}
		}
	}
	
	return matches == len(attr)
}

// This function finds all the instances of a given element.
// It can also take get elements that have specific attributes.
func FindAll(el string, n *html.Node, nodes []*html.Node, attr []Attr) []*html.Node {
	if n.Type == html.ElementNode && n.Data == el {
		// Check if we have to check the attributes, too.
		if len(attr) > 0 {
			if MatchAttr(n, attr) {
				nodes = append(nodes, n)
			}
		} else {
			nodes = append(nodes, n)
		}
	}

	for c := n.FirstChild; c != nil; c = c.NextSibling {
		nodes = FindAll(el, c, nodes, attr)
	}

	return nodes
}

// This function returns the attribute that was given.
func GetAttr(attr string, n *html.Node) string {
	for _, a := range n.Attr {
		if a.Key == attr {
			return a.Val
		}
	}
	return ""
}

// Get the things that we want from a given node.
// It's currently locked to find stuff from the "td" tag.
func PackageDetails(n *html.Node, attr Attr, fn func(*html.Node) Package) []Package {
	result := []Package{}

	nodes := FindAll("td", n, []*html.Node{}, []Attr{attr})
	for _, node := range nodes {
		result = append(result, fn(node))
	}

	return result
}

// Check whether a package should be of type "D" (for disambiguation) or type "A" (for article).
func DetermineType(pkgs []Package) []Package {
	for i, pkg := range pkgs {
		is_subdir := regexp.MustCompile(fmt.Sprintf("^%v/.+", pkg.Title))

		// If the abstract is blank, it is a directory.
		if pkg.Abstract == "" {
			disambig := [][]string{}
			for _, candidate := range pkgs {
				if is_subdir.MatchString(candidate.Title) {
					disambig = append(disambig, []string{candidate.Title, candidate.Abstract})
				}
			}
			// Format the disambiguation result.
			pkgs[i].Abstract = MakeDisambig(disambig)
			pkgs[i].Type = "D"
			// Remove the source if it's a disambiguation because it won't matter.
			pkgs[i].Source = ""
		} else {
			pkgs[i].Type = "A"
		}
	}

	return pkgs
}

// Format the array of disambiguations that DuckDuckGo can read.
func MakeDisambig(disambig [][]string) string {
	result := ""
	for _, d := range disambig {
		result += fmt.Sprintf("*[[%v]] %v\\n", d[0], d[1])
	}
	return result
}

// Add some common package queries, too.
// Ex. "cookiejar" should redirect to "net/http/cookiejar"
func AddRedirects(pkgs []Package) []Package {
	// We put everything in a map first because it's possible to have disambiguations in here.
	result := map[string][]Package{}

	for _, pkg := range pkgs {
		if pkg.Title != pkg.Alt {
			// Check if it already exists in our map.
			_, ok := result[pkg.Alt]
			pkg.Type = "R"
			if ok {
				result[pkg.Alt] = append(result[pkg.Alt], pkg)
			} else {
				result[pkg.Alt] = []Package{pkg}
			}
		}
	}
	
	// Now go through each key-value pair to check if there are any disambiguations.
	for _, v := range result {
		// Using the first package would do.
		first := v[0]
		if len(v) > 1 {
			// If there are multiple packages, pass it to the MakeDisambig function.
			disambig := [][]string{}
			for _, pkg := range v {
				disambig = append(disambig, []string{pkg.Title, pkg.Abstract})
			}
			first.Abstract = MakeDisambig(disambig)
			first.Type = "D"
			// It shouldn't have a source if it's a disambig.
			first.Source = ""
		} else {
			// If there is only one, just remove the abstract and add the redirect.
			first.Redirect = first.Title
		}
		first.Title = first.Alt

		pkgs = append(pkgs, first)
	}
	return pkgs
}

// Copy over some missing things from b to a.
func Update(a []Package, b []Package) []Package {
	for i, _ := range a {
		a[i].Abstract = b[i].Abstract
	}

	return a
}

// Format the output as specified.
func FormatOutput(pkgs []Package) string {
	result := ""

	for _, pkg := range pkgs {
		result += fmt.Sprintln(strings.Join(pkg.ToArray(), "\t"))
	}

	return result
}

func main() {
	// Fetch the HTML.
	resp, err := http.Get(Link)

	if err != nil {
		log.Fatalf("There was an error with fetching. %v\n", err)
		return
	}
	defer resp.Body.Close()

	// Parse the HTML that we got.
	parsed, err := html.Parse(resp.Body)

	if err != nil {
		log.Fatalf("There was an error with parsing: %v\n", err)
		return
	}

	// Get the package names.
	pkgs := PackageDetails(parsed, Attr{"class", "name"}, func(n *html.Node) Package {
		pkg := new(Package)
		pkg.Title = GetAttr("href", n.LastChild)
		pkg.Source = Link + GetAttr("href", n.LastChild)
		pkg.Alt = n.LastChild.FirstChild.Data

		// Let's remove the trailing slash in our title, too.
		// Ex. "unicode/utf8/" turns into "unicode/utf8"
		pkg.Title = pkg.Title[:len(pkg.Title)-1]
		return *pkg
	})

	// Get the abstracts.
	abstracts := PackageDetails(parsed, Attr{"style", "width: auto"}, func(n *html.Node) Package {
		pkg := new(Package)
		// Check if we found a child (the text node).
		// If we didn't find any, it just returns a Package with empty attributes.
		if(n.FirstChild != nil) {
			pkg.Abstract = n.FirstChild.Data
		}
		return *pkg
	})

	// Copy over the Abstracts from one array to the other.
	pkgs = Update(pkgs, abstracts)

	// Set the categories of each entry.
	// Determine which ones are articles and which ones are disambiguations.
	pkgs = DetermineType(pkgs)

	// Add redirects.
	// Some packages might be searched in a different way.
	pkgs = AddRedirects(pkgs)

	// Print it to a file.
	output, err := os.Create(Output)

	if err != nil {
		log.Fatalf("There was an error with creating %v. We got %v.\n", Output, err)
		return
	}
	defer output.Close()

	result := FormatOutput(pkgs)
	output.WriteString(result)
}
