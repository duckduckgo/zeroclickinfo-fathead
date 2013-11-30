package main

import (
	"fmt"
	"io/ioutil"
	"strings"
	"regexp"
	"code.google.com/p/go.net/html"
)

type Package struct {
	Name, Link, Abstract, Category string
}

type Attr struct {
	Key, Val string
}

const (
	HTML = "packages.html"
	Link = "http://golang.org/pkg/"
)

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

func DetermineCategories(pkgs []Package) {
	// Use the categories attribute as a temporary variable.
	for i, pkg := range pkgs {
		is_subdir := regexp.MustCompile(fmt.Sprintf("^%v/.+", pkg.Name))

		// If the abstract is blank, it is a directory.
		if pkg.Abstract == "" {
			disambig := [][]string{}
			for _, candidate := range pkgs {
				if is_subdir.MatchString(candidate.Link) {
					disambig = append(disambig, []string{candidate.Link, candidate.Abstract})
				}
			}
			// Format the disambiguation result.
			pkgs[i].Abstract = MakeDisambig(disambig)
			pkgs[i].Category = "D"
		} else {
			pkgs[i].Category = "A"
		}
	}
}

func MakeDisambig(disambig [][]string) string {
	result := ""
	for _, d := range disambig {
		result += fmt.Sprintf("*[[%v]] %v\\n", d[0], d[1])
	}
	return result
}

func main() {
	// Read the HTML file that we downloaded.
	file, err := ioutil.ReadFile(HTML)

	if err != nil {
		fmt.Printf("There was an error with reading. %v\n", err)
		return
	}

	// Parse the HTML that we got.
	parsed, err := html.Parse(strings.NewReader(string(file)))

	if err != nil {
		fmt.Printf("There was an error with parsing: %v\n", err)
		return
	}

	// Get the package name and the link from the table.
	pkgs := PackageDetails(parsed, Attr{"class", "name"}, func(n *html.Node) Package {
		return Package{n.LastChild.FirstChild.Data, GetAttr("href", n.LastChild), "", ""}
	})

	// Get the abstract from the table.
	abstracts := PackageDetails(parsed, Attr{"style", "width: auto"}, func(n *html.Node) Package {
		if(n.FirstChild != nil) {
			return Package{"", "", n.FirstChild.Data, ""}
		}
		return Package{"", "", "", ""}
	})

	// Let's copy over the abstracts to the pkgs slice.
	for i, pkg := range pkgs {
		pkgs[i].Abstract = abstracts[i].Abstract
		pkgs[i].Link = pkg.Link[:len(pkg.Link)-1]
	}

	// Set the categories of each entry.
	DetermineCategories(pkgs)

	// Add redirects.
	AddRedirects(pkgs)

	for _, pkg := range pkgs {
		fmt.Printf("%v\t%v\t%v\t%v\t%v\n", pkg.Link, Link + pkg.Link, pkg.Abstract, pkg.Category, pkg.Name)
	}
}
