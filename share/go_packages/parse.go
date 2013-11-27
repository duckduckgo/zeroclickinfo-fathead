package main

import (
	"fmt"
	"io/ioutil"
	"strings"
	"container/list"
	"code.google.com/p/go.net/html"
)

type Data struct {
	Title, Type, Categories, See, Link, Images, Abstract, Source string
}

type Attr struct {
	Key, Val string
}

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

// Helps us find an element that we're looking for.
func FindAll(el string, n *html.Node, l *list.List, attr []Attr) {
	if n.Type == html.ElementNode && n.Data == el {
		// Means we also take the attributes into consideration.
		if len(attr) > 0 {
			if MatchAttr(n, attr) {
				l.PushBack(n)
			}
		} else {
			l.PushBack(n)
		}
	}

	for c := n.FirstChild; c != nil; c = c.NextSibling {
		FindAll(el, c, l, attr)
	}
}

func GetAttr(attr string, n *html.Node) string {
	for _, a := range n.Attr {
		if a.Key == attr {
			return a.Val
		}
	}
	return ""
}

func GetText(n *html.Node) string {
	if n != nil && n.FirstChild.Type == html.TextNode {
		return n.FirstChild.Data
	}
	return ""
}

func main() {
	// Read the file that we want.
	file, err := ioutil.ReadFile("packages.html")

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

	// Choose the nodes that we want.
	// Find the <tr> tags first.
	packages := list.New()
	FindAll("td", parsed, packages, []Attr{
		Attr{"class", "name"},
	})

	for e := packages.Front(); e != nil; e = e.Next() {
		node := e.Value.(*html.Node)

		fmt.Println(node)
	}

	abstracts = list.New()
	FindAll("td", parsed, abstracts, []Attr{
		Attr{"style", "width: auto"},
	})

	for e := abstracts.Front(); e != nil; e = e.Next() {
		node := e.Value.(*html.Node)
		
		fmt.Println(node)
	}
}
