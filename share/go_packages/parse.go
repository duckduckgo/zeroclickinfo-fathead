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

// Helps us find an element that we're looking for.
func FindElements(el string, n *html.Node, l *list.List) {
	if n.Type == html.ElementNode && n.Data == el {
		l.PushBack(n)
	}

	for c := n.FirstChild; c != nil; c = c.NextSibling {
		FindElements(el, c, l)
	}
}

func GetAttr(attr string, n *html.Node) string {
	for _, a := range n.Attr {
		if a.Key == attr {
			return a.Val, nil
		} 
	}
	return ""
}

func PrintElements(l *list.List) {
	for e := l.Front(); e != nil; e = e.Next() {
		node := e.Value.(*html.Node)

		fmt.Println(GetAttr("href", node))
		// fmt.Println(node.FirstChild.Data)
	}
}

func main() {
	// Read the file that we want.
	file, err := ioutil.ReadFile("packages.html")

	if err != nil {
		fmt.Printf("There was an error with reading. %v\n", err)
		return
	}

	// Parse the HTML that we got.
	p, err := html.Parse(strings.NewReader(string(file)))

	if err != nil {
		fmt.Printf("There was an error with parsing: %v\n", err)
		return
	}

	// Choose the nodes that we want.
	// Find the <tr> tags first.
	tr := list.New()
	FindElements("tr", p, tr)

	for e := tr.Front(); e != nil; e = e.Next() {
		node := e.Value.(*html.Node)

		// Now let's look for the <a> tags inside.
		a := list.New()
		FindElements("a", node, a)

		PrintElements(a)
	}
}
