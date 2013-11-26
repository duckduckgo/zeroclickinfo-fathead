package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"code.google.com/p/go.net/html"
)

type Data struct {
	Title, Type, Categories, See, Link, Images, Abstract, Source string
}

type Node struct {
	Name string
	Attr []string
}

var find map[string]Node = Node{} 

// Let's create a function that will go through all the tags.
func CheckNode(n *html.Node) {
	if n.Type == html.ElementNode && format[n.Data] == nil {
		for _, f := range format[n.Data] {
			
		}
	}

	for c := n.FirstChild; c != nil; c = c.NextSibling {
		CheckNode(c, format)
	}
}

func main() {
	// Read the URL that we want.
	resp, err := http.Get("http://golang.org/pkg/")

	if err != nil {
		fmt.Printf("There was an error with reading. %v\n", err)
		return
	}

	// Make sure we close the response body.
	defer resp.Body.Close()

	// Parse the HTML that we got.
	p, err := html.Parse(resp.Body)

	if err != nil {
		fmt.Printf("There was an error with parsing: %v\n", err)
		return
	}

	// Choose the nodes that we want.
	CheckNode(p)
}
