#### Install [Go](http://golang.org/) 1.2
`sudo apt-get install golang` or follow the instructions on http://golang.org/doc/install

#### Set $GOPATH
```shell
mkdir $HOME/go
export GOPATH=$HOME/go
```

#### Get dependencies
`go get "code.google.com/p/go.net/html"`

#### Run the code
`cd` into the `share` directory and run `go run parse.go`