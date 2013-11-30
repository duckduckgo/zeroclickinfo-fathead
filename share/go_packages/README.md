#### Install [Go](http://golang.org/).
`sudo apt-get install golang`

#### Set $GOPATH.
```
mkdir $HOME/go
export GOPATH=$HOME/go
```

#### Get dependencies.
`go get "code.google.com/p/go.net/html"`

#### Run the code.
`go run parse.go`