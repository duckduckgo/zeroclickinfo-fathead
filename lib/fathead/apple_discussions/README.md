Apple Discussions Fathead
===

1. [Fetch the data](#fetch-the-data)
2. [Parse the data](#parse-the-data)

## Fetch the data

> To start, make sure you have `cd` into this directory and run the `npm i` command before running the base scripts!

The following command will download all the HTML from the set of urls in the *urls.txt* file in the *apple_discussions/* directory. Make sure this file exists.

```
bash ./fetch.sh
```

## Parse the data

The following command will parse the html files and create a file called *./output.json*.

```
bash ./parse.sh
```

## Statistics

The following command will output the parse statistics if the *./output.json* file exists.

```
node ./stats.js
```

