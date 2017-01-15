# Notes

## Intro

DuckDuckGo [Instant Answer Page](https://duck.co/ia/view/eslint) for [ESLint](http://eslint.org/).

### DDH Docs
- Main page: https://docs.duckduckhack.com/
- Fathead intro: https://docs.duckduckhack.com/resources/fathead-overview.html

### Manual Testing
For manual testing, first install **npm dependencies** from  https://www.npmjs.com/package/, then run **fetch** and **parse** scripts:

```
$ npm install
$ bash fetch.sh
$ bash parse.sh
```

### Test on Codio
Follow the [instructions](https://docs.duckduckhack.com/welcome/setup-dev-environment.html) and open the virtual machine on [Codio.com](https://codio.com/home/projects)

## Rule Sources

### Rule Summary

This URL can provide a YAML file containing a summary of all rules: https://github.com/eslint/eslint.github.io/blob/master/_data/rules.yml.

- The file sorts the rules by **category**.
- Following the docs, this file gets **auto-generated** from sources.

### Rule Detail
This URL contains a MD file for each rule with further details and examples:
https://github.com/eslint/eslint.github.io/tree/master/docs/rules

## Output

### Layout Draft
Quick draft how the content should get displayed on the search page.

> **no-console**
>
> Disallow the use of `console`.
>
> In JavaScript that is designed to be executed in the browser, it’s considered a best practice to avoid using methods on `console`. Such messages are considered to be for debugging purposes and therefore not suitable to ship to the client. In general, calls using `console` should be stripped before being pushed to production.
>
> ```js
> console.log("Made it here.");
> console.error("That shouldn't have happened.");
> ```
>
> &#x2714; Recommended: <b>yes</b> | <b>&#x2192;</b> Fixable: <b>no</b>
