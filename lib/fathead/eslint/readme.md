# Readme

[Instant Answer Fathead](https://duck.co/ia/view/eslint) for [ESLint](http://eslint.org/).

## Dependencies

Node >= v7.0

## Useful References
- DDH docs main page: https://docs.duckduckhack.com/
- DDH docs fathead intro: https://docs.duckduckhack.com/resources/fathead-overview.html

## Testing

### Test Environment on Codio
Test environment available at [Codio.com](https://codio.com/home/projects). Follow the  [DDH instructions](https://docs.duckduckhack.com/welcome/setup-dev-environment.html) to set it up initially.

### Manual Testing
For manual testing, first **npm install** dependencies, then run **fetch** and **parse** scripts:

```
$ npm install
$ bash fetch.sh
$ bash parse.sh
```

## Fathead Sources

### Rule Summary
https://raw.githubusercontent.com/eslint/eslint.github.io/master/_data/rules.yml

This URL can provide a YAML file containing a summary of all rules.
- The file sorts the rules by **category**.
- Following the docs, this file gets **auto-generated** from sources.

### Rule Detail
https://github.com/eslint/eslint.github.io/tree/master/docs/rules

This URL contains a MD file for each rule with further details and examples:

## Fathead Output

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

## Road Map
### V 1.0
#### Fetch
- Fetch `rules.yml` as **summary**

#### Parse
- Parse **summary**, display rule **names** and **descriptions**
- Basic markdown support for **descriptions**

### V 1.1
#### Fetch
- Fetch `rules/` folder as **details**

#### Parse
- Use *fixable* and *recommended* properties as **categories**
- Parse MD file with **details** for each rule. Extract content of first two sections.
- Markdown support for rule details (including code blocks)
