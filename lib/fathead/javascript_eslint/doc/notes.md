# Notes

## Sources

### List of all rules

https://github.com/eslint/eslint.github.io/blob/master/_data/rules.yml
auto generated
uses categories

### Docs for all rules
https://github.com/eslint/eslint.github.io/tree/master/docs/rules
folder contains MD files.

## DDG Resources

- [Codio Editor](https://codio.com/home/projects)
- [DuckPAN Server](http://brigade-extreme.codio.io:5000/)

## Layout Draft

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
