# Contributing an Instant Answer to DuckDuckGo.com

**The DuckDuckHack community and DuckDuckGo staff are working together on a mission to create the best search engine for programmers. To fully focus there, we are only accepting Pull Requests and Issues related to the [Programming Mission](https://forum.duckduckhack.com/t/duckduckhack-programming-mission-overview/53).**

## How to Contribute to the Programming Mission

Before sending a pull request, please read the [**contribution guidelines**](https://forum.duckduckhack.com/t/how-to-contribute-to-the-programming-mission/881).

## Formatting Your Pull Request

When submitting a pull request, the following guidelines help speed up the review process:

### New Instant Answers

1. New IAs should be titled as follows: **`New {IA TOPIC} {IA TYPE}`**. For example, `New NodeJS Spice` or `New Python Cheat Sheet`

2. Paste the relevant [Instant Answer Page URL](https://duck.co/ia/new_ia) in the description field. This will automatically link the PR to the Instant Answer.

### Improvements

1. Fixes should be titled as follows: **`{IA NAME}: Brief explanation`**. For example: `Java resources: Use smaller local image, fallback to API when needed.`

2. Paste the relevant [Instant Answer Page URL](https://duck.co/ia/new_ia) in the description field. This will automatically link the PR to the Instant Answer.

3. Include the issue number in the description (conveniently, this will automatically resolve the issue upon merging). Describe your motivation, thought process, and solution in the description. For example:

	"**Fixes #2038.** The images used by the API are very large and don't change often. I've put a smaller version of each image (and a 2x version for retina screens) in the share directory. The callback will try and load a local image based on the astronauts name and fallback to using the API's image if one does not exist."

> **IMPORTANT:** Don't worry if you get an initial error regarding failing Travis tests. The reason is that your IA page hasn't yet been moved out of the "Planning" status - which only community leaders/staff can do. As long as the ID in your IA page matches the ID in your code, and you've pasted the URL to your IA page, you can ignore this initial error.
