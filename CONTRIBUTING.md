# How to contribtute

## Code style

We follow the [google style
guide](https://google.github.io/styleguide/Rguide.html).

Differences or highlights:

- `camelCase` for functions and variable names
  - `snake_case` can be used for exported functions as an alias

  ```{r}
  #' Title
  #'
  #' @param x
  #'
  #' @return
  #' @export
  camelCase <- function(x) {
    print(x)
  }

  #' @rdname foo
  #' @export
  snake_case <- camelCase
  ```

- Use `##` for comments
- Non-exported functions should be prefixed with a `.`
- Use sections and headlines to organize the code
- Insert a newline between functions and two lines between sections

It is recommended to use tools like [lintr](https://github.com/jimhester/lintr)
and [styler](https://github.com/r-lib/styler) for automatic syntax checks and
formatting.

### for loops vs apply family

Prefer loops if

- the operations are not independent, e.g. if the next iteration depends on the
  result of the former
- you are doing recursion
- in-place modifications; with `apply` this would require the use of the `<<-`
  operator, which can be a bit tricky due to the way it works (e.g. it will look
  in the parent environments for a variable and assign the value to first one it
  finds).

If you use apply

- consider preferring `vapply()` as it is safer (e.g. no silent failure) vs `sapply()`

Performance considerations regarding loops vs apply are not really existent
anymore. You should favor readability, correctness and robustness.


## Internals

### Cache

- When you want to manipulate the cache `.pkgcache`, use the `assign()` and
  `get()` functions together with the correct environment. There are plenty of
  examples in the code. Otherwise you are in for some pain!
- When you are writing functions for the package, you can use `envir =
  .pkgcache`. For tests, use `envir = openMalariaUtilities:::.pkgcache`


## Commits & Pull requests

Please use the following template when creating commits and PRs.

```
Summarize the change in less than 50 characters

# Headline optional
# Use bullets
Problem:
- Problem, task, reason for commit
- Make a new bullet for each reason
- Each line should be under 72 characters

# Headline optional
# Use bullets or whole sentences to explain what has been done
Solution:
- Solution or list of Changes
Explain exactly what was done in this commit with more depth than the
50 character subject line. Remember to wrap at 72 characters!

Include any additional notes, relevant links, or co-authors.
```
