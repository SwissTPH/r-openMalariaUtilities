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

- Use `##` for comments and `###` for headlines
- Non-exported functions should be prefixed with a `.`
- Use sections and headlines to organize the code
- Insert a newline between functions and two lines between sections

It is recommended to use tools like [lintr](https://github.com/jimhester/lintr)
and [styler](https://github.com/r-lib/styler) for automatic syntax checks and
formatting.


### Use file headers

All files in the `/R` directory should have a header with a summary of the
functionality provided by the file.


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

### Load order matters

Due to the way the cache and the debug messages work in this package, the load
order of the R files matter. Thus, you need to make sure that the `include`
statements are present at the beginning of the file.

The `pkg_setup.R` file should be used when in doubt in order to make sure the
basic setup is loaded first.

```{r}
##' @include pkg_setup.R
NULL
```

You can require multiple files if necessary and skip `pkg_setup.R` if it is
already included by other files.

```{r}
##' @include cache.R printing.R
NULL
```


### Cache

- When you want to manipulate the cache `.pkgcache`, use the `putCache()` and
  `getCache()` functions. They will use the correct environment and make the
  usage of the cache much easier. There are plenty of examples in the code.
  Otherwise you are in for some pain!
- Consider using `syncCache()` or similar for within functions which manipulate
  the cache. This way, you can make sure that the updated cache is synchronized.


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

The summary line should have a prefix like:

- fix: fixes a bug or unexpected behavior (e.g. `fix: Function does not crash anymore`)
- tweak: improves, refactors or changes something (e.g. `tweak: Function is now 5% faster`)
- doc: add or update documentation (e.g. `doc: Updated vigentte XY`)
- feat: add a new feature (e.g. `feat: Add function XY`)

## Versioning & Releases

We follow the `YY.MM.minorversion` (e.g. `22.08.1`) schema for version names. 

Minor, non-breaking changes should only cause an increase of the minor version,
e.g. `.1 -> .2`. 

Big changes, which might be breaking, should lead to an updated `YY.MM`.

Consequentially, each bump in a version should lead to a tagged release.
Ideally, the version bump is the last commit before your do this.

**Ignoring any of the above guidelines can lead to the rejection of your contribution!**
