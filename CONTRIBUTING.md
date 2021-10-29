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
