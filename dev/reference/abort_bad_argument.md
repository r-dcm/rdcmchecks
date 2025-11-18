# Send an error message for an unexpected argument input

Send an error message for an unexpected argument input

## Usage

``` r
abort_bad_argument(
  arg,
  must = NULL,
  not = NULL,
  footer = NULL,
  custom = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- arg:

  The name of the argument.

- must:

  The requirement for input values that is not met.

- not:

  The current state of `argument` that is problematic.

- footer:

  Additional text to add to the error message.

- custom:

  A custom error message to override the default message of `must` +
  `not`.

- call:

  The call stack.

## Value

An error message created by
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).

## Examples

``` r
try(abort_bad_argument(arg = "my_arg", must = "be a character vector"))
#> Error in eval(expr, envir) : `my_arg` must be a character vector
```
