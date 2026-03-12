# Check that the missing data indicator is a scalar

The indicator for missing data should be a length 1, non-NULL scalar.

## Usage

``` r
check_missing(x, arg = rlang::caller_arg(x), call = rlang::caller_env())
```

## Arguments

- x:

  The provided missing data indicator to check.

- arg:

  The name of the argument.

- call:

  The call stack.

## Value

The original value (if the checks pass).

## Examples

``` r
check_missing(NA)
#> [1] NA
```
