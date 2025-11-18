# Check that data follows the expected structure

The data should be 1 row per respondent and 1 column per item, with an
optional additional column to store respondent identifiers. Each value
of the data should be a 0 or 1 to indicate the response to the item by
the given respondent. `clean_data()` calls `check_data()` to verify the
expected structure, and then performs additional data manipulation to
provide standard conventions. See details for additional information.

## Usage

``` r
check_data(
  x,
  identifier = NULL,
  missing = NA,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

clean_data(
  x,
  identifier = NULL,
  missing = NA,
  cleaned_qmatrix,
  arg_qmatrix = rlang::caller_arg(cleaned_qmatrix),
  valid_names = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)
```

## Arguments

- x:

  The provided data to check.

- identifier:

  The provided respondent identifier, as a character string. If no
  respondent identifier is present, the value should be `NULL` (the
  default).

- missing:

  A expression specifying how missing data in `x` is coded (e.g., `NA`,
  `"."`, `-99`). The default is `NA`.

- arg:

  The name of the argument.

- call:

  The call stack.

- cleaned_qmatrix:

  A cleaned Q-matrix, from
  [`clean_qmatrix()`](https://rdcmchecks.r-dcm.org/reference/check_qmatrix.md).

- arg_qmatrix:

  A character string with the name of the argument used to provide the
  Q-matrix.

- valid_names:

  An optional named vector of items (e.g., from a previous call to
  `clean_data()`). Used when checking new data objects for consistency
  with items used to estimate a model.

## Value

`check_data` returns the original data (if the checks pass) as a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html),
with missing data (i.e., `missing`) replaced with `NA`.

`clean_data` returns a list with five elements:

- `clean_data`: The cleaned data

- `item_identifier`: The real name of the item identifier

- `item_names`: The real names of the items

- `respondent_identifier`: The real name of the respondent identifier

- `respondent_names`: The real names of the respondents

## Details

In many instances, it's important to have standard conventions for a
data object so that we know what to expect (e.g., respondent and item
identifiers, data types). `clean_data()` provides this standardization.
Cleaned data is returned in long format, with one row per response.
Respondent and item columns are encoded as factors, and responses are
coerced to integer values.

To ensure downstream functions are able to identify the original
(pre-cleaned) values, `clean_data()` returns a list that includes the
cleaned data, as well as metadata that includes look-ups from the
original to cleaned values.

## Examples

``` r
example_data <- tibble::tibble(person = 1:10,
                               item1 = sample(0:1, 10, replace = TRUE),
                               item2 = sample(0:1, 10, replace = TRUE),
                               item3 = sample(0:1, 10, replace = TRUE))
check_data(example_data, identifier = "person")
#> # A tibble: 10 × 4
#>    person item1 item2 item3
#>     <int> <int> <int> <int>
#>  1      1     0     0     0
#>  2      2     1     1     0
#>  3      3     0     0     1
#>  4      4     0     0     0
#>  5      5     0     1     1
#>  6      6     1     1     1
#>  7      7     1     0     0
#>  8      8     0     0     0
#>  9      9     0     1     1
#> 10     10     0     0     1
example_qmatrix <- tibble::tibble(item = paste0("item", 1:3),
                                  att_1 = c(0, 0, 1),
                                  att_2 = c(1, 1, 1))

example_data <- tibble::tibble(person = 1:10,
                               item1 = sample(0:1, 10, replace = TRUE),
                               item2 = sample(0:1, 10, replace = TRUE),
                               item3 = sample(0:1, 10, replace = TRUE))

qmatrix <- clean_qmatrix(example_qmatrix, identifier = "item")
clean_data(example_data, identifier = "person",
           cleaned_qmatrix = qmatrix)
#> $clean_data
#> # A tibble: 30 × 3
#>    resp_id item_id score
#>    <fct>   <fct>   <int>
#>  1 1       item1       1
#>  2 1       item2       1
#>  3 1       item3       0
#>  4 2       item1       0
#>  5 2       item2       0
#>  6 2       item3       0
#>  7 3       item1       0
#>  8 3       item2       1
#>  9 3       item3       1
#> 10 4       item1       1
#> # ℹ 20 more rows
#> 
#> $item_identifier
#> [1] "item"
#> 
#> $item_names
#> item1 item2 item3 
#>     1     2     3 
#> 
#> $respondent_identifier
#> [1] "person"
#> 
#> $respondent_names
#>  1  2  3  4  5  6  7  8  9 10 
#>  1  2  3  4  5  6  7  8  9 10 
#> 
```
