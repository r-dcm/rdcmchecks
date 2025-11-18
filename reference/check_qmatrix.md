# Check that a Q-matrix follows the expected structure

The Q-matrix should be 1 row per item and 1 column per attribute, with
an optional additional column to store item identifiers. Each value of
the Q-matrix should be a 0 or 1 to indicate measurement of the attribute
by the given item. `clean_qmatrix()` calls `check_qmatrix()` to verify
the expected structure, and then performs additional data manipulation
to provide standard conventions. See details for additional information.

## Usage

``` r
check_qmatrix(
  x,
  identifier = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

clean_qmatrix(
  x,
  identifier = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)
```

## Arguments

- x:

  The provided Q-matrix to check.

- identifier:

  The provided item identifier, as a character string. If no item
  identifier is present, the value should be `NULL` (the default).

- arg:

  The name of the argument.

- call:

  The call stack.

## Value

`check_qmatrix` returns the original Q-matrix (if the checks pass) as a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with one row per item.

`clean_qmatrix` returns a list with four elements:

- `clean_qmatrix`: The cleaned Q-matrix

- `attribute_names`: The real names of the attributes

- `item_identifier`: The real name of the item identifier

- `item_names`: The real names of the items

## Details

In many instances, it's important to have standard conventions for a
Q-matrix so that we know what to expect (e.g., item identifiers,
attribute names). `clean_qmatrix()` provides this standardization. For
the cleaned Q-matrix, item identifiers and item names are removed.
Additionally, all attributes are renamed `att1`, `att2`, `att3`, etc.
Finally, all columns are coerced to integer values.

To ensure downstream functions are able to identify the original
(pre-cleaned) values, `clean_qmatrix()` returns a list that includes the
cleaned Q-matrix, as well as metadata that includes look-ups from the
original to cleaned values.

## Examples

``` r
example_qmatrix <- tibble::tibble(item = paste0("item_", 1:5),
                                  att_1 = c(0, 0, 1, 1, 1),
                                  att_2 = c(1, 1, 1, 0, 0))
check_qmatrix(example_qmatrix, identifier = "item")
#> # A tibble: 5 × 3
#>   item   att_1 att_2
#>   <chr>  <int> <int>
#> 1 item_1     0     1
#> 2 item_2     0     1
#> 3 item_3     1     1
#> 4 item_4     1     0
#> 5 item_5     1     0
example_qmatrix <- tibble::tibble(item = paste0("item_", 1:5),
                                  att_1 = c(0, 0, 1, 1, 1),
                                  att_2 = c(1, 1, 1, 0, 0))
clean_qmatrix(example_qmatrix, identifier = "item")
#> $clean_qmatrix
#> # A tibble: 5 × 2
#>    att1  att2
#>   <int> <int>
#> 1     0     1
#> 2     0     1
#> 3     1     1
#> 4     1     0
#> 5     1     0
#> 
#> $attribute_names
#>  att_1  att_2 
#> "att1" "att2" 
#> 
#> $item_identifier
#> [1] "item"
#> 
#> $item_names
#> item_1 item_2 item_3 item_4 item_5 
#>      1      2      3      4      5 
#> 
```
