# abort_bad_argument() is informative.

    Code
      abort_bad_argument("size", "be an integer", not = "character")
    Condition
      Error:
      ! `size` must be an integer; not character
    Code
      abort_bad_argument("size", must = "be an integer", not = "character", footer = c(
        i = "please"))
    Condition
      Error:
      ! `size` must be an integer; not character
      i please
    Code
      abort_bad_argument("size", must = "be an integer", not = "character", footer = "required",
        custom = "A new error")
    Condition
      Error:
      ! A new error

# abort_bad_argument() can add inline markup to footer

    Code
      a_local_variable <- "local var"
      footer <- cli::format_message(c(x = "Look at {.val {a_local_variable}}"))
      abort_bad_argument("size", "be an integer", footer = footer)
    Condition
      Error:
      ! `size` must be an integer
      x Look at "local var"

