# Sanitize error message for user display

Strips the leading "Error in : " prefix that R prepends to condition
messages so that only the human-readable part is shown in the sidebar.

## Usage

``` r
sanitize_error_msg(msg)
```

## Arguments

- msg:

  Character string (typically `e$message`).

## Value

Simplified character string.
