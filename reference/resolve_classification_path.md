# Resolve classification source path across OS conventions

Converts backslashes to forward slashes and, on non-Windows systems,
attempts to map Windows drive-letter paths (e.g. `Z:/...`) to
`/mnt/z/...` when that mount exists.

## Usage

``` r
resolve_classification_path(path)
```

## Arguments

- path:

  Character path as configured by the user.

## Value

A normalized path candidate.
