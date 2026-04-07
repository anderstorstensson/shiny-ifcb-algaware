# Normalize a file path setting

Converts backslashes to forward slashes so that Windows paths pasted
from File Explorer work correctly regardless of platform.

## Usage

``` r
normalize_settings_path(p)
```

## Arguments

- p:

  Character path.

## Value

Normalized path with forward slashes, or `p` unchanged if empty.
