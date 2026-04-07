# Add front page mosaic overview page to the report

Add front page mosaic overview page to the report

## Usage

``` r
add_mosaic_overview(
  doc,
  baltic_mosaic,
  westcoast_mosaic,
  cleanup,
  baltic_taxa = NULL,
  westcoast_taxa = NULL,
  taxa_lookup = NULL
)
```

## Arguments

- doc:

  An rdocx object.

- baltic_mosaic:

  Magick image for the Baltic Sea mosaic, or NULL.

- westcoast_mosaic:

  Magick image for the West Coast mosaic, or NULL.

- cleanup:

  Environment with a `files` character vector.

- baltic_taxa:

  Optional character vector of taxa names for Baltic caption.

- westcoast_taxa:

  Optional character vector of taxa names for West Coast.

- taxa_lookup:

  Optional taxa lookup table with `italic` column.

## Value

The modified rdocx object.
