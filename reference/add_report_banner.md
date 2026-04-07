# Add a banner with SMHI and AlgAware logos

Reads SMHI and ALGAWARE logo files from `inst/templates/logos/`, trims
whitespace, optionally adds baseline-alignment padding, and inserts them
side-by-side as inline images in a centred paragraph. Falls back to the
legacy `ALGAWARE_title.PNG` banner if the individual logos are
unavailable.

## Usage

``` r
add_report_banner(doc, center_pp, cleanup = NULL, logo_scale = 1)
```

## Arguments

- doc:

  An rdocx object.

- center_pp:

  A centred `fp_par` paragraph property object.

- cleanup:

  Environment with a `files` character vector for tracking temp files
  (may be NULL).

- logo_scale:

  Numeric scaling factor applied to the base logo height. Default 1.

## Value

The modified rdocx object.
