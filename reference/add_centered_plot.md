# Save a ggplot to a temp file and add centered to document

Save a ggplot to a temp file and add centered to document

## Usage

``` r
add_centered_plot(
  doc,
  plot,
  cleanup,
  width,
  height,
  display_width,
  display_height
)
```

## Arguments

- doc:

  An rdocx object.

- plot:

  A ggplot object.

- cleanup:

  Environment with a `files` character vector.

- width, height:

  Plot dimensions in inches for ggsave.

- display_width, display_height:

  Display dimensions in the document.

## Value

The modified rdocx object.
