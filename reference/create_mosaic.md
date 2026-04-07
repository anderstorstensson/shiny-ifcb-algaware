# Create an adaptive image mosaic for a taxon

Produces a mosaic image where elongated organisms (chains) are shown in
single rows with fewer images, while compact organisms use a tighter
grid.

## Usage

``` r
create_mosaic(
  image_paths,
  n_images = 32L,
  max_width_px = 1800L,
  target_height = 120L,
  max_height_px = 1500L,
  max_cols = NULL,
  labels = NULL,
  allow_taller_rows = FALSE
)
```

## Arguments

- image_paths:

  Character vector of PNG file paths.

- n_images:

  Maximum number of images to include. Default 32.

- max_width_px:

  Maximum mosaic width in pixels. Default 1800 (fits A4 page at 300 dpi
  with margins).

- target_height:

  Base target height in pixels. Default 120.

- max_height_px:

  Maximum mosaic height in pixels. Default 1500 (approximately half an
  A4 page at 300 dpi). Images are dropped to stay within this limit.

- max_cols:

  Maximum images per row, or `NULL` (default) to auto-detect from median
  aspect ratio. Set to a high value (e.g. `Inf`) to pack purely by
  width.

- labels:

  Optional character vector of labels (e.g. sequence numbers) to
  annotate on each image. Must be the same length as `image_paths`.
  Labels are drawn after resizing so font size is consistent.

- allow_taller_rows:

  Logical; if TRUE, rows with spare horizontal space are allowed to grow
  taller (up to limits) to improve readability of larger organisms in
  mixed mosaics. Default FALSE.

## Value

A `magick` image object.
