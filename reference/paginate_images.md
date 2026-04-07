# Paginate a data frame of images

Paginate a data frame of images

## Usage

``` r
paginate_images(imgs, page, page_size)
```

## Arguments

- imgs:

  Data frame of images.

- page:

  Integer page number (1-based).

- page_size:

  Number of images per page.

## Value

Row-subset of `imgs` for the requested page.
