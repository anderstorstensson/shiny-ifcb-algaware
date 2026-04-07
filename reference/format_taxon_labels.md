# Format taxon labels with italic and sflag for HTML (ggtext) rendering

Builds display labels for a vector of scientific names (name + sflag
combined). Italic names are wrapped in `<i>...</i>`; the sflag suffix is
always plain text.

## Usage

``` r
format_taxon_labels(scientific_names, taxa_lookup, format = c("html", "plain"))
```

## Arguments

- scientific_names:

  Character vector of display names to format.

- taxa_lookup:

  Data frame with columns `name`, `sflag`, and `italic`.

- format:

  `"html"` (default) wraps the name in `<i>...</i>` for ggtext
  rendering; `"plain"` returns the display name unchanged.

## Value

Named character vector of HTML-formatted labels, same length and names
as `scientific_names`.
