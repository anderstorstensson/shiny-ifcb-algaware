# Fix page numbering to start at 1 after the front page

Post-processes the .docx file XML to add `pgNumType` with `start="1"` to
the second section (first content section).

## Usage

``` r
fix_page_numbering(docx_path)
```

## Arguments

- docx_path:

  Path to the .docx file.
