# Build Grouped Relabel Choices

Merges the database class list, taxa lookup, and custom classes into a
grouped list suitable for `selectizeInput` with optgroups. Database
classes appear first, then taxa lookup classes not already in the
database, then custom classes.

## Usage

``` r
build_relabel_choices(
  db_class_list = character(0),
  taxa_lookup = NULL,
  custom_classes = NULL
)
```

## Arguments

- db_class_list:

  Character vector of class names from the global class list (database).

- taxa_lookup:

  Data frame with at least a `clean_names` column.

- custom_classes:

  Data frame with at least a `clean_names` column.

## Value

A named list with two elements: `grouped` (a named list of character
vectors for selectize optgroups) and `all` (a flat character vector of
all unique class names).
