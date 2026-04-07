# Resolve the class list from database or auto-generate

Tries to load the global class list from the configured SQLite database
(shared with ClassiPyR). If no database is configured or it has no class
list, auto-generates one from the union of all taxa lookup names and
observed classification class names. The caller receives a flag
indicating which path was taken so it can show a notification.

## Usage

``` r
resolve_classes(config, taxa_lookup, classifications)
```

## Arguments

- config:

  Reactive values with settings (`db_folder`).

- taxa_lookup:

  Data frame with `clean_names` column.

- classifications:

  Data frame with `class_name` column.

## Value

A list with `class_list` (character vector) and `auto_generated`
(logical).
