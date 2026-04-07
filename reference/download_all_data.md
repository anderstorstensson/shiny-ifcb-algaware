# Download raw data, features, and classification files

Downloads .roi/.adc/.hdr raw files and feature CSVs from the IFCB
Dashboard, then copies AI classification H5 files from the configured
source path. All three destinations are subdirectories of `storage`:
`raw/`, `features/`, and `classified/`.

## Usage

``` r
download_all_data(config, sample_ids, storage)
```

## Arguments

- config:

  Reactive values with settings (`dashboard_url`, `dashboard_dataset`,
  `classification_path`).

- sample_ids:

  Character vector of sample PIDs to retrieve.

- storage:

  Local base directory for downloaded files.

## Value

A list with `raw_dir`, `feat_dir`, `class_dir` paths.
