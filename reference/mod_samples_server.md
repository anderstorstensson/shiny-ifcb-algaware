# Sample Exclusion Module Server

Provides a table of loaded samples and lets the user exclude selected
samples from the active dataset. Excluded samples remain available in
memory and can be re-included later.

## Usage

``` r
mod_samples_server(id, rv, config)
```

## Arguments

- id:

  Module namespace ID.

- rv:

  Reactive values for app state.

- config:

  Reactive values with settings.

## Value

NULL (side effects only).
