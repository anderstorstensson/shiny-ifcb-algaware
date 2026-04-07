# Ensure all HAB taxa in text are followed by an asterisk

Post-processes LLM-generated text to add a trailing `*` after any HAB
taxon name that is missing one. This makes HAB marking deterministic
regardless of whether the LLM remembered to include the asterisk.

## Usage

``` r
ensure_hab_asterisks(text, taxa_lookup)
```

## Arguments

- text:

  Character string of plain text.

- taxa_lookup:

  Data frame with columns `name` and `HAB`.

## Value

Character string with asterisks added after every HAB taxon mention.

## Details

Genus names (single word, e.g. *Pseudochattonella*) also match their
*spp.* / *sp.* form so the asterisk lands after the full mention rather
than mid-name. Abbreviated species names (e.g. *D. acuminata*) are
matched alongside the full form.
