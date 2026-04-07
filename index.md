# AlgAware-IFCB

[![R-CMD-check](https://github.com/nodc-sweden/ifcb-algaware/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nodc-sweden/ifcb-algaware/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-brightgreen.svg)](https://nodc-sweden.github.io/ifcb-algaware/)

An interactive R/Shiny application for processing, validating, and
reporting phytoplankton data from Imaging FlowCytobot (IFCB)
instruments. Developed for Swedish marine monitoring at SMHI.

AlgAware-IFCB integrates with the IFCB Dashboard for automated data
retrieval, provides an interactive image gallery for reviewing and
correcting AI classifier predictions, and generates Word reports with
biomass maps, chlorophyll maps, heatmaps, image mosaics, and CTD
fluorescence profiles.

> **Intended use:** This application is developed for internal use at
> SMHI as part of the Swedish national marine monitoring programme. It
> may be adapted for use at other institutes operating IFCB instruments,
> though configuration and data infrastructure will need to be adjusted
> accordingly.

## Installation

Install the latest release from GitHub including all optional
dependencies:

``` r
# install.packages("remotes")
remotes::install_github("nodc-sweden/ifcb-algaware",
                        dependencies = TRUE,
                        ref = remotes::github_release())
```

The `dependencies = TRUE` flag installs both required and suggested
packages. Suggested packages enable optional features:

| Package     | Feature                                         |
|-------------|-------------------------------------------------|
| `oce`       | Parsing SeaBird CTD `.cnv` files                |
| `patchwork` | Multi-panel CTD regional figures in the report  |
| `yaml`      | Reading standard monitoring station definitions |

Install suggested packages individually if needed:

``` r
install.packages(c("oce", "patchwork", "yaml"))
```

## Usage

``` r
library(algaware)
launch_app()
```

The application opens in your default browser.

## Features

### Data Loading

- Fetch metadata from any IFCB Dashboard instance
- Filter by cruise number or date range
- Automatic spatial matching of IFCB bins to monitoring stations in the
  Baltic Area
- Download raw data, features, and HDF5 classification files
- FerryBox chlorophyll integration (optional)

### Image Gallery and Validation

- Browse classified images by taxon and region (Baltic Sea / West Coast)
- Searchable class dropdown for quick navigation across 100+ classes
- Visual tags for HAB species and non-biological classes
- Select individual images or entire pages
- Relabel selected images or entire classes
- Invalidate misclassified taxa
- Store manual annotations to SQLite (compatible with ClassiPyR)
- Measurement tool for on-screen size estimation

### CTD & Chlorophyll Data

- Load SeaBird CTD `.cnv` files from a cruise folder
- Load LIMS discrete chlorophyll bottle data
- Per-station fluorescence profiles (0–50 m, deduplicated casts)
- Chlorophyll time series with smooth spline historical statistics
  (1991–2020 climatology)
- Regional multi-panel figures (one row per station, profile + time
  series) included in the report

### Visualizations

- **Maps**: Biomass, image count, and chlorophyll (CTD fluorescence +
  LIMS bottle) distribution across stations
- **Heatmaps**: Biovolume by taxon and station visit
- **Stacked bar charts**: Relative biovolume composition (top 15 taxa)
- **Summary table**: Interactive, sortable station-level data

### Report Generation

- Automated Word document (`.docx`) with all plots and station sections
- AI-generated summaries and station descriptions via OpenAI or Google
  Gemini
- Front-page mosaic designer with interactive taxon and image selection
- Image mosaics for top taxa per region (adaptive layout for chains
  vs. compact organisms)
- CTD regional figures with fluorescence profiles and Chl-a time series
- HAB species annotations throughout
- Classifier model attribution
- Corrections log export (CSV)

## Configuration

Settings are persisted in `~/.config/R/algaware/settings.json` and can
be edited through the in-app Settings panel:

| Setting                | Description                                            | Default                                       |
|------------------------|--------------------------------------------------------|-----------------------------------------------|
| Dashboard URL          | IFCB Dashboard base URL                                | –                                             |
| Dashboard Dataset      | Dataset name                                           | –                                             |
| Classification Path    | Directory with HDF5 classifier output                  | –                                             |
| FerryBox Data Path     | Directory with FerryBox CSV files                      | –                                             |
| Local Storage Path     | Where downloaded data is cached                        | `./algaware_data`                             |
| Database Folder        | Directory for annotations.sqlite                       | –                                             |
| Non-biological Classes | Comma-separated list of classes excluded from analysis | `detritus,Air_bubbles,Beads,Debris,mix,mixed` |
| Pixels per Micron      | IFCB camera calibration factor                         | `2.77`                                        |

Extra monitoring stations can be added from the SHARK station register
through the Settings panel.

### LLM Configuration

Set one of the following environment variables to enable AI-generated
report text (summaries and station descriptions):

| Variable         | Provider                                  |
|------------------|-------------------------------------------|
| `OPENAI_API_KEY` | OpenAI (default: gpt-4.1)                 |
| `GEMINI_API_KEY` | Google Gemini (default: gemini-2.5-flash) |

Override the model with `OPENAI_MODEL` or `GEMINI_MODEL`. When both keys
are set, OpenAI is used by default.

## Bundled Data

| File                                                  | Description                                                           |
|-------------------------------------------------------|-----------------------------------------------------------------------|
| `inst/extdata/taxa_lookup.csv`                        | Phytoplankton taxa with WoRMS AphiaID references and HAB status flags |
| `inst/extdata/standard_stations.yaml`                 | Standard monitoring stations with regional assignments                |
| `inst/extdata/station_mapper.txt`                     | Synonym mapper for raw station names to canonical names               |
| `inst/extdata/annual_1991-2020_statistics_chl20m.txt` | Historical 0–20 m Chl-a monthly statistics (1991–2020)                |
| `inst/extdata/report_writing_guide.md`                | LLM system prompt and style guide for report text generation          |
| `inst/stations/algaware_stations.tsv`                 | 12 AlgAware monitoring stations (6 Baltic Sea, 6 West Coast)          |
| `inst/templates/report_template.docx`                 | Word document template for generated reports                          |

## Project Structure

    algaware/
    ├── R/
    │   ├── run_app.R              # launch_app() entry point
    │   ├── mod_settings.R         # Settings module
    │   ├── mod_data_loader.R      # Data loading pipeline
    │   ├── mod_gallery.R          # Image gallery browser
    │   ├── mod_validation.R       # Annotation and relabeling
    │   ├── mod_frontpage.R        # Front-page mosaic designer
    │   ├── mod_ctd.R              # CTD & chlorophyll data module
    │   ├── mod_samples.R          # Sample selection module
    │   ├── mod_report.R           # Report generation module
    │   ├── ctd.R                  # CTD/LIMS parsing and Chl-a averaging
    │   ├── ctd_plots.R            # CTD fluorescence profiles and time series
    │   ├── data_download.R        # Dashboard API integration
    │   ├── data_processing.R      # Biovolume and aggregation
    │   ├── database.R             # SQLite operations
    │   ├── plots.R                # Maps, heatmaps, bar charts
    │   ├── mosaics.R              # Adaptive image mosaics
    │   ├── taxa.R                 # Taxon label formatting and class resolution
    │   ├── stations.R             # Station matching
    │   ├── report.R               # Word document builder
    │   ├── llm.R                  # LLM integration (OpenAI/Gemini)
    │   └── utils.R                # Settings and utilities
    ├── inst/
    │   ├── app/                   # Shiny app (ui.R, server.R)
    │   ├── extdata/               # Bundled data files
    │   ├── stations/              # Station definitions
    │   └── templates/             # Report template
    └── tests/testthat/            # Test suite

## Testing

``` r
devtools::test()
```

## License

MIT
