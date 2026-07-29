# Read the activity manifest from a Strava export

Reads `activities.csv` from the repository root and returns a tidy
manifest, newest first. The `stem` column is the activity's stream file
name with all extensions stripped, which is the key linking a manifest
row to its Parquet and HTML files. It is `NA` for manual activities that
have no stream file.

## Usage

``` r
load_activities_csv(repo = here("strava_repo"))
```

## Arguments

- repo:

  Path to the Strava repository.

## Value

A tibble with one row per activity, sorted newest first.
