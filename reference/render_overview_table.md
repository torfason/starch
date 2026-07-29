# Render the overview table page

Writes `dashboard/overview_table.html`, a filterable and sortable table
of every activity in the manifest, joined to per-activity statistics
read from the Parquet layer. Each row links out to the activity on
Strava, and, where a detail page exists, back into the dashboard.

## Usage

``` r
render_overview_table(repo = here("strava_repo"), quiet = FALSE)
```

## Arguments

- repo:

  Path to the Strava repository.

- quiet:

  Suppress progress reporting.

## Value

Path to the written page, invisibly.
