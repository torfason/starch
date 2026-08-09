# Render the filterable activity table

Writes `dashboard_qs/overview_table.html`, a sortable and filterable
table of every activity in the manifest joined to per-activity
statistics read from the Parquet layer. Each row links out to the
activity on Strava and, where a page exists, back into the dashboard.

## Usage

``` r
qs_render_table(repo = here("strava_repo"), verbose = FALSE, quiet = FALSE)
```

## Arguments

- repo:

  Path to the Strava repository.

- verbose:

  Pass the Quarto CLI's own output through, which is verbose but is the
  only way to see why a render failed.

- quiet:

  Suppress starch's own progress reporting. Independent of `verbose`:
  the default reports progress without the CLI's output.

## Value

Path to the written page, invisibly.

## Details

Unlike the detail pages this reads every Parquet footer, so it is the
slow part of a full build, and the table's data is embedded in the page
rather than shared.
