# Render the Quarto dashboard

Builds `dashboard_qd/` inside the Strava repository: a static index
shell, an overview page carrying a client-side trends chart and activity
table, and a single detail page that renders whichever activity is named
by its `?id=` query parameter.

## Usage

``` r
qd_render_dashboard(
  repo = here("strava_repo"),
  max_files = 10,
  force = FALSE,
  quiet = FALSE
)
```

## Arguments

- repo:

  Path to the Strava repository.

- max_files:

  Maximum number of per-activity data files to write in one call.

- force:

  Re-render the Quarto pages even when they look current.

- quiet:

  Suppress progress reporting.

## Value

Path to the dashboard index, invisibly.

## Details

The pages carry no data of their own. Everything they display is written
to `dashboard_qd/data/` as JavaScript files, so adding activities
rewrites data and does not re-render any HTML. The result opens from
disk without a web server, and needs no network once built.

This runs alongside
[`rmd_render_dashboard()`](https://torfason.github.io/starch/reference/rmd_render_dashboard.md),
which builds the older Rmd dashboard into `dashboard/`. The two share
the manifest reader and the Parquet statistics helper and are otherwise
independent.
