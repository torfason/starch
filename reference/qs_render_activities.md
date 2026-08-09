# Render per-activity pages with Quarto

Renders one HTML page per activity into `dashboard_qs/`, working
backwards from the most recent activity and stopping after `max_files`.

## Usage

``` r
qs_render_activities(
  repo = here("strava_repo"),
  max_files = 10,
  max_points = 600,
  verbose = FALSE,
  quiet = FALSE
)
```

## Arguments

- repo:

  Path to the Strava repository.

- max_files:

  Maximum number of activities to render in one call.

- max_points:

  Number of stream points kept in each page's charts. The streams run to
  tens of thousands of points, which no chart can resolve and which
  dominate the size of the rendered page, so they are thinned evenly on
  the way in. Statistics are computed from the full stream regardless,
  so this affects only chart resolution and file size. Use `0` to keep
  every point, which is worth doing for a single activity examined
  closely.

- verbose:

  Pass the Quarto CLI's own output through, which is verbose but is the
  only way to see why a render failed.

- quiet:

  Suppress starch's own progress reporting. Independent of `verbose`:
  the default reports progress without the CLI's output.

## Value

A tibble of the activities rendered, invisibly.

## Details

An activity is rendered when it has a Parquet stream file and does not
yet have an HTML page. As in the Rmd stack there is no content hashing:
editing the template will not rebuild an existing page. Delete the page
to force one.
