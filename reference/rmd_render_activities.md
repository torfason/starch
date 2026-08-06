# Render per-activity dashboard pages

Renders one HTML page per activity into `dashboard_rmd/activities/`,
working backwards from the most recent activity and stopping after
`max_files`.

## Usage

``` r
rmd_render_activities(
  repo = here("strava_repo"),
  max_files = 10,
  quiet = FALSE
)
```

## Arguments

- repo:

  Path to the Strava repository.

- max_files:

  Maximum number of activities to render in one call.

- quiet:

  Suppress progress reporting.

## Value

A tibble of the activities rendered, invisibly.

## Details

An activity is rendered when it has a Parquet stream file and does not
yet have an HTML page. There is no content hashing at this stage:
editing the template or changing an activity's manifest row will not
cause an existing page to be rebuilt. Delete the page to force one.
