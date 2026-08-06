# Render the activity dashboard

Renders outstanding activity pages, then rebuilds the index so that it
reflects them. The index is always rewritten; it is a single cheap file.

## Usage

``` r
rmd_render_dashboard(repo = here("strava_repo"), max_files = 10, quiet = FALSE)
```

## Arguments

- repo:

  Path to the Strava repository.

- max_files:

  Maximum number of activities to render in one call.

- quiet:

  Suppress progress reporting.

## Value

Path to the index, invisibly.
