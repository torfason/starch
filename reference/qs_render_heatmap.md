# Render the route heat map

Writes `dashboard_qs/heatmap.html`, every GPS point from every activity
as a single leaflet heat layer.

## Usage

``` r
qs_render_heatmap(
  repo = here("strava_repo"),
  types = NULL,
  grid_digits = 5,
  max_points = NULL,
  verbose = FALSE,
  quiet = FALSE
)
```

## Arguments

- repo:

  Path to the Strava repository.

- types:

  Activity types to include, or `NULL` for all.

- grid_digits:

  Decimal places to round coordinates to before counting. `NULL` keeps
  every point at full precision and does not aggregate.

- max_points:

  Cap on the number of grid cells written to the page, or `NULL` for no
  cap. A blunt instrument for trading detail against file size: cells
  are dropped evenly across the aggregated table, which is not spatially
  even, so prefer a coarser `grid_digits` where that will do.

- verbose:

  Pass the Quarto CLI's own output through, which is verbose but is the
  only way to see why a render failed.

- quiet:

  Suppress starch's own progress reporting. Independent of `verbose`:
  the default reports progress without the CLI's output.

## Value

Path to the written page, invisibly.

## Details

The points are rounded to a grid and counted, so repeated routes
contribute weight rather than overplotting, and the page carries one row
per cell instead of one per fix. At the default five decimals the grid
is about a metre, which is finer than the fixes themselves, so the
reduction comes only from genuine repetition: expect the page to run to
several megabytes.
