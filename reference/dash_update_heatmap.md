# Rebuild the route heat map

Rebuilds `overview_heatmap.html` and then the index, so that a heat map
built for the first time is linked from the sidebar. Separate from
[`dash_render()`](https://torfason.github.io/starch/reference/dash_render.md)
because it reads every Parquet file in the repository.

## Usage

``` r
dash_update_heatmap(
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
  cap.

- verbose:

  Pass the Quarto CLI's own output through.

- quiet:

  Suppress progress reporting.

## Value

Path to the heat map page, invisibly.
