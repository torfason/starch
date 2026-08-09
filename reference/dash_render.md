# Render the activity dashboard

Renders outstanding activity pages, then the overview pages, then the
navigation index so that it links to whatever now exists.

## Usage

``` r
dash_render(
  repo = here("strava_repo"),
  max_files = 10,
  max_points = 600,
  update_heatmap = FALSE,
  verbose = FALSE,
  quiet = FALSE
)
```

## Arguments

- repo:

  Path to the Strava repository.

- max_files:

  Maximum number of activity pages to render in one call.

- max_points:

  Stream points kept in each page's charts.

- update_heatmap:

  Rebuild the heat map even when it already exists.

- verbose:

  Pass the Quarto CLI's own output through.

- quiet:

  Suppress progress reporting.

## Value

Path to the index page, invisibly.

## Details

The heat map is not rebuilt by default. It reads every Parquet file in
the repository and takes far longer than the rest of the dashboard put
together, while changing very little between runs. It is built when
missing, and otherwise only when asked for; see
[`dash_update_heatmap()`](https://torfason.github.io/starch/reference/dash_update_heatmap.md).
