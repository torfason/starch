# Render the static Quarto dashboard

Renders outstanding activity pages, then rebuilds the index so that it
links to whatever now exists.

## Usage

``` r
qs_render_dashboard(
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

Path to the index page, invisibly.
