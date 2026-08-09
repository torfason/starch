# Render the static dashboard index

Writes `dashboard_qs/index.html`, a table of every activity in the
manifest, linking to those that already have a page. The manifest is
written to the staged project as `manifest.csv` and read back by
`index.qmd`, so the template holds no knowledge of the repository
layout.

## Usage

``` r
qs_render_index(repo = here("strava_repo"), verbose = FALSE, quiet = FALSE)
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
