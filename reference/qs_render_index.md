# Render the dashboard navigation index

Writes `dashboard_qs/index.html`, a sidebar of every activity in the
manifest over an iframe that shows the selected page. Overview pages are
listed above the activities, and only those that exist are offered.

## Usage

``` r
qs_render_index(repo = here("strava_repo"), quiet = FALSE)
```

## Arguments

- repo:

  Path to the Strava repository.

- quiet:

  Suppress progress reporting.

## Value

Path to the written page, invisibly.

## Details

The shell is assembled by R rather than rendered by Quarto, so adding an
activity rebuilds one small file instead of re-rendering every page,
which is what a Quarto-native sidebar would require.
