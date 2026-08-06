# Render the dashboard index

Writes `dashboard_rmd/index.html`, a sidebar of every activity in the
manifest beside a viewer pane. Activities without a rendered page are
shown dimmed and are not selectable, so the index is usable long before
every page exists.

## Usage

``` r
rmd_render_index(repo = here("strava_repo"), quiet = FALSE)
```

## Arguments

- repo:

  Path to the Strava repository.

- quiet:

  Suppress progress reporting.

## Value

Path to the written index, invisibly.
