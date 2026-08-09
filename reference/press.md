# Run the maintenance sequence

Imports the latest Strava export, converts new activities to Parquet,
renders the static dashboard, and offers to open it. Each step reports
what it is about to do and asks before doing it, so the whole run can be
stepped through, skipped past, or abandoned.

## Usage

``` r
press(
  repo = here("strava_repo"),
  zip = latest_strava_zip(),
  max_parquet = 50,
  max_pages = 10,
  max_points = 600,
  confirm = TRUE,
  view = TRUE,
  quiet = FALSE
)
```

## Arguments

- repo:

  Path to the Strava repository.

- zip:

  Path to the export archive. Defaults to the most recent one, and the
  import step is skipped if none is found.

- max_parquet:

  Default number of activities to convert to Parquet.

- max_pages:

  Default number of dashboard pages to render.

- max_points:

  Stream points kept per page, passed to the renderer.

- confirm:

  Ask before each step. `FALSE` runs the whole sequence on the defaults
  without prompting.

- view:

  Offer to open the dashboard at the end.

- quiet:

  Suppress the underlying functions' progress reporting. The prompts and
  step summaries are not affected.

## Value

Path to the repository, invisibly, or `NULL` if aborted.
