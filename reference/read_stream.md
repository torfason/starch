# Read a Strava export track file into a stream tibble

`read_stream()` reads a single activity track file from a Strava bulk
export and returns a tibble of per-point stream data, dispatching on the
file extension to a format-specific reader. GPX, TCX, and FIT are
supported, including the gzip-compressed (`.gz`) files as shipped in the
export. Fields a given source does not provide are dropped rather than
filled with `NA`.

## Usage

``` r
read_stream(path)

read_gpx_stream(path)

read_tcx_stream(path)

read_fit_stream(path)
```

## Arguments

- path:

  Path to a track file. The extension (after stripping any `.gz`) must
  be one of `gpx`, `tcx`, or `fit`.

## Value

A tibble with one row per track point, drawn from `timestamp`, `lat`,
`lng`, `altitude`, `heartrate`, `cadence`, `temp`, `dev_dist`,
`velocity_smooth`, `watts`, and `grade_smooth`, plus a
`"activity_metadata"` attribute (see Details above).

## Details

Each returned tibble also carries per-activity metadata as
`attr(x, "activity_metadata")`: a flat named list with elements
`format`, `source`, `sport` (all sports found, slash-joined),
`sub_sport`, `title`, `start_time`, `n_sessions`, `total_distance`,
`total_timer_time`, and `total_calories`, with `NA` for anything the
source does not provide. It is intentionally flat (no nesting) so it
converts to a one-row table via
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).
Subsetting and many dplyr verbs drop attributes, so persist it
deliberately.

## Functions

- `read_gpx_stream()`: Read a GPX file. Garmin TrackPointExtension
  fields (`hr`, `cad`, `atemp`) are read via namespace-stripped XPath,
  so no GDAL extension flag is required.

- `read_tcx_stream()`: Read a TCX file. Strava's TCX exports carry
  leading whitespace before the `<?xml>` declaration, so the file is
  read as text and trimmed before parsing.

- `read_fit_stream()`: Read a FIT file (requires the FITfileR package).
  `records()` may return several tibbles when a file has multiple record
  definitions; they are bound. Record fields vary between files, so a
  safe getter returns `NA` for absent fields.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- read_stream("activities/9973795459.gpx.gz")
attr(d, "activity_metadata")
} # }
```
