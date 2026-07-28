# AGENTS.md

Guidance for AI agents (and humans) working in this repository. Keep it
current when structure or conventions change.

## Project

`starch` is an R package for reading Strava bulk-export track files
(GPX, TCX, FIT, including the gzip-compressed `.gz` files as shipped)
into tidy, analysis-ready stream tibbles, and deriving per-point columns
(elapsed time, cumulative distance, speed).

Longer term it will grow a file → Parquet → reports pipeline.

- Repo: `torfason/starch` on GitHub; default branch `main`.
- Docs site: pkgdown at `torfason.github.io/starch/`.
- License: MIT
- CI: R-CMD-check (single-platform quickstart workflow).

## Setup and common commands

Run from the package root in R:

``` r

devtools::load_all()     # load package for interactive dev
devtools::document()     # regenerate NAMESPACE and man/*.Rd after roxygen changes
devtools::test()         # run the test suite (fast; skips the thorough loop)
devtools::check()        # full R CMD check
test_thorough()          # run tests including the slow exhaustive fixture loop
```

`test_thorough()` is a helper (see `tests/testthat/helper-fixtures.R`)
that sets `STARCH_TEST_THOROUGH=true` for one `devtools::test()` run.

## Architecture

Source lives in `R/`:

- `read_streams.R` – the reader family. `read_stream(path)` dispatches
  on the extension (after stripping `.gz`) to
  [`read_gpx_stream()`](https://torfason.github.io/starch/reference/read_stream.md),
  [`read_tcx_stream()`](https://torfason.github.io/starch/reference/read_stream.md),
  or
  [`read_fit_stream()`](https://torfason.github.io/starch/reference/read_stream.md).
  Each reads the file **once** and returns a stream tibble (one row per
  track point) with columns drawn from `timestamp`, `lat`, `lng`,
  `altitude`, `heartrate`, `cadence`, `temp`, `dev_dist`,
  `velocity_smooth`, `watts`, `grade_smooth`. `drop_empty_cols()`
  removes any column that is entirely `NA`.
- `derive_columns.R` – the `addcols_*` transforms:
  [`addcols_time()`](https://torfason.github.io/starch/reference/derive_columns.md),
  [`addcols_distance()`](https://torfason.github.io/starch/reference/derive_columns.md)
  (adds `distance`, plus `dist_diff` when device distance `dev_dist` is
  present),
  [`addcols_speed()`](https://torfason.github.io/starch/reference/derive_columns.md)
  (smoothed),
  [`addcols_speed_naive()`](https://torfason.github.io/starch/reference/derive_columns.md).
  Pure tibble → tibble; documented as one family via `@describeIn`.

### Per-activity metadata

Each reader also extracts activity-level (not per-point) metadata in the
same pass and attaches it as a single flat attribute,
`attr(d, "activity_metadata")`, built by the internal
`new_stream_meta()`. It is a flat named list with a fixed field set:
`format`, `source`, `sport`, `sub_sport`, `title`, `start_time`,
`n_sessions`, `total_distance`, `total_timer_time`, `total_calories`.
Design rules:

- Flat only – no nested values – so it converts to a one-row table via
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).
- When a source offers several values for one field (e.g. a multisport
  FIT file), they are collapsed into one slash-joined string
  (`"swimming/cycling"`).
- Format inconsistencies are **not** normalized (TCX `Biking` vs FIT
  `cycling`, case, etc.); extract as-is, resolve downstream if ever
  needed.
- A recordless FIT file (a workout definition, not an activity) reads as
  a genuine empty `0 x 0` stream, still carrying the metadata attribute.

### Fixtures

`inst/extdata/` holds gzipped test files, split into `activities/` and
`workouts/`. They are vendored (MIT) from
`msimms/TestFilesForFitnessApps`; see `inst/extdata/SOURCES.md` and
`LICENSE-testfiles`. Regenerate with `data-raw/testfiles.R`. Files are
gzipped with `compression = 9`; R’s
[`gzfile()`](https://rdrr.io/r/base/connections.html) already omits the
mtime and original filename, so re-gzipping is byte-stable (equivalent
to `gzip -n`) and won’t produce spurious git diffs.

## Conventions

- **Tidyverse with native pipe.** Prefer dplyr/tidyr/purrr idioms and
  the native `|>` pipe (not magrittr `%>%`). Before writing a
  from-scratch implementation, look for an existing function in a
  well-known package.
- **Attributes attach last.** `[` and dplyr verbs drop custom
  attributes, so `activity_metadata` must be attached after
  `drop_empty_cols()` and any column reordering. If a step needs the
  attribute preserved, set it afterward.
- **One reader per format, read the file once.** Do not add separate
  metadata readers; points and metadata come from the same parse (memory
  matters – large exports, and a suspected xml2/FIT leak in long runs).
- **`.data` pronoun in package code.** Inside `mutate()` etc., reference
  columns as `.data$col` (imported via `@importFrom dplyr .data`) and
  pass tidyselect args like `.after` as strings, to avoid the “no
  visible binding” NOTE.
- **FIT is optional.** `FITfileR` is a `Suggests` (installed from
  r-universe / GitHub, not CRAN). Guard its use with
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) in code
  and `skip_if_not_installed("FITfileR")` in tests.
- **Prose style.** Use en dashes (–), not em dashes (—), in comments and
  docs.

### Canonical column order

Stream tibbles should present columns in this order (a column not listed
is kept after the listed ones):

    timestamp, time, distance          # axes
    lat, lng, altitude                 # position
    speed, speed_ms, speed_kmh, pace   # movement (robust; front-of-house)
    heartrate, cadence, watts, temp    # recorded sensors
    velocity_smooth, dev_dist, grade_smooth   # device-reported channels
    dist_diff                          # QA diagnostic

Ordering is enforced by relocating to this list, not by per-function
`.after` placement.

## Testing

- Tests live in `tests/testthat/`. `helper-fixtures.R` provides
  fixture-path helpers (`fixture_activity()`, `fixture_workout()`, and
  the all-paths `fixture_activities()`, `fixture_workouts()`), the
  `meta_fields` vector, and the thorough-run helpers.
- The exhaustive loop over every fixture is gated by
  `skip_if_not_thorough()` and only runs under `test_thorough()`. Fast
  single-file tests always run.
- Activity tests live in `test-read_streams.R` and
  `test-derive_columns.R`; recordless workout behavior in
  `test-workouts.R`.

## TODO

- [`strava_activities_to_parquet()`](https://torfason.github.io/starch/reference/strava_activities_to_parquet.md)
  currently drops `attr(d, "activity_metadata")` on write, because
  [`nanoparquet::write_parquet()`](https://nanoparquet.r-lib.org/reference/write_parquet.html)
  does not carry attributes. Add an attribute-preserving write/read
  wrapper and persist the metadata through the Parquet round-trip.

## Gotchas

- Native pipe placeholder (`|> f(x = _)`) requires R \>= 4.2 – that is
  the package’s declared floor.
- Dependencies: treat `DESCRIPTION` as authoritative; do not assume the
  list from memory. FITfileR needs its `Remotes` /
  `Additional_repositories` entries to install.
- `R/hello.R` and `tests/testthat/test-hello.R` are leftover `usethis`
  stubs and should be removed together if still present.
