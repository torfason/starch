# Convert repository activities to Parquet

Reads every stream file in a Strava repository's `activities/`
directory, derives the standard columns, and writes one Parquet file per
activity into `activities_parquet/`. Only activities whose *content* has
changed are rebuilt.

## Usage

``` r
activity_streams_to_parquet(
  repo = here("strava_repo"),
  max_files = 400,
  quiet = FALSE
)
```

## Arguments

- repo:

  Path to the Strava repository, as produced by
  [`strava_zip_to_repo()`](https://torfason.github.io/starch/reference/strava_zip_to_repo.md).

- max_files:

  Maximum number of stale activities to convert in one call.

- quiet:

  Suppress progress reporting.

## Value

A tibble logging one row per converted activity, invisibly.

## Staleness

Every import performed by
[`strava_zip_to_repo()`](https://torfason.github.io/starch/reference/strava_zip_to_repo.md)
rewrites the whole archive, so file modification times say nothing about
whether an activity actually changed. Instead, each input's md5 is
recorded as an empty marker file in `activities_hashes/`, named for the
hash and stamped with the time that content was first seen. An activity
is rebuilt when its Parquet output is older than that marker, which
happens only when the bytes genuinely differ.

Note that the hash covers the compressed file, so recompressing the
archive on a different machine invalidates every marker at once and
forces a full rebuild.

## Failures

A stream that will not parse is a hard error that aborts the run, on the
basis that a file the readers cannot handle is a bug to fix rather than
a row to skip. The offending file is named in the error.

Streams that parse but carry no records are written out as empty Parquet
files, so that they count as converted and are not retried on every run.
