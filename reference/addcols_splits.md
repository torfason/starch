# Add rolling split columns

Adds one column per window in `distance`, each holding a rolling measure
over the preceding window: either the pace across it, or the time taken
to cover it. Every value looks strictly backwards, so `min(pace_10k)`
over an activity is the fastest 10 km run within it.

## Usage

``` r
addcols_splits(
  d,
  distance = c(1, 5, 10),
  units = c("km", "m", "miles"),
  type = c("pace", "time")
)
```

## Arguments

- d:

  A stream tibble carrying `distance` and `time`. If either is absent,
  or there are too few distinct distances to interpolate between, `d` is
  returned unchanged.

- distance:

  Window sizes, in `units`.

- units:

  Unit the windows are expressed in.

- type:

  Whether each column holds the pace across its window or the time taken
  to cover it.

## Value

`d` with one column appended per window that yielded any data.

## Details

The point exactly one window back almost never coincides with a recorded
sample, so the time at that distance is interpolated rather than snapped
to the nearest row. Values are `NA` over the opening stretch, where the
window would reach back beyond the start of the track.

## Column names and units

Names are the type, the window, and the unit suffix: `pace_1k`,
`time_5k`, `pace_400m`, `pace_5mi`. Fractional windows replace the
decimal point with an underscore, so 21.1 km becomes `pace_21_1k`, which
needs no backticks.

`type = "pace"` is reported per kilometre for `"km"` and `"m"`, and per
mile for `"miles"`, so a 400 m split reads as min/km rather than the
useless min/metre. This matches the units of the `pace` column from
[`addcols_speed()`](https://torfason.github.io/starch/reference/derive_columns.md),
so the two can share an axis. `type = "time"` is in seconds, matching
the `time` column.

Windows longer than the activity produce nothing, and those columns are
dropped rather than carried as all-`NA`. Columns are appended in the
order given; use
[`relocate_activity_cols()`](https://torfason.github.io/starch/reference/relocate_activity_cols.md)
if a canonical order is wanted.
