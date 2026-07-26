# Add derived columns to a stream tibble

A family of transforms that add elapsed time, cumulative distance, and
speed to a stream tibble from
[`read_stream()`](https://torfason.github.io/starch/reference/read_stream.md).
They are pure `tibble` -\> `tibble` functions and assume rows are
ordered by `timestamp` (and that `d` is non-empty). Distance and speed
additionally require `lat`/`lng`; speed requires that `time` and
`distance` already exist, so the usual order is `addcols_time()` -\>
`addcols_distance()` -\> `addcols_speed()`.

## Usage

``` r
addcols_time(d)

addcols_distance(d)

addcols_speed(d, window = 5)

addcols_speed_naive(d)
```

## Arguments

- d:

  A stream tibble from
  [`read_stream()`](https://torfason.github.io/starch/reference/read_stream.md),
  ordered by `timestamp`.

- window:

  Half-width, in points, of the centred moving-average window used to
  smooth instantaneous speed (`.before`/`.after` in
  `slider::slide_dbl()`).

## Value

`d` with the columns described for each function inserted after the
relevant existing column.

## Details

`addcols_speed()` and `addcols_speed_naive()` are alternatives that both
emit `speed_kmh` and `pace`, so apply only one of them.

## Functions

- `addcols_time()`: Add cumulative elapsed `time` (seconds from the
  first point).

- `addcols_distance()`: Add cumulative `distance` (metres) from geodesic
  point-to-point segments. When a device-reported `dev_dist` column is
  present (TCX/FIT), also add `dist_diff` = `dev_dist` - `distance` for
  QA.

- `addcols_speed()`: Add smoothed instantaneous `speed` (m/s), plus
  `speed_kmh` and `pace` (min/km). Speed is a centred moving average of
  point-to-point `diff(distance) / diff(time)` over `window` points
  either side.

- `addcols_speed_naive()`: Add naive average `speed_ms` (m/s) from
  cumulative `distance / time`, plus `speed_kmh` and `pace` (min/km).
  Unsmoothed; this is the running average from the start, not an
  instantaneous rate.

## Examples

``` r
if (FALSE) { # \dontrun{
read_stream("activities/9973795459.gpx.gz") |>
  addcols_time() |>
  addcols_distance() |>
  addcols_speed(window = 2)
} # }
```
