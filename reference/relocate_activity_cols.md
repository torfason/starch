# Reorder stream columns into the canonical activity layout

Relocates whichever canonical stream columns are present into a fixed
order (axes, position, speed/pace, recorded sensors, device-reported
channels, then diagnostics). Uses
[`dplyr::any_of()`](https://tidyselect.r-lib.org/reference/all_of.html),
so absent columns are skipped rather than raising an error, and any
column not in the canonical list is kept, in its existing relative
order, after the listed ones.

## Usage

``` r
relocate_activity_cols(d)
```

## Arguments

- d:

  A stream tibble, e.g. from
  [`read_stream()`](https://torfason.github.io/starch/reference/read_stream.md)
  after the `addcols_*` transforms.

## Value

`d` with columns relocated; contents and row order unchanged.
