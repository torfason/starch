# Locate the most recent Strava export archive

Strava bulk exports arrive as a zip archive. This picks the most recent
one out of a directory of downloaded exports, and is the default source
for
[`strava_zip_to_repo()`](https://torfason.github.io/starch/reference/strava_zip_to_repo.md).

## Usage

``` r
latest_strava_zip(dir = here("strava_zips"), by = c("both", "name", "time"))
```

## Arguments

- dir:

  Directory holding downloaded Strava export archives.

- by:

  How the most recent archive is identified. `"both"` (the default)
  requires the name and modification time orderings to agree, `"name"`
  uses the file name alone, and `"time"` the modification time alone.

## Value

Path to the most recent `.zip` archive in `dir`.

## Details

"Most recent" can mean either the last file name or the last
modification time. These usually agree, but need not: copying or
restoring files rewrites modification times, and name ordering only
tracks chronology if the archives are named so that they sort that way.
The default (`"both"`) requires the two to agree and errors when they do
not, rather than silently picking one.
