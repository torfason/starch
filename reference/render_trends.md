# Render the trends page

Writes `dashboard/trends.html`, an interactive view of totals over time.
Distance, activity count, elapsed time or moving time are bucketed by
week or month over a period measured back from the newest activity, for
any selection of sports. It is built entirely from `activities.csv`, so
it is complete across the whole history even while most activities lack
a detail page. The controls re-aggregate client-side; there is no server
component.

## Usage

``` r
render_trends(repo = here("strava_repo"), quiet = FALSE)
```

## Arguments

- repo:

  Path to the Strava repository.

- quiet:

  Suppress progress reporting.

## Value

Path to the written page, invisibly.
