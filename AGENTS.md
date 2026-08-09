# AGENTS.md

Guidance for AI agents (and humans) working in this repository. Keep it current when structure or conventions change.

## Project

`starch` is an R package for reading Strava bulk-export track files (GPX, TCX, FIT, including the gzip-compressed `.gz` files as shipped) into tidy, analysis-ready stream tibbles, and deriving per-point columns (elapsed time, cumulative distance, speed).

It also carries the pipeline that feeds those readers: importing a Strava export zip into a version-controlled repository, and converting the archived activities to Parquet. Reports reading from the Parquet layer are still to come.

- Repo: `torfason/starch` on GitHub; default branch `main`.
- Docs site: pkgdown at `torfason.github.io/starch/`.
- License: MIT
- CI: R-CMD-check (single-platform quickstart workflow).

## Setup and common commands

Run from the package root in R:

```r
devtools::load_all()     # load package for interactive dev
devtools::document()     # regenerate NAMESPACE and man/*.Rd after roxygen changes
devtools::test()         # run the test suite (fast; skips the thorough loop)
devtools::check()        # full R CMD check
test_thorough()          # run tests including the slow exhaustive fixture loop
```

`test_thorough()` is a helper (see `tests/testthat/helper-fixtures.R`) that sets `STARCH_TEST_THOROUGH=true` for one `devtools::test()` run.

## Architecture

Source lives in `R/`:

- `read_streams.R` – the reader family. `read_stream(path)` dispatches on the extension (after stripping `.gz`) to `read_gpx_stream()`, `read_tcx_stream()`, or `read_fit_stream()`. Each reads the file **once** and returns a stream tibble (one row per track point) with columns drawn from `timestamp`, `lat`, `lng`, `altitude`, `heartrate`, `cadence`, `temp`, `dev_dist`, `velocity_smooth`, `watts`, `grade_smooth`. `drop_empty_cols()` removes any column that is entirely `NA`.
- `derive_columns.R` – the `addcols_*` transforms: `addcols_time()`, `addcols_distance()` (adds `distance`, plus `dist_diff` when device distance `dev_dist` is present), `addcols_speed()` (smoothed), `addcols_speed_naive()`, `addcols_latlng_offset()` (coordinates relative to the first fix, since the absolute values dwarf the within-activity variation). Pure tibble → tibble; documented as one family via `@describeIn`. Also holds `activity_col_order` and `relocate_activity_cols()`.
- `strava_repo.R` – export import. `latest_strava_zip()` picks the most recent archive from a directory; `strava_zip_to_repo()` extracts it into a git repository, gzips the tracks that arrived uncompressed, and commits.
- `activities_parquet.R` – `activity_streams_to_parquet()` converts archived activities to one Parquet file each, rebuilding only what changed. The internal `content_check()` implements the staleness test.
- `dash.R` – the public dashboard API: `dash_render()`, `dash_view()`, `dash_update_heatmap()`. Every stack keeps its own prefixed functions internal, and these resolve to the static Quarto stack. If another stack becomes the default, this is the only file that changes.
- `press.R` – `press()`, the one-call maintenance run: import the export archive, convert to Parquet, render the static dashboard, open it. Prompts before each step with the step's context, and takes every default as an argument so the sequence can also run unattended with `confirm = FALSE`. Sits above the stacks, so it is unprefixed.
- `dashboard_common.R` – shared by every dashboard stack, and belonging to none of them. `load_activities_csv()` reads the export manifest, `parquet_stream_stats()` reads per-activity statistics out of the Parquet footers, and `require_pkgs()` checks the render-time Suggests. Nothing here is prefixed, and nothing here may depend on a particular stack.
- `rmd_dashboard.R` – the Rmd dashboard. `rmd_render_dashboard()` renders one flexdashboard page per activity into `strava_repo/dashboard_rmd/`, plus a reactable overview table and a static index. Templates in `inst/rmd_templates/`.
- `quarto_dynamic_dashboard.R` – the dynamic Quarto dashboard, a prototype. `qd_render_dashboard()` builds `strava_repo/dashboard_qd/` from templates in `inst/quarto_dynamic_templates/`. Static shells plus data injected as classic scripts; see *No ES modules, no fetch* below.
- `quarto_static_dashboard.R` – the static Quarto dashboard. `qs_render_dashboard()` renders one page per activity into `strava_repo/dashboard_qs/` from templates in `inst/quarto_static_templates/`, then three overview pages (`overview_list.html`, `overview_table.html`, `overview_heatmap.html`, each from the qmd of the same name) and finally `index.html`, the navigation shell. Activity pages are nested under `activities/`, and their template lives in the matching `activities/` subdirectory of the project so that Quarto resolves `site_libs/` relative to it. Quarto templates are staged to a temp dir before rendering, because the installed copy is read-only and a Quarto project render writes into its own tree. The shell is assembled by R from `inst/quarto_static_shell/`, which is deliberately outside the Quarto project: it is plain HTML with glue placeholders, so adding an activity rebuilds one small file rather than re-rendering every page, which is what a Quarto-native sidebar would cost.

### Pipeline stages

```
strava_zips/*.zip  --strava_zip_to_repo()-->  strava_repo/
strava_repo/activities/  --activity_streams_to_parquet()-->  strava_repo/activities_parquet/
```

The repository holds the export verbatim under version control: every import extracts the whole archive and overwrites, because activities can be edited in Strava after the fact, so a file's presence says nothing about whether its content is current. Git determines what actually changed. Generated artefacts (`activities_parquet/`, `activities_hashes/`) live inside the repository but are held out of it by the `.gitignore` written at init, from the `strava_repo_ignore` constant.

`strava_zip_to_repo()` refuses to run unless the target is either empty or a git repository with a clean working tree. That guard is what makes unattended overwriting safe, and it also makes a failed import recoverable with `git reset --hard`.

### Parquet staleness

Because every import rewrites the whole archive, modification times say nothing about whether an activity changed. `content_check()` instead records each input's md5 as an **empty marker file** in `activities_hashes/`, named for the hash and stamped with the time that content was first seen. An activity is rebuilt when its Parquet output is older than its marker – which happens only when the bytes genuinely differ. The plain mtime comparison is returned alongside as `semistale`, for comparison only.

A conversion that fails leaves an old or absent output against a fresh marker, so it stays stale and is retried on the next run.

### The dashboard stacks

Three stacks run side by side, so they can be compared and any of them dropped without disturbing the others. Each owns a function prefix, a source-file prefix, a template directory and an output directory, and shares nothing but `dashboard_common.R`:

| Stack | Functions | Sources | Templates | Output |
|---|---|---|---|---|
| Rmd | `rmd_` | `rmd_*.R` | `inst/rmd_templates/` | `strava_repo/dashboard_rmd/` |
| Quarto dynamic | `qd_` | `quarto_dynamic_*.R` | `inst/quarto_dynamic_templates/` | `strava_repo/dashboard_qd/` |
| Quarto static | `qs_` | `quarto_static_*.R` | `inst/quarto_static_templates/` | `strava_repo/dashboard_qs/` |

The Quarto-static stack is the closest port of the Rmd one: same charts, same
per-activity page, built by Quarto instead of rmarkdown. Its templates are plain
qmd documents parameterised by `parquet_path`, so a page can be reproduced by
hand outside the package. Because the pages are ordinary Quarto documents in a
website project, the library assets they need land once in `site_libs/` rather
than once per page.

All of them read the same manifest and the same Parquet statistics – `qd_activities_table()` calls `load_activities_csv()` and `parquet_stream_stats()` rather than reimplementing them – so they cannot disagree on numbers. Rmd and Quarto-dynamic differ in where the data lives:

| | Rmd (`dashboard_rmd/`) | Quarto dynamic (`dashboard_qd/`) |
|---|---|---|
| Detail pages | one HTML per activity | one `detail_a.html`, chosen by `?id=` |
| Data | baked into each page | `data/*.js`, pages are static shells |
| Adding activities | renders N pages | writes N data files, renders nothing |
| Charts | plotly + leaflet | Observable Plot (UMD) |
| Table | reactable widget | hand-rolled, reads `data/activities.js` |

The Quarto-dynamic output layout is:

```
dashboard_qd/
  dash_index.html      # static shell, builds its sidebar from the manifest
  dash_overview.html   # trends chart + activity table
  detail_a.html        # one activity, selected by ?id= at view time
  index.html           # redirect to dash_index.html
  site_libs/           # Quarto's own assets, shared by both pages
  lib/                 # d3, Plot, starch-dash.js, starch-dash.css
  data/                # activities.js, act_<activity_id>.js
```

Sources live in `inst/quarto_dynamic_templates/`. Files under `_static/` and the `_quarto.yml` are underscore-prefixed so a project render ignores them; R copies `_static/` into the output's `lib/` itself.

### No ES modules, no fetch

This is the constraint the whole Quarto design turns on. The dashboard must open by double-clicking the file, and on a `file://` URL the browser gives the document an opaque origin, which blocks **ES module scripts** and **`fetch()`**. So:

- Every script is a *classic* script. `d3` and `Plot` are loaded as UMD builds, which is why they are vendored rather than imported.
- Data is delivered by injecting further classic `<script>` tags. `data/activities.js` and `data/act_<id>.js` are JSON wrapped in an assignment for exactly this reason.
- `history.replaceState()` also throws on `file://` in Chrome, so the index guards it in a `try`/`catch`.

This is also why the pages do not use Observable JS despite being Quarto documents. Quarto loads its OJS runtime as a module and ships an explicit `file://` guard; `Inputs` and `Plot` are then lazy-loaded from `cdn.jsdelivr.net` by Observable's `require()`, so even with the guard removed and `embed-resources: true` the page would need the network on every open. The upstream issue is closed `wontfix` (quarto-cli#6371).

Do not "modernize" this code to `import`/`fetch` without first deciding to give up opening the dashboard from disk.

### Per-activity metadata

Each reader also extracts activity-level (not per-point) metadata in the same pass and attaches it as a single flat attribute, `attr(d, "activity_metadata")`, built by the internal `new_stream_meta()`. It is a flat named list with a fixed field set: `format`, `source`, `sport`, `sub_sport`, `title`, `start_time`, `n_sessions`, `total_distance`, `total_timer_time`, `total_calories`. Design
rules:

- Flat only – no nested values – so it converts to a one-row table via `tibble::as_tibble()`.
- When a source offers several values for one field (e.g. a multisport FIT file), they are collapsed into one slash-joined string (`"swimming/cycling"`).
- Format inconsistencies are **not** normalized (TCX `Biking` vs FIT `cycling`, case, etc.); extract as-is, resolve downstream if ever needed.
- A recordless FIT file (a workout definition, not an activity) reads as a genuine empty `0 x 0` stream, still carrying the metadata attribute.

### Fixtures

`inst/extdata/` holds gzipped test files, split into `activities/` and `workouts/`. They are vendored (MIT) from `msimms/TestFilesForFitnessApps`; see `inst/extdata/SOURCES.md` and `LICENSE-testfiles`. Regenerate with `data-raw/testfiles.R`. Files are gzipped with `compression = 9`; R's `gzfile()` already omits the mtime and original filename, so re-gzipping is byte-stable (equivalent to `gzip -n`) and won't produce spurious git diffs.

## Conventions

- **Tidyverse with native pipe.** Prefer dplyr/tidyr/purrr idioms and the native `|>` pipe (not magrittr `%>%`). Before writing a from-scratch implementation,  look for an existing function in a well-known package.
- **Attributes attach last.** `[` and dplyr verbs drop custom attributes, so `activity_metadata` must be attached after `drop_empty_cols()` and any column reordering. If a step needs the attribute preserved, set it afterward.
- **One reader per format, read the file once.** Do not add separate metadata readers; points and metadata come from the same parse (memory matters – large exports, and a suspected xml2/FIT leak in long runs).
- **`.data` pronoun in package code.** Inside `mutate()` etc., reference columns as `.data$col` (imported via `@importFrom dplyr .data`) and pass tidyselect args like `.after` as strings, to avoid the "no visible binding" NOTE.
- **FIT is optional.** `FITfileR` is a `Suggests` (installed from r-universe / GitHub, not CRAN). Guard its use with `requireNamespace()` in code and `skip_if_not_installed("FITfileR")` in tests.
- **Prose style.** Use en dashes (–), not em dashes (—), in comments and docs.
- **Long-running functions report progress.** Anything that moves thousands of files (`strava_zip_to_repo()`, `activity_streams_to_parquet()`) reports each phase with `cli`, times the expensive steps with the internal `elapsed()` helper, and shows a progress bar over per-file loops. Every such function takes `quiet = FALSE`, and the reporting is guarded with `if (!quiet)` rather than suppressed wholesale.

### Canonical column order

Stream tibbles should present columns in this order (a column not listed is kept after the listed ones):

```
timestamp, time, distance          # axes
lat, lng, altitude                 # position
speed, speed_ms, speed_kmh, pace   # movement (robust; front-of-house)
heartrate, cadence, watts, temp    # recorded sensors
velocity_smooth, dev_dist, grade_smooth   # device-reported channels
dist_diff                          # QA diagnostic
lat_offset, lng_offset             # recentred position
```

`relocate_activity_cols()` is the single authority on ordering; the `.after` arguments still present in some `addcols_*` functions are vestigial and should not be relied on or extended.

## Testing

- Tests live in `tests/testthat/`. `helper-fixtures.R` provides fixture-path helpers (`fixture_activity()`, `fixture_workout()`, and the all-paths `fixture_activities()`, `fixture_workouts()`), the `meta_fields` vector, and the thorough-run helpers.
- The exhaustive loop over every fixture is gated by `skip_if_not_thorough()` and only runs under `test_thorough()`. Fast single-file tests always run.
- Activity tests live in `test-read_streams.R` and `test-derive_columns.R`; recordless workout behavior in `test-workouts.R`.
- Golden tests use `summarize_stream()` (in `helper-fixtures.R`) to reduce a stream to one row per column – name, NA count, mean, and a hash for character columns – and compare with `expect_snapshot_value(style = "json2")`, with snapshots under `_snaps/`. Comparing summaries rather than hashing whole columns is deliberate: `expect_equal()` semantics keep a numeric tolerance, whereas a digest of a double vector is a bit-exactness test that fails across platforms, since the geodesic maths in `geodist` does not agree to the last bit between macOS/ARM and Linux/x86. Regenerate with `testthat::snapshot_accept()`.
- The import and Parquet functions have no tests yet (see open tasks).

## Open tasks

- **Metadata through Parquet.** `activity_streams_to_parquet()` drops `attr(d, "activity_metadata")` on write, because `nanoparquet::write_parquet()` does not carry attributes. Add an attribute-preserving write/read wrapper and persist the metadata across the round-trip.
- **Reports.** The reporting layer reading from `activities_parquet/` is not started.
- **Quarto dashboard is a prototype.** `detail_a.html` is a spike: the route is drawn as a bare lat/lng trace rather than a map, because a tiled basemap needs the network. Streams are thinned to `qdetail_points` (600) on write. The `_a` in the name is room for further per-activity templates.
- **Pre-rendered pages.** The Quarto pages carry no data, so they could be rendered at package build time and shipped in `inst/`, which would drop Quarto from a user-facing dependency to a developer-only one. Not done yet.
- **Vendored libraries.** `qvendor_libs()` downloads d3 and Plot into the output directory on first build, so the build needs the network once, though the dashboard never does. Vendoring them into `inst/` instead would trade ~500 KB of package sources for an offline build.
- **Robust `speed` / `pace`.** The order reserves `speed` ahead of `speed_ms` for a version derived the most reliable way available per file – preferring device-reported channels (`velocity_smooth`, `dev_dist`) and falling back to computed – so that it is present whenever it can be inferred at all. `addcols_speed()` currently always computes from `distance` and `time`.
- **Tests for the pipeline.** Neither `strava_zip_to_repo()` nor `activity_streams_to_parquet()` has tests. Build a synthetic mini-export with `zip::zip()` from the `inst/extdata` fixtures rather than using a real export, which contains personal data. The invariant worth asserting is idempotence: run twice, and the second run extracts nothing, converts nothing, and makes no commit.
- **One tree walk too many.** `strava_zip_to_repo()` calls `git_status()` before staging purely to count changes for the commit message. Running `git_add(".")` first and then `git_status(staged = TRUE)` would cut a full walk, which is minutes on a large import.
- **`activities_html/`** is not in `strava_repo_ignore`; add it if HTML reports land inside the repository.

## Open questions

- **Empty streams.** A recordless FIT file reads as `0 x 0`, which has no Parquet schema, so `activity_streams_to_parquet()` substitutes a zero-row tibble with a single `timestamp` column in order to write something readable. The log records the original `0, 0` shape. Confirm this is the behaviour wanted, and check what `nanoparquet` actually does with a zero-column frame.
- **Offsets in Parquet.** `lat_offset` / `lng_offset` are an inspection aid but are currently persisted, keeping the Parquet shape identical to the canonical stream. Alternative is to drop them on write and recompute on read.
- **The conversion log.** `activity_streams_to_parquet()` returns a per-activity log and does not write it anywhere. Whether it should be persisted, and where, is unresolved.
- **Deleted activities.** Activities deleted in Strava vanish from later exports but persist in the repository. Archive semantics argue for keeping them, but it is a silent divergence from Strava's state.
- **`commit = FALSE`** leaves a dirty working tree, which blocks the next import until it is resolved. Intended, but it makes the flag deliberately unpleasant.

## Gotchas

- Native pipe placeholder (`|> f(x = _)`) requires R >= 4.2 – that is the package's declared floor.
- Dependencies: treat `DESCRIPTION` as authoritative; do not assume the list from memory. FITfileR needs its `Remotes` / `Additional_repositories` entries to install.
- gzip byte-stability holds **within** a machine, not across them: the OS byte is fixed at build time and the deflate stream can differ between zlib versions. Since the Parquet markers hash the compressed file, moving to another machine invalidates every marker at once and forces a full rebuild. Acceptable, but not free.
- `tools::md5sum()` takes file paths, not strings. The hash branch in `summarize_stream()` only fires for character columns, of which stream tibbles currently have none; if one ever appears it will yield `NA` rather than a hash.
