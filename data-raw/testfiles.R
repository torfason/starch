# data-raw/vendor-testfiles.R
#
# Vendor a small, MIT-licensed set of GPX / TCX / FIT fixtures into
# inst/extdata/ for use in tests, examples, and vignettes. Each file is gzipped
# to mirror the shape of a Strava bulk export (and because read_fit_stream()
# requires gzip input). Re-run to refresh; commit the results under
# inst/extdata/ together with this script.
#
# Source: https://github.com/msimms/TestFilesForFitnessApps
#         MIT License, Copyright (c) 2018 Mike Simms
#
# One-time setup (creates data-raw/ and adds it to .Rbuildignore):
#   usethis::use_data_raw("testfiles")
# then place this file at data-raw/testfiles.R (or vendor-testfiles.R).

base_url <- "https://raw.githubusercontent.com/msimms/TestFilesForFitnessApps/master"
dest_dir <- here::here("inst/extdata")

# subdir, filename, and the code path / case each file exercises (-> SOURCES.md)
files <- tibble::tribble(
  ~subdir, ~file,                                    ~note,
  "fit",   "20191117_tri_garmin_fenix_3_hr.fit",     "Triathlon (multisport): multiple record messages, exercises the records() -> bind_rows() path in read_fit_stream().",
  "fit",   "20191117_bike_wahoo_elemnt.fit",         "Bike, Wahoo ELEMNT: GPS plus power/cadence (populates `watts`).",
  "fit",   "20140622_swim_garmin_fr910xt.fit",       "Pool swim, Garmin FR910XT: no GPS (distance/speed branch skipped).",
  "fit",   "20210218_zwift_bike_race.fit",           "Virtual bike, Zwift: indoor trainer; verify whether it carries virtual coordinates.",
  "fit",   "20130720_run_garmin_forerunner_10.fit",  "Run, Garmin Forerunner 10: tiny smoke-test file (~1.5 KB).",
  "fit",   "Half_Mile_Repeats_workout.fit",          "Run workout definition: minimal / likely no track records (empty-stream case).",
  "gpx",   "run_03_garmin.gpx",                      "Run, Garmin: TrackPointExtension hr/cad/atemp via namespace-stripped XPath.",
  "gpx",   "20180831_beach_run_runkeeper.gpx",       "Run, Runkeeper: non-Garmin GPX extension flavor.",
  "gpx",   "hike_01_iphone.gpx",                     "Hike, iPhone: plain GPX (lat/lng/ele only, no extensions).",
  "tcx",   "20181108_run_garmin_fenix_3_hr.tcx",     "Run, Garmin fenix 3 HR: HR plus foot-pod cadence.",
  "tcx",   "20180810_zwift_innsbruckring_x2.tcx",    "Stationary bike, Zwift: no GPS (TCX no-lat/lng case).",
  "tcx",   "20120611_run_garmin_fr405cx.tcx",        "Run, Garmin FR405CX: older device, simpler TCX."
)

dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

# Download each file and gzip it in place (<file> -> <file>.gz).
for (i in seq_len(nrow(files))) {
  src <- file.path(base_url, files$subdir[i], files$file[i])
  raw <- file.path(dest_dir, files$file[i])
  message("Fetching ", files$file[i])
  utils::download.file(src, raw, mode = "wb", quiet = TRUE)
  R.utils::gzip(raw, overwrite = TRUE, remove = TRUE, compression = 9)  # -> paste0(raw, ".gz")
}

# Vendor the upstream MIT license text alongside the data.
utils::download.file(
  file.path(base_url, "LICENSE"),
  file.path(dest_dir, "LICENSE-testfiles"),
  mode = "wb", quiet = TRUE
)

# Write the attribution / provenance file.
sources_md <- c(
  "# Test fixtures - sources and license",
  "",
  "The gzipped GPS activity files in this directory are vendored from:",
  "",
  "  TestFilesForFitnessApps",
  "  https://github.com/msimms/TestFilesForFitnessApps",
  "  MIT License, Copyright (c) 2018 Mike Simms (see `LICENSE-testfiles`).",
  "",
  "They exercise starch's GPX/TCX/FIT readers. Each is gzipped to mirror a",
  "Strava bulk export, and because `read_fit_stream()` expects gzip input.",
  "Regenerate with `data-raw/vendor-testfiles.R`.",
  "",
  "Note: these are Garmin Connect / device-native / other-app exports, not",
  "Strava bulk-export files, so they do not reproduce Strava-specific quirks",
  "(leading whitespace before the TCX `<?xml>` declaration, Strava's GPX",
  "extension namespacing). Add a few anonymized files from a real export for",
  "those regression cases.",
  "",
  "## Files",
  "",
  sprintf("- `%s.gz` (%s) - %s", files$file, files$subdir, files$note)
)
writeLines(sources_md, file.path(dest_dir, "SOURCES.md"))

message("Done. Wrote ", nrow(files), " fixtures to ", dest_dir, "/")
