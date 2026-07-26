# Test fixtures - sources and license

The gzipped GPS activity files in this directory are vendored from:

  TestFilesForFitnessApps
  https://github.com/msimms/TestFilesForFitnessApps
  MIT License, Copyright (c) 2018 Mike Simms (see `LICENSE-testfiles`).

They exercise starch's GPX/TCX/FIT readers. Each is gzipped to mirror a
Strava bulk export, and because `read_fit_stream()` expects gzip input.
Regenerate with `data-raw/vendor-testfiles.R`.

Note: these are Garmin Connect / device-native / other-app exports, not
Strava bulk-export files, so they do not reproduce Strava-specific quirks
(leading whitespace before the TCX `<?xml>` declaration, Strava's GPX
extension namespacing). Add a few anonymized files from a real export for
those regression cases.

## Files

- `20191117_tri_garmin_fenix_3_hr.fit.gz` (fit) - Triathlon (multisport): multiple record messages, exercises the records() -> bind_rows() path in read_fit_stream().
- `20191117_bike_wahoo_elemnt.fit.gz` (fit) - Bike, Wahoo ELEMNT: GPS plus power/cadence (populates `watts`).
- `20140622_swim_garmin_fr910xt.fit.gz` (fit) - Pool swim, Garmin FR910XT: no GPS (distance/speed branch skipped).
- `20210218_zwift_bike_race.fit.gz` (fit) - Virtual bike, Zwift: indoor trainer; verify whether it carries virtual coordinates.
- `20130720_run_garmin_forerunner_10.fit.gz` (fit) - Run, Garmin Forerunner 10: tiny smoke-test file (~1.5 KB).
- `Half_Mile_Repeats_workout.fit.gz` (fit) - Run workout definition: minimal / likely no track records (empty-stream case).
- `run_03_garmin.gpx.gz` (gpx) - Run, Garmin: TrackPointExtension hr/cad/atemp via namespace-stripped XPath.
- `20180831_beach_run_runkeeper.gpx.gz` (gpx) - Run, Runkeeper: non-Garmin GPX extension flavor.
- `hike_01_iphone.gpx.gz` (gpx) - Hike, iPhone: plain GPX (lat/lng/ele only, no extensions).
- `20181108_run_garmin_fenix_3_hr.tcx.gz` (tcx) - Run, Garmin fenix 3 HR: HR plus foot-pod cadence.
- `20180810_zwift_innsbruckring_x2.tcx.gz` (tcx) - Stationary bike, Zwift: no GPS (TCX no-lat/lng case).
- `20120611_run_garmin_fr405cx.tcx.gz` (tcx) - Run, Garmin FR405CX: older device, simpler TCX.
