# Read Strava bulk-export track files (GPX / TCX / FIT) into stream tibbles.
#
# Each reader returns a tibble whose columns are a more or less a subset of the
# schema used by rStrava::get_activity_streams() (metric units), so file-based
# and API-based streams stack cleanly with dplyr::bind_rows().
#
# read_fit_stream() needs FITfileR, which is on GitHub / r-universe only:
#   install.packages("FITfileR", repos = "https://grimbough.r-universe.dev")

# Drop columns that are uniformly NA (i.e. fields the source did not provide).
# Base subsetting keeps the tibble class and avoids a tidyselect::where import.
drop_empty_cols <- function(d) {
  keep <- vapply(d, function(x) !all(is.na(x)), logical(1))
  d[keep]
}

#' Read a Strava export track file into a stream tibble
#'
#' `read_stream()` reads a single activity track file from a Strava bulk export
#' and returns a tibble of per-point stream data, dispatching on the file
#' extension to a format-specific reader. GPX, TCX, and FIT are supported,
#' including the gzip-compressed (`.gz`) files as shipped in the export. Fields
#' a given source does not provide are dropped rather than filled with `NA`.
#'
#' @param path Path to a track file. The extension (after stripping any `.gz`)
#'   must be one of `gpx`, `tcx`, or `fit`.
#' @return A tibble with one row per track point. Columns present depend on the
#'   source but are drawn from `timestamp`, `lat`, `lng`, `altitude`,
#'   `heartrate`, `cadence`, `temp`, `dev_dist`, `velocity_smooth`, `watts`,
#'   and `grade_smooth`.
#' @examples
#' \dontrun{
#' read_stream("activities/9973795459.gpx.gz")
#' }
#' @importFrom tibble tibble
#' @importFrom dplyr coalesce bind_rows
#' @importFrom lubridate ymd_hms
#' @importFrom xml2 read_xml xml_find_all xml_find_first
#' @importFrom xml2 xml_text xml_attr xml_double
#' @export
read_stream <- function(path) {
  ext <- path |> sub("\\.gz$", "", x = _) |> tools::file_ext() |> tolower()
  switch(ext,
         gpx = read_gpx_stream(path),
         tcx = read_tcx_stream(path),
         fit = read_fit_stream(path),
         stop("unsupported extension: ", ext)
  )
}

#' @describeIn read_stream Read a GPX file. Garmin TrackPointExtension fields
#'   (`hr`, `cad`, `atemp`) are read via namespace-stripped XPath, so no GDAL
#'   extension flag is required.
#' @export
read_gpx_stream <- function(path) {

  doc <- read_xml(path)
  pts <- doc |> xml_find_all("//*[local-name()='trkpt']")

  tibble(
    timestamp = pts |> xml_find_first(".//*[local-name()='time']")  |> xml_text() |> ymd_hms(tz = "UTC"),
    lat       = pts |> xml_attr("lat") |> as.numeric(),
    lng       = pts |> xml_attr("lon") |> as.numeric(),
    altitude  = pts |> xml_find_first(".//*[local-name()='ele']")   |> xml_double(),
    heartrate = pts |> xml_find_first(".//*[local-name()='hr']")    |> xml_double(),
    cadence   = pts |> xml_find_first(".//*[local-name()='cad']")   |> xml_double(),
    temp      = pts |> xml_find_first(".//*[local-name()='atemp']") |> xml_double()
  ) |>
    drop_empty_cols()
}

#' @describeIn read_stream Read a TCX file. Strava's TCX exports carry leading
#'   whitespace before the `<?xml>` declaration, so the file is read as text and
#'   trimmed before parsing.
#' @export
read_tcx_stream <- function(path) {

  con <- gzfile(path)
  withr::defer(close(con))

  doc <- con |>
    readLines(warn = FALSE) |>
    paste(collapse = "\n") |>
    trimws() |>
    read_xml()

  pts <- doc |> xml_find_all("//*[local-name()='Trackpoint']")

  tibble(
    timestamp = pts |> xml_find_first(".//*[local-name()='Time']")             |> xml_text() |> ymd_hms(tz = "UTC"),
    lat       = pts |> xml_find_first(".//*[local-name()='LatitudeDegrees']")  |> xml_double(),
    lng       = pts |> xml_find_first(".//*[local-name()='LongitudeDegrees']") |> xml_double(),
    altitude  = pts |> xml_find_first(".//*[local-name()='AltitudeMeters']")   |> xml_double(),
    heartrate = pts |> xml_find_first(".//*[local-name()='HeartRateBpm']/*[local-name()='Value']") |> xml_double(),
    dev_dist  = pts |> xml_find_first(".//*[local-name()='DistanceMeters']")   |> xml_double(),
    cadence   = pts |> xml_find_first(".//*[local-name()='Cadence']")          |> xml_double()
  ) |>
    drop_empty_cols()
}

#' @describeIn read_stream Read a FIT file (requires the FITfileR package).
#'   `records()` may return several tibbles when a file has multiple record
#'   definitions; they are bound. Record fields vary between files, so a safe
#'   getter returns `NA` for absent fields.
#' @export
read_fit_stream <- function(path) {

  if (!requireNamespace("FITfileR", quietly = TRUE)) {
    stop(
      "Package 'FITfileR' is required to read FIT files. Install with:\n",
      '  install.packages("FITfileR", repos = "https://grimbough.r-universe.dev")',
      call. = FALSE
    )
  }

  # FITfileR needs a decompressed .fit path (no connection / gzip support).
  tmp <- withr::local_tempfile(fileext = ".fit")
  R.utils::gunzip(path, destname = tmp, overwrite = TRUE, remove = FALSE)

  recs <- FITfileR::readFitFile(tmp) |> FITfileR::records()
  if (!inherits(recs, "data.frame")) recs <- bind_rows(recs)

  g <- function(nm) if (nm %in% names(recs)) recs[[nm]] else NA  # safe column

  tibble(
    timestamp       = g("timestamp"),
    lat             = g("position_lat"),
    lng             = g("position_long"),
    altitude        = coalesce(g("enhanced_altitude"), g("altitude")),
    heartrate       = g("heart_rate"),
    dev_dist        = g("distance"),
    cadence         = g("cadence"),
    velocity_smooth = coalesce(g("enhanced_speed"), g("speed")),
    watts           = g("power"),
    temp            = g("temperature"),
    grade_smooth    = g("grade")
  ) |>
    drop_empty_cols()
}
