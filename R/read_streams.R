# Read Strava bulk-export track files (GPX / TCX / FIT) into stream tibbles.
#
# Each reader returns a tibble whose columns are more or less a subset of the
# schema used by rStrava::get_activity_streams() (metric units), so file-based
# and API-based streams stack cleanly with dplyr::bind_rows().
#
# In one pass, each reader also extracts per-activity (not per-point) metadata
# and attaches it as a single flat attribute, attr(d, "activity_metadata"). See
# new_stream_meta() for the fields.
#
# read_fit_stream() needs FITfileR, which is on GitHub / r-universe only:
#   install.packages("FITfileR", repos = "https://grimbough.r-universe.dev")

#' Read a Strava export track file into a stream tibble
#'
#' `read_stream()` reads a single activity track file from a Strava bulk export
#' and returns a tibble of per-point stream data, dispatching on the file
#' extension to a format-specific reader. GPX, TCX, and FIT are supported,
#' including the gzip-compressed (`.gz`) files as shipped in the export. Fields
#' a given source does not provide are dropped rather than filled with `NA`.
#'
#' Each returned tibble also carries per-activity metadata as `attr(x,
#' "activity_metadata")`: a flat named list with elements `format`, `source`,
#' `sport` (all sports found, slash-joined), `sub_sport`, `title`, `start_time`,
#' `n_sessions`, `total_distance`, `total_timer_time`, and `total_calories`,
#' with `NA` for anything the source does not provide. It is intentionally flat
#' (no nesting) so it converts to a one-row table via `tibble::as_tibble()`.
#' Subsetting and many dplyr verbs drop attributes, so persist it deliberately.
#'
#' @param path Path to a track file. The extension (after stripping any `.gz`)
#'   must be one of `gpx`, `tcx`, or `fit`.
#' @return A tibble with one row per track point, drawn from `timestamp`, `lat`,
#'   `lng`, `altitude`, `heartrate`, `cadence`, `temp`, `dev_dist`,
#'   `velocity_smooth`, `watts`, and `grade_smooth`, plus a
#'   `"activity_metadata"` attribute (see Details above).
#' @examples
#' \dontrun{
#' d <- read_stream("activities/9973795459.gpx.gz")
#' attr(d, "activity_metadata")
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



# Drop columns that are uniformly NA (i.e. fields the source did not provide).
# Base subsetting keeps the tibble class and avoids a tidyselect::where import.
drop_empty_cols <- function(d) {
  keep <- vapply(d, function(x) !all(is.na(x)), logical(1))
  d[keep]
}

# ---- Per-activity metadata -------------------------------------------------
# A flat, one-value-per-field record of activity-level metadata, attached to the
# returned stream tibble as attr(d, "activity_metadata"). Every reader emits the
# same fields (missing ones are NA), and the record is deliberately flat - no
# nested attributes - so a set of them rbinds / as_tibble()s straight into a
# table:
#
#   tibble::as_tibble(attr(d, "activity_metadata"))
#
# Where a format offers several values for one field (e.g. the sports of a
# multisport FIT file), they are collapsed into one slash-joined string; splits
# can be recovered later with strsplit(x, "/").
#
# Note: subsetting and many dplyr verbs drop this attribute, so persist it
# deliberately (e.g. via a nanoparquet attribute wrapper when writing).
new_stream_meta <- function(...) {
  m <- list(
    format           = NA_character_,   # "gpx" / "tcx" / "fit"
    source           = NA_character_,   # creator / device / app
    sport            = NA_character_,   # all sports found, slash-joined
    sub_sport        = NA_character_,   # FIT sub-sports, slash-joined
    title            = NA_character_,   # activity name / notes / workout name
    start_time       = as.POSIXct(NA, tz = "UTC"),
    n_sessions       = NA_integer_,     # tracks / activities / sessions in file
    total_distance   = NA_real_,        # metres (device-reported)
    total_timer_time = NA_real_,        # seconds (device-reported)
    total_calories   = NA_real_
  )
  o <- list(...)
  m[names(o)] <- o
  m
}

# XML helpers: return NA (not empty) when the node/attribute is absent, and
# collapse multiple hits into one slash-joined string.
.txt1 <- function(node, xpath) {
  x <- xml_find_first(node, xpath) |> xml_text() |> trimws()
  if (length(x) != 1 || is.na(x) || !nzchar(x)) NA_character_ else x
}
.attr1 <- function(node, attr) {
  x <- xml_attr(node, attr)
  if (length(x) != 1 || is.na(x) || !nzchar(x)) NA_character_ else x
}
.join_txt <- function(node, xpath) {
  x <- xml_find_all(node, xpath) |> xml_text() |> trimws()
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) NA_character_ else paste(unique(x), collapse = "/")
}
.join_attr <- function(node, xpath, attr) {
  x <- xml_find_all(node, xpath) |> xml_attr(attr) |> trimws()
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) NA_character_ else paste(unique(x), collapse = "/")
}
.parse_time1 <- function(x) {
  if (is.na(x)) return(as.POSIXct(NA, tz = "UTC"))
  ymd_hms(x, tz = "UTC", quiet = TRUE)
}

# FIT metadata, pulled from the non-record messages of an already-read FitFile.
# Called only after the FITfileR availability check in read_fit_stream().
fit_meta <- function(fit) {
  types <- FITfileR::listMessageTypes(fit)
  msg <- function(type) {
    if (!type %in% types) return(NULL)
    m <- FITfileR::getMessagesByType(fit, type)
    if (!inherits(m, "data.frame")) m <- bind_rows(m)
    m
  }
  col <- function(df, nm) if (!is.null(df) && nm %in% names(df)) df[[nm]] else NULL
  jn  <- function(...) {
    x <- as.character(unlist(list(...), use.names = FALSE))
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x)) NA_character_ else paste(unique(x), collapse = "/")
  }
  sm  <- function(x) {
    if (is.null(x)) return(NA_real_)
    x <- suppressWarnings(as.numeric(x))
    if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  }
  tm  <- function(x) {
    if (is.null(x)) return(as.POSIXct(NA, tz = "UTC"))
    if (!inherits(x, "POSIXct")) x <- suppressWarnings(ymd_hms(as.character(x), tz = "UTC", quiet = TRUE))
    if (all(is.na(x))) as.POSIXct(NA, tz = "UTC") else min(x, na.rm = TRUE)
  }

  fid <- msg("file_id")
  ses <- msg("session")
  wkt <- msg("workout")

  manu <- col(fid, "manufacturer")
  prod <- col(fid, "garmin_product"); if (is.null(prod)) prod <- col(fid, "product")

  new_stream_meta(
    format           = "fit",
    source           = jn(if (is.null(manu)) NA else manu[1],
                          if (is.null(prod)) NA else prod[1]),
    sport            = jn(col(ses, "sport"), col(wkt, "sport")),
    sub_sport        = jn(col(ses, "sub_sport")),
    title            = { w <- col(wkt, "wkt_name"); if (is.null(w)) NA_character_ else as.character(w[1]) },
    start_time       = tm(col(ses, "start_time")),
    n_sessions       = if (is.null(ses)) NA_integer_ else nrow(ses),
    total_distance   = sm(col(ses, "total_distance")),
    total_timer_time = sm(col(ses, "total_timer_time")),
    total_calories   = sm(col(ses, "total_calories"))
  )
}


#' @describeIn read_stream Read a GPX file. Garmin TrackPointExtension fields
#'   (`hr`, `cad`, `atemp`) are read via namespace-stripped XPath, so no GDAL
#'   extension flag is required.
#' @export
read_gpx_stream <- function(path) {

  doc <- read_xml(path)
  pts <- doc |> xml_find_all("//*[local-name()='trkpt']")

  d <- tibble(
    timestamp = pts |> xml_find_first(".//*[local-name()='time']")  |> xml_text() |> ymd_hms(tz = "UTC"),
    lat       = pts |> xml_attr("lat") |> as.numeric(),
    lng       = pts |> xml_attr("lon") |> as.numeric(),
    altitude  = pts |> xml_find_first(".//*[local-name()='ele']")   |> xml_double(),
    heartrate = pts |> xml_find_first(".//*[local-name()='hr']")    |> xml_double(),
    cadence   = pts |> xml_find_first(".//*[local-name()='cad']")   |> xml_double(),
    temp      = pts |> xml_find_first(".//*[local-name()='atemp']") |> xml_double()
  ) |>
    drop_empty_cols()

  root <- xml_find_first(doc, "/*")
  attr(d, "activity_metadata") <- new_stream_meta(
    format     = "gpx",
    source     = .attr1(root, "creator"),
    sport      = .join_txt(doc, "//*[local-name()='trk']/*[local-name()='type']"),
    title      = .join_txt(doc, "//*[local-name()='trk']/*[local-name()='name']"),
    start_time = .parse_time1(.txt1(doc, "//*[local-name()='metadata']/*[local-name()='time']")),
    n_sessions = length(xml_find_all(doc, "//*[local-name()='trk']"))
  )
  d
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

  d <- tibble(
    timestamp = pts |> xml_find_first(".//*[local-name()='Time']")             |> xml_text() |> ymd_hms(tz = "UTC"),
    lat       = pts |> xml_find_first(".//*[local-name()='LatitudeDegrees']")  |> xml_double(),
    lng       = pts |> xml_find_first(".//*[local-name()='LongitudeDegrees']") |> xml_double(),
    altitude  = pts |> xml_find_first(".//*[local-name()='AltitudeMeters']")   |> xml_double(),
    heartrate = pts |> xml_find_first(".//*[local-name()='HeartRateBpm']/*[local-name()='Value']") |> xml_double(),
    dev_dist  = pts |> xml_find_first(".//*[local-name()='DistanceMeters']")   |> xml_double(),
    cadence   = pts |> xml_find_first(".//*[local-name()='Cadence']")          |> xml_double()
  ) |>
    drop_empty_cols()

  attr(d, "activity_metadata") <- new_stream_meta(
    format     = "tcx",
    source     = .join_txt(doc, "//*[local-name()='Creator']/*[local-name()='Name']"),
    sport      = .join_attr(doc, "//*[local-name()='Activity']", "Sport"),
    title      = .join_txt(doc, "//*[local-name()='Notes']"),
    start_time = .parse_time1(.txt1(doc, "//*[local-name()='Activity']/*[local-name()='Id']")),
    n_sessions = length(xml_find_all(doc, "//*[local-name()='Activity']"))
  )
  d
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

  # Read the file once; take both records and metadata from the same object.
  fit  <- FITfileR::readFitFile(tmp)

  # records() emits a message and returns NULL when a file has no record
  # messages (e.g. workout-definition files); guard rather than let it print.
  recs <- if ("record" %in% FITfileR::listMessageTypes(fit)) FITfileR::records(fit) else NULL
  if (!inherits(recs, "data.frame")) recs <- bind_rows(recs)

  g <- function(nm) if (nm %in% names(recs)) recs[[nm]] else NA  # safe column

  d <- tibble(
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

  # Metadata extraction must never break the point read.
  attr(d, "activity_metadata") <- tryCatch(fit_meta(fit), error = function(e) new_stream_meta(format = "fit"))
  d
}
