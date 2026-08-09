# The public face of the dashboard. Every stack keeps its own prefixed
# functions internal; these are the ones a user calls, and they currently
# resolve to the static Quarto stack. If another stack becomes the default,
# this is the only file that changes.

#' Render the activity dashboard
#'
#' Renders outstanding activity pages, then the overview pages, then the
#' navigation index so that it links to whatever now exists.
#'
#' The heat map is not rebuilt by default. It reads every Parquet file in the
#' repository and takes far longer than the rest of the dashboard put together,
#' while changing very little between runs. It is built when missing, and
#' otherwise only when asked for; see [dash_update_heatmap()].
#'
#' @param repo Path to the Strava repository.
#' @param max_files Maximum number of activity pages to render in one call.
#' @param max_points Stream points kept in each page's charts.
#' @param update_heatmap Rebuild the heat map even when it already exists.
#' @param verbose Pass the Quarto CLI's own output through.
#' @param quiet Suppress progress reporting.
#'
#' @return Path to the index page, invisibly.
#' @export
dash_render <- function(repo = here("strava_repo"),
                        max_files = 10,
                        max_points = 600,
                        update_heatmap = FALSE,
                        verbose = FALSE,
                        quiet = FALSE) {
  if (!quiet) cli::cli_h1("Rendering dashboard")

  qs_render_activities(
    repo,
    max_files = max_files, max_points = max_points,
    verbose = verbose, quiet = quiet
  )
  qs_render_overview_list(repo, verbose = verbose, quiet = quiet)
  qs_render_overview_table(repo, verbose = verbose, quiet = quiet)

  heatmap <- fs::path(repo, "dashboard_qs", "overview_heatmap.html")
  if (update_heatmap || !file.exists(heatmap)) {
    qs_render_overview_heatmap(repo, verbose = verbose, quiet = quiet)
  } else if (!quiet) {
    cli::cli_alert_info(
      "Skipping {.file overview_heatmap.html} \\
       (exists; pass {.code update_heatmap = TRUE} to rebuild)"
    )
  }

  qs_render_index(repo, quiet = quiet)
}


#' Rebuild the route heat map
#'
#' Rebuilds `overview_heatmap.html` and then the index, so that a heat map
#' built for the first time is linked from the sidebar. Separate from
#' [dash_render()] because it reads every Parquet file in the repository.
#'
#' @param repo Path to the Strava repository.
#' @param types Activity types to include, or `NULL` for all.
#' @param grid_digits Decimal places to round coordinates to before counting.
#'   `NULL` keeps every point at full precision and does not aggregate.
#' @param max_points Cap on the number of grid cells written to the page, or
#'   `NULL` for no cap.
#' @param verbose Pass the Quarto CLI's own output through.
#' @param quiet Suppress progress reporting.
#'
#' @return Path to the heat map page, invisibly.
#' @export
dash_update_heatmap <- function(repo = here("strava_repo"),
                                types = NULL,
                                grid_digits = 5,
                                max_points = NULL,
                                verbose = FALSE,
                                quiet = FALSE) {
  out <- qs_render_overview_heatmap(
    repo,
    types = types, grid_digits = grid_digits, max_points = max_points,
    verbose = verbose, quiet = quiet
  )
  qs_render_index(repo, quiet = quiet)
  invisible(out)
}


#' Open the dashboard in a browser
#'
#' @param repo Path to the Strava repository.
#'
#' @return Path to the index page, invisibly.
#' @export
dash_view <- function(repo = here("strava_repo")) {
  qs_view_dashboard(repo)
}
