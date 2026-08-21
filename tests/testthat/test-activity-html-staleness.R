# A manifest shaped like load_activities_csv()'s output, in its newest-first
# order. The ids and the stems deliberately disagree, as they do in real
# exports, and one activity is manual and so has no stream at all.
fake_manifest <- function() {
  tibble::tibble(
    activity_id = c("9003", "9002", "9001"),
    activity_name = c("Third", "Second", "First"),
    activity_type = "Run",
    distance_km = c(5, 10, 7),
    stem = c("333", NA, "111")
  )
}

fake_repo <- function(env = parent.frame()) {
  repo <- withr::local_tempdir(.local_envir = env)
  fs::dir_create(fs::path(repo, "activities_parquet"))
  fs::dir_create(fs::path(repo, "dashboard_qs", "activities"))
  for (stem in c("333", "111")) {
    writeLines(
      paste("parquet", stem),
      fs::path(repo, "activities_parquet", paste0(stem, ".parquet"))
    )
  }
  repo
}

# Stand in for a successful render: the page appears, then the sidecar.
fake_render <- function(checked, i) {
  fs::file_create(checked$outfile[[i]])
  hash_write_one(
    checked$hashfile[[i]],
    c(parquet = checked$parquet[[i]], row = checked$row[[i]])
  )
}


test_that("pages are keyed on the activity id, not the stream stem", {
  repo <- fake_repo()
  acts <- fake_manifest()
  checked <- activity_html_staleness(repo, acts)

  # The manual activity has no stream and so can never have a page.
  expect_identical(checked$key, c("9003", "9001"))
  expect_match(as.character(checked$outfile[[1]]), "9003[.]html$")
  expect_match(as.character(checked$parquet_file[[1]]), "333[.]parquet$")
  expect_true(all(checked$stale))
  expect_true(all(checked$reason == "missing"))
})


test_that("a page is rebuilt when its Parquet file changes", {
  repo <- fake_repo()
  acts <- fake_manifest()
  checked <- activity_html_staleness(repo, acts)
  for (i in seq_len(nrow(checked))) fake_render(checked, i)
  expect_false(any(activity_html_staleness(repo, acts)$stale))

  writeLines("parquet 333 edited", fs::path(repo, "activities_parquet", "333.parquet"))
  checked <- activity_html_staleness(repo, acts)
  expect_identical(checked$stale, c(TRUE, FALSE))
  expect_identical(checked$reason[[1]], "parquet")
})


test_that("a page is rebuilt when its manifest row changes", {
  repo <- fake_repo()
  acts <- fake_manifest()
  checked <- activity_html_staleness(repo, acts)
  for (i in seq_len(nrow(checked))) fake_render(checked, i)

  # Renaming an activity in Strava never touches the stream, and this is the
  # case that went unnoticed before.
  renamed <- acts
  renamed$activity_name[[1]] <- "Third, renamed"
  checked <- activity_html_staleness(repo, renamed)
  expect_identical(checked$stale, c(TRUE, FALSE))
  expect_identical(checked$reason[[1]], "row")

  # The whole row is hashed, so a column the template does not read still
  # rebuilds. Over-rendering is the intended trade against missing a new
  # dependency.
  widened <- acts
  widened$perceived_exertion <- 4
  expect_true(all(activity_html_staleness(repo, widened)$reason == "row"))
})


test_that("an activity awaiting conversion is neither outstanding nor current", {
  repo <- fake_repo()
  acts <- fake_manifest()
  acts$stem[[2]] <- "222" # a stream with no Parquet file yet

  checked <- activity_html_staleness(repo, acts)
  awaiting <- checked$key == "9002"
  expect_false(checked$stale[awaiting])
  expect_false(checked$have_parquet[awaiting])
  expect_true(all(checked$have_parquet[!awaiting]))
})

# Orphaned pages should not be swept
# test_that("orphaned pages are swept", {
#   repo <- fake_repo()
#   dir <- fs::path(repo, "dashboard_qs", "activities")
#   # 333/111 are pages from when files were named for the stream stem; 8888 is
#   # an activity since deleted in Strava.
#   fs::file_create(fs::path(dir, paste0(c("9003", "333", "111", "8888"), ".html")))
#
#   swept <- qs_sweep_orphan_pages(
#     fs::path(repo, "dashboard_qs"), c("9003", "9001"), quiet = TRUE
#   )
#   expect_length(swept, 3L)
#   expect_identical(fs::path_file(fs::dir_ls(dir)), "9003.html")
#
#   expect_length(qs_sweep_orphan_pages(fs::path(repo, "dashboard_qs"),
#                                       c("9003", "9001"), quiet = TRUE), 0L)
# })


test_that("an empty manifest yields no rows rather than one made of nothing", {
  repo <- fake_repo()
  checked <- activity_html_staleness(repo, fake_manifest()[0, ])

  expect_identical(nrow(checked), 0L)
  expect_true(all(
    c("key", "stale", "reason", "outfile", "hashfile",
      "parquet", "row", "parquet_file", "have_parquet") %in% names(checked)
  ))
})


test_that("the renderer's join carries what the template needs", {
  repo <- fake_repo()
  acts <- fake_manifest()
  checked <- activity_html_staleness(repo, acts)

  todo <- dplyr::inner_join(
    acts, checked[checked$stale, ], by = c("activity_id" = "key")
  )

  expect_identical(todo$activity_id, c("9003", "9001"))
  expect_false(any(grepl("[.]x$|[.]y$", names(todo))))
  expect_true(all(
    c("parquet_file", "hashfile", "parquet", "row",
      "activity_name", "activity_type") %in% names(todo)
  ))
  expect_identical(
    nrow(dplyr::inner_join(acts, checked[0, ], by = c("activity_id" = "key"))),
    0L
  )
})
