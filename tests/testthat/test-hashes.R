test_that("hash_rows() hashes each row, values and types alike", {
  d <- tibble::tibble(a = c(1, 2, 2), b = c("x", "y", "y"))
  h <- hash_rows(d)

  expect_type(h, "character")
  expect_length(h, 3L)
  expect_identical(h[[2]], h[[3]])
  expect_false(h[[1]] == h[[2]])

  # A column whose storage type changed is a change.
  expect_false(
    hash_rows(tibble::tibble(a = 1L)) == hash_rows(tibble::tibble(a = 1))
  )
  expect_length(hash_rows(d[0, ]), 0L)
})


test_that("sidecars round-trip through DCF", {
  repo <- withr::local_tempdir()
  path <- hash_path(repo, "activity_html", "9876543210")

  expect_identical(
    as.character(path),
    as.character(fs::path(repo, "hashes/activity_html/9876543210.dcf"))
  )
  expect_length(hash_read_one(path), 0L)

  hash_write_one(path, c(parquet = "aa11", row = "bb22"))
  back <- hash_read_one(path)
  expect_setequal(names(back), c("parquet", "row"))
  expect_identical(unname(back[c("parquet", "row")]), c("aa11", "bb22"))
})


test_that("an unreadable sidecar is stale rather than an error", {
  repo <- withr::local_tempdir()
  out <- fs::path(repo, "a.out")
  hashfile <- hash_path(repo, "st", "a")
  fs::file_create(out)
  fs::dir_create(fs::path_dir(hashfile))
  current <- tibble::tibble(x = "1")

  # read.dcf() parses most junk as some field or other and chokes on the rest.
  # Either way the field asked for is missing or wrong, so the row is stale.
  for (junk in c("not dcf: [{", "", "  leading continuation")) {
    writeLines(junk, hashfile)
    expect_true(hash_check("a", out, hashfile, current)$stale)
  }
})


test_that("hash_check() reports why a row is stale", {
  repo <- withr::local_tempdir()
  keys <- c("a", "b", "c", "d")
  out <- fs::path(repo, paste0(keys, ".out"))
  hashfiles <- hash_path(repo, "st", keys)
  current <- tibble::tibble(x = c("1", "2", "3", "4"), y = rep("9", 4))

  fs::file_create(out[2:4])                          # a has no output
  hash_write_one(hashfiles[[3]], c(x = "3", y = "9")) # c is current
  hash_write_one(hashfiles[[4]], c(x = "moved", y = "9"))
                                                      # b has output, no sidecar
  chk <- hash_check(keys, out, hashfiles, current)

  expect_identical(chk$reason, c("missing", "unrecorded", "", "x"))
  expect_identical(chk$stale, c(TRUE, TRUE, FALSE, TRUE))
  expect_true(all(c("x", "y") %in% names(chk)))
  expect_identical(hash_reason_summary(chk), "1 missing, 1 unrecorded, 1 x")
  expect_identical(hash_reason_summary(chk[3, ]), "")

  # Every changed field is named, and a field absent from an older sidecar
  # counts as changed.
  hash_write_one(hashfiles[[4]], c(x = "moved", y = "also"))
  expect_identical(hash_check("d", out[4], hashfiles[4], current[4, ])$reason, "x,y")
  hash_write_one(hashfiles[[4]], c(x = "4"))
  expect_identical(hash_check("d", out[4], hashfiles[4], current[4, ])$reason, "y")
})


test_that("parquet_staleness() is newest first and survives an empty repo", {
  repo <- withr::local_tempdir()
  cols <- c("key", "stale", "reason", "outfile", "hashfile", "stream", "infile")

  # paste0() treats a zero-length vector as "", so the empty cases are the ones
  # that would quietly invent a file.
  expect_identical(nrow(parquet_staleness(repo)), 0L)
  expect_true(all(cols %in% names(parquet_staleness(repo))))
  fs::dir_create(fs::path(repo, "activities"))
  expect_identical(nrow(parquet_staleness(repo)), 0L)

  for (id in c("100", "2000", "30")) {
    writeLines(paste("content", id), fs::path(repo, "activities", paste0(id, ".gpx")))
  }
  s <- parquet_staleness(repo)
  expect_identical(s$key, c("2000", "100", "30"))
  expect_true(all(s$stale))
  expect_true(all(s$reason == "missing"))
})


test_that("parquet_staleness() tracks content, not modification time", {
  repo <- withr::local_tempdir()
  fs::dir_create(fs::path(repo, "activities"))
  fs::dir_create(fs::path(repo, "activities_parquet"))
  for (id in c("100", "2000")) {
    writeLines(paste("content", id), fs::path(repo, "activities", paste0(id, ".gpx")))
  }

  convert <- function(s, i) {
    fs::file_create(s$outfile[[i]])
    hash_write_one(s$hashfile[[i]], c(stream = s$stream[[i]]))
  }
  s <- parquet_staleness(repo)
  convert(s, 1)
  convert(s, 2)
  expect_false(any(parquet_staleness(repo)$stale))

  # An edited stream is stale, and says so.
  writeLines("content 2000 edited", fs::path(repo, "activities", "2000.gpx"))
  s <- parquet_staleness(repo)
  expect_identical(s$stale, c(TRUE, FALSE))
  expect_identical(s$reason[[1]], "stream")

  # Rewriting the former bytes goes quiet again, however new the mtime. This is
  # the case that matters: every import rewrites the whole archive.
  writeLines("content 2000", fs::path(repo, "activities", "2000.gpx"))
  expect_false(any(parquet_staleness(repo)$stale))

  # An activity changing to content another activity already produced is still
  # detected. The superseded content-marker scheme missed exactly this.
  writeLines("content 2000", fs::path(repo, "activities", "100.gpx"))
  s <- parquet_staleness(repo)
  expect_identical(s$stale, c(FALSE, TRUE))
  expect_identical(s$reason[[2]], "stream")
})
