# Import a Strava export archive into a version-controlled repository

Extracts a Strava bulk export into a git repository, gzips the track
files that arrived uncompressed, and commits the result. Running it
against successive exports builds a version-controlled history of the
archive, in which each commit is one export and the diff is whatever
actually changed.

## Usage

``` r
strava_zip_to_repo(
  zip = latest_strava_zip(),
  repo = here("strava_repo"),
  commit = TRUE,
  author = NULL,
  quiet = FALSE
)
```

## Arguments

- zip:

  Path to a Strava export archive. Defaults to the most recent archive
  found by
  [`latest_strava_zip()`](https://torfason.github.io/starch/reference/latest_strava_zip.md).

- repo:

  Path to the repository the export is imported into.

- commit:

  Whether to commit the result. When `FALSE` the changes are left in the
  working tree, which will block the next import until they are dealt
  with.

- author:

  Optional git signature for the commit, as accepted by
  [`gert::git_commit()`](https://docs.ropensci.org/gert/reference/git_commit.html).
  When `NULL`, the configured git identity is used and its absence is
  reported before any files are written.

- quiet:

  Suppress progress reporting.

## Value

The
[`gert::git_status()`](https://docs.ropensci.org/gert/reference/git_commit.html)
table of what changed, invisibly.

## Details

Every export is extracted in full, overwriting what is already there.
Activities can be edited in Strava after the fact, so the presence of a
file in the repository says nothing about whether its content is
current. Git, not the file system, determines what changed.

## Repository preconditions

Because the import overwrites files without prompting, `repo` must be in
one of exactly two states, checked before anything is written:

- **Empty or absent** - the directory is created, a repository is
  initialised, and a `.gitignore` is written.

- **A git repository with a clean working tree** - no staged changes, no
  unstaged changes, and no untracked files. Ignored files do not count.

Anything else is an error, on the assumption that it is an unrelated
directory. The clean-tree requirement also makes a failed import
recoverable: nothing predates the run, so `git reset --hard` undoes it.

Archives containing entries under `.git/`, absolute paths, or parent
traversal are rejected outright.
