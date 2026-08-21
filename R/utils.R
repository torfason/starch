# Small helpers that belong to no particular layer.

# Hash each row of a data frame, returning one hash per row.
#
# vec_chop() splits the frame into a list of one-row frames in one pass, rather
# than subsetting per row, and this formulation benchmarks an order of
# magnitude faster than the obvious alternatives (pasting each row into a
# string, or applying a hasher column-wise and combining). rlang::hash() is
# likewise the fastest of the available object hashers, so the pair is what the
# package uses wherever row-level change detection is wanted.
#
# The hash covers types and attributes as well as values, so two rows that
# print alike but differ in storage type hash differently. That is the wanted
# behaviour here: a column whose type changed is a change.
hash_rows <- function(d, hash_func = rlang::hash) {
  stopifnot(is.data.frame(d))
  stopifnot(is.function(hash_func))

  d |>
    vctrs::vec_chop() |>
    vapply(hash_func, character(1))
}
