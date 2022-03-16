test_that("multiplication works", {
  grid <- as.integer(c(1, 1, 2, 2, 3, 3, 4, 4))
  counts <- as.integer(c(11, 21, 33, 67, 98, 123, 555, 2))
  cast <- unique(grid)
  aggregate_vc(grid, counts, cast)
})
