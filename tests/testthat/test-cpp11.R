test_that("cpp11 tapply works", {
  grid <- as.double(c(1, 1, 2, 2, 3, 3, 4, 4))
  counts <- as.double(c(11, 21, 33, 67, 98, 123, 555, 2))
  expect_snapshot(
    mytapply_(grid, counts)
  )
})
