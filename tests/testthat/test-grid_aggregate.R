test_that("reading the matlab ion count cubes works", {
  # search pattern
  search_pattern <- paste0(paste0(c("12C", "13C"), "_cnt.mat$"), collapse = "|")
  # list files
  ls_files <- list.files(
    get_matlab("2020-08-20-GLENDON"),
    pattern = search_pattern,
    full.names = TRUE
  )
  # read ion count mat files
  all_files <- purrr::map(ls_files, ~readmat::read_mat(.x))

  IC <- grid_aggregate(all_files, "height", grid_cell = 64)

  expect_snapshot(head(IC, 35)) # head
  expect_snapshot(tail(IC, 35)) # tail
  expect_equal(nrow(IC), 2048) # number of rows
  expect_equal(ncol(IC), 9) # number of variables
})

test_that("metadata is retained", {

  # search pattern
  search_pattern <- paste0(paste0(c("13C", "12C"), "_cnt.mat$"), collapse = "|")
  # list files
  ls_files <- list.files(
    get_matlab("2020-08-20-GLENDON"),
    pattern = search_pattern,
    full.names = TRUE
  )
  # read ion count mat files
  all_files <- purrr::map(ls_files, ~readmat::read_mat(.x))

  IC <- grid_aggregate(all_files, c("height", "depth", "width"), grid_cell = 64)

  expect_snapshot(
    point::unfold(IC)
  )
  # check that metadata is not duplicated
  expect_equal(
    nrow(point::unfold(IC)),
    33792
  )
})

test_that("an expanded grid can be made the Kronecker product", {
  xc <- grid_gen(c(256, 256, 400), quo(depth), 64, c("height", "width", "depth"))
  expect_snapshot(xc)
})
