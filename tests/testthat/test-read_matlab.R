test_that("reading the matlab ion count cubes works", {
  IC <- read_matlab(get_matlab("2020-08-20-GLENDON"), height, "MEX",
                    c("12C", "13C"), grid_cell = 64)
  expect_snapshot(head(IC, 35)) # head
  expect_snapshot(tail(IC, 35)) # tail
  expect_equal(nrow(IC), 2048) # number of rows
  expect_equal(ncol(IC), 9) # number of variables
})

test_that("flattening ot the cube works", {
  dirmat <- get_matlab("2020-08-20-GLENDON")
  grid_cell <- 64
  output <- "sum"
  grid_sel <- NULL
  scalar = 40 / 256
  # search pattern
  search_pattern <- stringr::str_c(paste0("12C", "_cnt.mat$"), collapse = "|")
  # list files
  ls_files <- list.files(dirmat, pattern = search_pattern, full.names = TRUE)

  # read count cube files
  all_files <- purrr::map(ls_files, ~readmat::read_mat(.x))
  # dimensions
  dim_names <- c("height", "width", "depth")

  # data and metadata
  all_files <- purrr::map(all_files, ~dim_labeller(.x, dims = dim_names))
  dim_sizes <- purrr::map(all_files, ~set_names(dim(.x), nm = dim_names))
  all_species <- stringr::str_extract_all(
    ls_files,
    "((?<=/)[[:alnum:]]*)(?=_cnt)"
  )
  chop_files <- stringr::str_split(dirmat,"/")[[1]]
  file_names <- purrr::map(
    1 : dplyr::n_distinct(ls_files),
    ~ chop_files[dplyr::n_distinct(chop_files)]
  )

  # execute aggregation
  args <- list(matfile = all_files[[1]] ,species = all_species[[1]],
               file_name = file_names[[1]], dim_size = dim_sizes[[1]],
               plane = quo(height), title = "MEX",
               grid_cell = grid_cell, output = output,
               grid_sel = grid_sel, scalar = scalar,
               dim_names = dim_names)

  xc <- do.call(cube_to_ion_tibble, args)
  expect_snapshot(head(xc, 35)) # head
  expect_snapshot(tail(xc, 35)) # tail
})

test_that("an expanded grid can be made the Kronecker product", {
  xc <- grid_gen(c(256, 256, 400), quo(depth), 64, c("height", "width", "depth"))
  expect_snapshot(xc)
})
