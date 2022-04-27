test_that("reading the matlab ion count cubes works", {

  skip_if_not(
    exists("loaded", "package:pointapply"),
    "Skip test if not in development mode."
  )
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  # search pattern
  search_pattern <- paste0(paste0(c("12C", "13C"), "_cnt.mat$"), collapse = "|")
  # list files
  MEX_files <- list.files(
    get_matlab("2020-08-20-GLENDON"),
    pattern = search_pattern,
    full.names = TRUE
  )
  # read files (alternatively use R.matlab)
  MEX <- lapply(MEX_files, readmat::read_mat)
  # get measured species names
  MEX_species <- vapply(MEX, attr, character(1), "file")
  MEX_species <- gsub("_cnt", "", MEX_species)
  # set names
  MEX <- unlist(MEX, recursive = FALSE) |>  stats::setNames(MEX_species)

  # aggregation (inner function)
  IC <- flatten_cube_(
    unlist(MEX[[1]], recursive = FALSE),
    c(height = 256, width = 256, depth = 400),
    "depth",
    "12C",
    grid_cell = 64
  )

  expect_snapshot(head(IC, 35)) # head
  expect_snapshot(tail(IC, 35)) # tail
  expect_equal(nrow(IC), 6400) # number of rows
  expect_equal(ncol(IC), 8) # number of variables

  # aggregation (outer function)
  IC <- grid_aggregate(MEX, c("width", "height"), grid_cell = 64)

  expect_snapshot(head(IC, 35)) # head
  expect_snapshot(tail(IC, 35)) # tail
  expect_equal(nrow(IC), 4096) # number of rows
  expect_equal(ncol(IC), 12) # number of variables

  # selection (inner function)
  IC <- extract_cube_(
    MEX[[1]],
    c(height = 256, width = 256, depth = 400),
    "height",
    "12C",
    grid_cell = 64,
    select_cell = 2
  )

  expect_snapshot(head(IC, 35)) # head
  expect_snapshot(tail(IC, 35)) # tail
  expect_equal(nrow(IC), 6553600) # number of rows
  expect_equal(ncol(IC), 9) # number of variables

  # selection (outer function)
  IC <- grid_select(MEX, c("width", "height"), grid_cell = 64,
                    select_cell = c(2, 4))

  expect_snapshot(head(IC, 35)) # head
  expect_snapshot(tail(IC, 35)) # tail
  expect_equal(nrow(IC), 52428800) # number of rows
  expect_equal(ncol(IC), 12) # number of variables

  # some errors
  expect_error(
    grid_aggregate(MEX, "width", save = TRUE),
    "Provide a title for the file to be saved."
  )
  dims <- c("height" = 256, "width" = 256, "depth" = 400)
  expect_error(
    check_select_cell(17, "depth", dims, 64),
    NULL
  )
  expect_error(
    grid_select(MEX, c("width", "height"), grid_cell = 64,
                select_cell = 5),
    NULL
  )
  expect_warning(
    grid_aggregate(MEX, "height", grid_cell = 64,
                   select_cell = 5),
    NULL
  )
})

test_that("metadata is retained", {

  skip_if_not(
    exists("loaded", "package:pointapply"),
    "Skip test if not in development mode."
  )
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  # search pattern
  search_pattern <- paste0(paste0(c("12C", "13C"), "_cnt.mat$"), collapse = "|")
  # list files
  MEX_files <- list.files(
    get_matlab("2020-08-20-GLENDON"),
    pattern = search_pattern,
    full.names = TRUE
  )
  # read files (alternatively use R.matlab)
  MEX <- lapply(MEX_files, readmat::read_mat)
  # get measured species names
  MEX_species <- vapply(MEX, attr, character(1), "file")
  MEX_species <- gsub("_cnt", "", MEX_species)
  # set names
  MEX <- unlist(MEX, recursive = FALSE) |>  stats::setNames(MEX_species)


  IC <- grid_aggregate(MEX, c("height", "depth", "width"), grid_cell = 64,
                       corrected = TRUE)

  expect_snapshot(
    point::unfold(IC)
  )
  # check that metadata is not duplicated
  expect_equal(
    nrow(point::unfold(IC)),
    16896
  )
})

test_that("an expanded grid can be made with the Kronecker product", {
  # for aggregation (depth)
  xc <- kronecker_subsample_grid(
    c("height" = 256, "width" = 256, "depth" = 400),
    "depth",
    64
  )
  expect_equal(dim(xc), c(256, 256, 400))
  expect_snapshot(xc)
  # for selection (depth)
  xc <- kronecker_subsample_grid(
    c("height" = 256, "width" = 256, "depth" = 400),
    "depth",
    64,
    output = "select"
  )
  expect_equal(dim(xc), c(256, 256, 400))
  expect_snapshot(xc)
  # for selection (height)
  xc <- kronecker_subsample_grid(
    c("height" = 256, "width" = 256, "depth" = 400),
    "height",
    64,
    output = "select"
  )
  expect_equal(dim(xc), c(256, 400, 256))
  expect_snapshot(xc)
})

test_that("new dimenions can be generated after subsampling", {
  # for grid_cell size = 64
  xc <- grid_positions(
    dims = c(height = 256, width = 256, depth = 400),
    plane = "depth",
    grid_cell = 64
  )
  expect_snapshot(xc)
  # for grid_cell size = 0 (depth)
  xc <- grid_positions(
    dims = c(height = 64, width = 64, depth = 400),
    plane = "depth",
    grid_cell = 1
  )
  expect_snapshot(xc)
  # for grid_cell size = 0 (height)
  xc <- grid_positions(
    dims = c(height = 64, width = 256, depth = 400),
    plane = "height",
    grid_cell = 1
  )
  expect_snapshot(xc)
})
