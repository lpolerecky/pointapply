test_that("gg_effect is consistent", {

  skip_if_not(
    exists("loaded", "package:pointapply"),
    "Skip test if not in development mode."
  )
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  # execute
  xc <- gg_effect("MEX", "12C-40Ca16O", viri = "B")
  # snapshot data
  expect_snapshot(xc)
  # snapshot plot
  vdiffr::expect_doppelganger(
    "ggplot2 with gridded R and ion raster image",
    ggplot2::last_plot()
    )

  # execute
  xc <- gg_effect("MON", "12C-40Ca16O", viri = "B")
  # snapshot data
  expect_snapshot(xc)
  # snapshot plot
  vdiffr::expect_doppelganger(
    "ggplot2 with gridded R and ion raster image",
    ggplot2::last_plot()
  )
})

test_that("gg_sketch is consistent", {

  skip_if_not(
    exists("loaded", "package:pointapply"),
    "Skip test if not in development mode."
  )
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  # snapshot plot
  vdiffr::expect_doppelganger(
    "ggplot2 sketch of grid numbering",
    gg_sketch(32)
  )
})

test_that("3D configuration can be converted to 2D configuration", {

  skip_if_not(
    exists("loaded", "package:pointapply"),
    "Skip test if not in development mode."
  )
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  load_point("map_raster_image", "MEX")
  load_point("map_sum_grid", "MEX", 64)

  im <- point::unfold(map_raster_image_MEX)
  expect_snapshot(
    dim_folds(im, "raster", 256, 64)
  )

  IC <- point::unfold(map_sum_grid_64_MEX)
  expect_equal(
    dim_folds(IC, "grid", 256, 64) |>
      dplyr::filter(dim_name.nm == "depth") |>
      dplyr::pull(width.mt) |>
      sum(),
      1638400
  )

  expect_snapshot(
    dim_folds(IC, "grid", 256, 64)
  )
})

test_that("diagnostics preserve metadata", {

  skip_if_not(
    exists("loaded", "package:pointapply"),
    "Skip test if not in development mode."
  )
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  load_point("map_sum_grid", "MEX", 64)
  xc <- point::diag_R(map_sum_grid_64_MEX, "13C", "12C", dim_name.nm,
                      sample.nm, file.nm, grid.nm, .nest = grid.nm,
                      .output = "complete", .meta = TRUE)

  expect_snapshot(
    point::unfold(xc, merge = FALSE)
  )
})
