#-------------------------------------------------------------------------------
# Change over time (golden test)
#-------------------------------------------------------------------------------

test_that("multiplication works", {
  skip_if_not(exists("loaded", "package:pointapply"), "Skip test if not in development mode.")
  # load ion maps
  load_point("map_raster_image", "MEX", return_name = FALSE)
  # grid data
  load_point("map_sum_grid", "MEX", 64, return_name = FALSE)
  # diagnostics
  tb_dia <- point::diag_R(
      map_sum_grid_64_MEX,
      "13C",
      "12C",
      dim_name.nm,
      sample.nm,
      file.nm,
      grid.nm,
      .nest = grid.nm,
      .output = "complete",
      .meta = TRUE
  )
  # execute
  diag_64_MON <- gg_effect(tb_dia, map_raster_image_MEX, "MON", "12C-40Ca16O",
                           viri = "B")
  # snapshot data
  expect_snapshot(diag_64_MON)
  # snapshot plot
  vdiffr::expect_doppelganger(
    "ggplot2 with gridded R and ion raster image",
    ggplot2::last_plot()
    )
})
