#-------------------------------------------------------------------------------
# Change over time (golden test)
#-------------------------------------------------------------------------------

test_that("multiplication works", {
  skip_if_not(exists("loaded", "package:pointapply"), "Skip test if not in development mode.")
  # load ion maps
  load_point("map_raster_image", "MON", NULL, return_name = FALSE)
  # gridded data
  load_point("map_sum_grid", "MON", 64, return_name = FALSE)
  # diagnostics
  ls_diag <- purrr::map(
    map_sum_grid_64_MON,
    ~point::diag_R(
      .x,
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
  )
  # execute
  diag_64_MON <- gg_effect(ls_diag, map_raster_image_MON, "MON", "12C-40Ca16O",
                           viri = "B")
  # snapshot data
  expect_snapshot(diag_64_MON)
  # snapshot plot
  vdiffr::expect_doppelganger(
    "ggplot2 with gridded R and ion raster image",
    ggplot2::last_plot()
    )
})
