#-------------------------------------------------------------------------------
# Change over time (golden test)
#-------------------------------------------------------------------------------

test_that("plot in depth analysis", {
  skip_if_not(exists("loaded", "package:pointapply"), "Skip test if not in development mode.")
  # load ion maps
  load_point("map_raster_image", "MEX", NULL, return_name = FALSE)
  # full grid-cell datasets for in-depth analysis
  load_point("map_full_grid", "MEX", 64, return_name = FALSE)
  # execute
  p <- gg_point(
    IC = map_full_grid_64_MEX$map_full_grid_64_2_MEX, # full dataset
    image = map_raster_image_MEX$depth, # raster image
    ion1_thr = "12C", ion2_thr = "40Ca16O", thr = 20, # ion ratio for filtering
    ion1_R = "13C", ion2_R = "12C" # isotope ratio
  )
  # snapshot
  expect_true(ggplot2::is.ggplot(p))
  # snapshot plot
  vdiffr::expect_doppelganger("ggplot2 with in depth count analysis", p)
})
