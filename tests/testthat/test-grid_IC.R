#-------------------------------------------------------------------------------
# Change over time (golden test)
#-------------------------------------------------------------------------------

test_that("multiplication works", {
  skip_if_not(exists("loaded", "package:pointapply"), "Skip test if not in development mode.")


  # execute
  diag_64_MEX <- gg_effect("MEX", "12C-40Ca16O", viri = "B")
  # snapshot data
  expect_snapshot(diag_64_MON)
  # snapshot plot
  vdiffr::expect_doppelganger(
    "ggplot2 with gridded R and ion raster image",
    ggplot2::last_plot()
    )
})
