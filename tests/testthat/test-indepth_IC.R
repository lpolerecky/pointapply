#-------------------------------------------------------------------------------
# Change over time (golden test)
#-------------------------------------------------------------------------------

test_that("plot in depth analysis", {
  skip_if_not(
    exists("loaded", "package:pointapply"),
    "Skip test if not in development mode."
  )
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  p <- gg_point(
    title = "MEX",
    grid_cell = 2,
    # ion ratio for filtering
    ion1_thr = "12C14N",
    ion2_thr = "12C",
    thr = 0.75,
    # isotope ratio
    ion1_R = "13C",
    ion2_R = "12C"
  )
  # snapshot
  expect_true(ggplot2::is.ggplot(p))
  # snapshot plot
  vdiffr::expect_doppelganger("ggplot2 with in depth count analysis", p)
})
