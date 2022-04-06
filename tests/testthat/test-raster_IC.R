test_that("raster ion image plotting is consistent", {

  skip_if_not(
    exists("loaded", "package:pointapply"),
    "Skip test if not in development mode."
  )
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  vdiffr::expect_doppelganger(
    "raster image MEX",
    gg_cnts("MEX", "12C14N", "12C", viri = "D")
  )
  vdiffr::expect_doppelganger(
    "raster image MEX with all sides included",
    gg_cnts("MEX", "12C14N", "12C", viri = "D", compilation = TRUE)
  )
})

test_that("height and depth dimensions can be aggregated", {

  skip_if_not(
    exists("loaded", "package:pointapply"),
    "Skip test if not in development mode."
  )
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  load_point("map_raster_image", "MEX")

  IC <- point::unfold(map_raster_image_MEX)

  expect_snapshot(
    dim_aggregate_(IC, 64, "height")
  )

  expect_snapshot(
    dim_aggregate(IC, 64)
  )
})
