test_that("sensitivity tests works", {

  skip_if_not(
    exists("loaded", "package:pointapply"),
    "Skip test if not in development mode."
  )
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  # varying linear trends in the ionization efficiency (percent common isotope)
  var_T <- seq(0, 120, length.out = 12)
  # varying isotope offset (delta per mille)
  var_I <- seq(0, -22, length.out = 12)

  expect_snapshot(
    test_sensitivity(var_T, var_I, reps = 10, mc_cores = 4)
  )
})
