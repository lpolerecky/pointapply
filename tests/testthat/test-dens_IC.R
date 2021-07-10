test_that("consistency of plotting", {
  skip_if_not(exists("loaded", "package:pointapply"), "Skip test if not in development mode.")
  # grid-cell sizes
  grid_cell <- sapply(2:7, function(x) 2 ^ x)
  # names of the analytes in the paper
  name <- c("MEX", "MON")
  # load
  name <- load_point("map_sum_grid", name, grid_cell, return_name = TRUE)
  # data frame aggregated over depth
  tb <- purrr::map_dfr(rlang::syms(name), ~purrr::pluck(eval(.x), "depth"))

  # calc QQ
  tb_QQ <- point::diag_R(tb, "13C", "12C", file.nm, sample.nm, grid_size.nm,
                         grid.nm, .method = "QQ", .hyp = "norm",
                         .output = "diagnostics")
  # plot QQ
  QQ_dens <- gg_dens(
    tb_QQ,
    TQ,
    RQ,
    "Theoretical quantiles",
    "Sample quantiles",
    "Normal QQ plot",
    grid_size.nm,
    1,
    c(-4, 4),
    c(-4, 4)
  )
  # ggplot plotting
  expect_true(ggplot2::is.ggplot(QQ_dens))
  # snapshot
  vdiffr::expect_doppelganger("ggplot2 with two-D density for QQ", QQ_dens)
})
