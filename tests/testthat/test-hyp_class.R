#-------------------------------------------------------------------------------
# Change over time (golden test)
#-------------------------------------------------------------------------------

test_that("plot hyptohesis test over grid sizes", {
  skip_if_not(exists("loaded", "package:pointapply"), "Skip test if not in development mode.")
  # grid-cell sizes
  grid_cell <- sapply(2:7, function(x) 2 ^ x)
  # names of the analytes in the paper
  name <- c("MEX", "MON")
  # load
  name <- load_point("map_sum_grid", name, grid_cell, return_name = TRUE)
  # data frame aggregated over depth
  tb <- purrr::map_dfr(rlang::syms(name), ~purrr::pluck(eval(.x), "depth"))
  # QQ diagnostics
  tb_QQ <- point::diag_R(tb, "13C", "12C", file.nm, sample.nm, grid_size.nm,
                         grid.nm, .method = "QQ", .hyp = "norm",
                         .output = "diagnostics")
  # execute
  QQ_class <- hyp_class(tb_QQ, vc_N_13C, "Normality test") # barplot
  # snapshot
  expect_true(ggplot2::is.ggplot(QQ_class))
  # snapshot plot
  vdiffr::expect_doppelganger("barplot QQ over gridsize", QQ_class)
})
