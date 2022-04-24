#-------------------------------------------------------------------------------
# Change over time (golden test)
#-------------------------------------------------------------------------------

test_that("plot hyptohesis test over grid sizes", {

  skip_if_not(
    exists("loaded", "package:pointapply"),
    "Skip test if not in development mode."
  )
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  # grid-cell sizes
  grid_cell <- sapply(2:7, function(x) 2 ^ x)
  # names of the analytes in the paper
  name <- c("MEX", "MON")
  # load
  name <- load_point("map_sum_grid", name, grid_cell, return_name = TRUE)
  # data frame aggregated over depth
  tb <- purrr::map_dfr(
    rlang::syms(name),
    ~dplyr::filter(eval(.x), .data$dim_name.nm == "depth")
  )

  vc_N_13C <- dplyr::group_by(tb, grid_size.nm, species.nm) %>%
    dplyr::summarise(N = mean(N.pr), .groups = "drop") %>%
    dplyr::filter(species.nm == "13C")
  vc_N_13C <- rlang::set_names(
    sprintf("%.0f", vc_N_13C$N),
    nm = vc_N_13C$grid_size.nm
  )

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
