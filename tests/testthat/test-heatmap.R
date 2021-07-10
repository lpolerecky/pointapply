#-------------------------------------------------------------------------------
# Change over time (golden test)
#-------------------------------------------------------------------------------

test_that("heatmap for accuracy of intra-analysis isotope test", {
  skip_if_not(exists("loaded", "package:pointapply"), "Skip test if not in development mode.")
  # load simulated data (intra-isotope variability)
  load_point("simu", "sens_IC_intra", NULL, return_name = FALSE)
  # load test statistics (intra-isotope variability)
  load_point("simu", "CD_eval_intra", NULL, return_name = FALSE)
  load_point("simu", "CM_eval_intra", NULL, return_name = FALSE)
  # bind the two diagnostic treatments of simulated ion (Cameca and Cooks D)
  tb_eval  <- list(simu_CM_eval_intra, simu_CD_eval_intra) %>%
    purrr::set_names(nm = c("Cameca", "Cook's D")) %>%
    dplyr::bind_rows(.id = "stat.nm")

  # calculate accuracy of model classification
  tb_intra <-  dplyr::group_by(tb_eval, stat.nm, type.nm, trend.nm, force.nm) %>%
    dplyr::summarise(
      accuracy = acc_fun(unique(force.nm), p_R_Xt.sm, ntot = dplyr::n()),
      .groups = "drop"
      )
  # statistics on the single common isotope
  tb_X <- point::stat_X(
    simu_sens_IC_intra,
    trend.nm,
    .N = N.sm,
    .X = Xt.sm,
    .stat = c("RS", "hat_RS")
  ) %>%
    dplyr::filter(species.nm == "12C")

  # named vector
  vc_RS <- purrr::set_names(
    tb_X$trend.nm,
    nm = sprintf("%.0f", tb_X$RS_Xt.sm - tb_X$hat_RS_N.sm)
  )

  # rename methods, make nice labels
  tb_intra <- dplyr::mutate(
    tb_intra,
    stat.nm =
      factor(
        stat.nm,
        levels = c("Cameca", "Cook's D"),
        labels = c(sigma[R]~"rejection", "Cook~s~D")
      )
  )
  p <- heat_map(
    tb_intra,
    x = force.nm,
    y = trend.nm,
    stat = accuracy,
    grp1 = stat.nm,
    grp2 = type.nm,
    conversion = vc_RS,
    ttl = "Intra-isotope variability",
    x_lab = expression(
      "isotope variation in substrate "*Delta[B - A] *" [\u2030] ("*f[B] == 1/6*")"
    ),
    y_lab = "excess ionization [%]",
    x_sec = expression(
      "fractional size of inclusion "*f[B]~"("*Delta[B - A] == -20 ~ "[\u2030])"
    )
  )
  # snapshot
  expect_true(ggplot2::is.ggplot(p))
  # snapshot plot
  vdiffr::expect_doppelganger("ggplot2 heatmap intra-analysis isotope accuracy", p)
})
