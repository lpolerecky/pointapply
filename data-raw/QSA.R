# load simulated data
load_point("simu", "CD_eval_intra", NULL, return_name = FALSE, on_build)

tb <- dplyr::bind_rows(map_sum_grid_64_MEX$depth, map_sum_grid_64_MON$depth)
# QSA test
test_QSA_64_all <- point::QSA_test(tb, "13C", "12C", file.nm, sample.nm,
                                   grid.nm, .nest = grid.nm)
# make mlm model line
test_QSA_64_all <- dplyr::mutate(
  test_QSA_64_all,
  hat_R = alpha_grid.nm + beta_grid.nm * Xt.pr.12C
)

usethis::use_data(test_QSA_64_all, overwrite = TRUE)
