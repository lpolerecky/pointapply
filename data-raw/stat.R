# load "real" data
load_point("map_sum_grid", c("MEX", "MON"), grid_cell = 64, return_name = FALSE,
           on_build = TRUE)

# single ion statistics
stat_X_64_all <- purrr::map_dfr(
  purrr::flatten(list(map_sum_grid_64_MEX, map_sum_grid_64_MON)),
  ~stat_X(.x, file.nm, sample.nm, dim_name.nm, grid.nm)
)
usethis::use_data(stat_X_64_all, overwrite = TRUE)

# single ion statistics
stat_R_64_all <- purrr::map_dfr(
  purrr::flatten(list(map_sum_grid_64_MEX, map_sum_grid_64_MON)),
  ~stat_R(.x, "13C", "12C", file.nm, sample.nm, dim_name.nm, grid.nm)
)
usethis::use_data(stat_R_64_all, overwrite = TRUE)
