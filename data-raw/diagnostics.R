# names of the analytes in the paper
name <- c("MEX", "MON")
# load grid aggregated counts
load_point("map_sum_grid", name, 64, return_name = FALSE, on_build = TRUE)

# combined raster data for 64 pixel size
map_sum_grid_64_all <- dplyr::bind_rows(
  dplyr::bind_rows(map_sum_grid_64_MEX),
  dplyr::bind_rows(map_sum_grid_64_MON)
)
usethis::use_data(map_sum_grid_64_all, overwrite = TRUE)

# diagnostics MON
ls_diag <- purrr::map(
  map_sum_grid_64_MON,
  ~point::diag_R(.x, "13C", "12C", dim_name.nm, sample.nm, file.nm, grid.nm,
                 .nest = grid.nm, .output = "complete")
  )

diag_64_MON <- gg_effect(ls_diag, map_raster_image_MON, "MON", "12C-40Ca16O",
                         viri = "B")

usethis::use_data(diag_64_MON, overwrite = TRUE)

# diagnostics MEX
ls_diag <- purrr::map(
  map_sum_grid_64_MEX,
  ~point::diag_R(.x, "13C", "12C", dim_name.nm, sample.nm, file.nm, grid.nm,
                 .nest = grid.nm, .output = "complete")
  )

diag_64_MEX  <- gg_effect(ls_diag, map_raster_image_MEX, "MEX", "12C-40Ca16O",
                          viri = "B")

usethis::use_data(diag_64_MEX, overwrite = TRUE)
