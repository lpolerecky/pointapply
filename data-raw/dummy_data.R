## dummy datasets

# 4, 8, 16, 32, 64, and 128 px grid-cell size  for aggregating data
vc_px <- sapply(2:7, function(x) 2 ^ x)

dummy_data <- function(grid_cell, title){
  name <- paste("map_sum_grid", grid_cell, title, sep = "_")
  grid_cell <- "Dummy dataset. Use download_point to download data from my Zenodo repo."
  assign(name, grid_cell)
  do.call("use_data", list(as.name(name), overwrite = TRUE, compress = "xz"))
}

purrr::walk(vc_px, ~dummy_data(.x, "MEX"))
purrr::walk(vc_px, ~dummy_data(.x, "MON"))
