library(dplyr)
library(point)

# load "real" data
load_point("map_sum_grid", c("MEX", "MON"), grid_cell = 64)
# load "diagnostics" data
load_point("diag_64", paste(c("MEX", "MON"), "13C_12C", sep = "_"))

# combined
map_sum_grid_64_all <- bind_rows(map_sum_grid_64_MEX, map_sum_grid_64_MON)

# single ion statistics
stat_X_64_all <- stat_X(map_sum_grid_64_all, file.nm, sample.nm, dim_name.nm, grid.nm)

# ratios statistics
stat_R_64_all <- stat_R(map_sum_grid_64_all, "13C", "12C", file.nm, sample.nm, dim_name.nm, grid.nm)

# save
usethis::use_data(map_sum_grid_64_all, stat_X_64_all, stat_R_64_all, diag_64_MON_13C_12C,
                  diag_64_MEX_13C_12C, internal = TRUE, overwrite = TRUE)
# demo data
demo <- tibble(message = "Use `download_point` or check the vignettes to obtain the data associated with this package")
usethis::use_data(demo, overwrite = TRUE)
