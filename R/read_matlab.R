#' Read matlab ion count data
#'
#' \code{read_matlab} Tabulate  and aggregate the ion count data contained in
#' matlab files exported by LANS. Matlab files are read with
#' \code{R.matlab::\link[R.matlab:readMat]{readMat}}.
#'
#' @param directory Character string for path to the Matlab data-file
#' @param plane Variable for the dimension over which to be aggregated
#' @param title Character string for sample name.
#' @param species Character string or vector with chemical species.
#' @param grid_cell Integer indicating size of cells in pixels, only exponent of
#' base two allowed (default = 64)
#' @param output Character string for type of output; \code{"complete"} or
#' \code{"sum"}.
#' @param grid_sel Integer selecting an grid-cell for in-depth analysis.
#' @param scalar Numeric converting the pixels to metric dimension of
#' measurement (default is the conversion used in this study).
#'
#' @return A \code{tibble::\link[tibble:tibble]{tibble}} containing the ion counts
#' aggregated over the plane of choice, if \code{output = "sum"}. If
#' \code{output = "complete"} only works in combination with \code{grid_sel} and
#' produces a pixel-by-pixel dataset for that grid-cell.
#'
#' @export
read_matlab <- function(directory, plane, title, species = NULL, grid_cell = 64,
                        output = "sum", grid_sel = NULL, scalar = 40 / 256){

  plane <- enquo(plane)
  # search pattern
  search_pattern <- stringr::str_c(paste0(species, "_cnt.mat$"), collapse = "|")
  # list files
  ls_files <- list.files(directory, pattern = search_pattern, full.names = TRUE)

  # read count cube files
  all_files <- purrr::map(ls_files, ~readmat::read_mat(.x)) # purrr::pluck(R.matlab::readMat(.x), "cnt"))
  # dimensions
  dim_names <- c("height", "width", "depth")

  # data and metadata
  all_files <- purrr::map(all_files, ~dim_labeller(.x, dims = dim_names))
  dim_sizes <- purrr::map(all_files, ~set_names(dim(.x), nm = dim_names))
  all_species <- stringr::str_extract_all(
    ls_files,
    "((?<=/)[[:alnum:]]*)(?=_cnt)"
    )
  chop_files <- stringr::str_split(directory,"/")[[1]]
  file_names <- purrr::map(
    1 : dplyr::n_distinct(ls_files),
    ~ chop_files[dplyr::n_distinct(chop_files)]
    )

  # execute aggregation
  args <- list(matfile = all_files ,species = all_species,
               file_name = file_names, dim_size = dim_sizes)
  purrr::pmap_dfr(args, cube_to_ion_tibble, plane = plane, title = title,
                  grid_cell = grid_cell, output = output,
                  grid_sel = grid_sel, scalar = scalar,
                  dim_names = dim_names)
}

#-------------------------------------------------------------------------------
# additional supporting functions
#-------------------------------------------------------------------------------

# convert the matlab data cub to a long format dataframe
cube_to_ion_tibble <- function(matfile, species, file_name, dim_size, plane,
                               title, grid_cell, output, grid_sel, scalar,
                               dim_names) {

  if (grid_cell != 1) {
    # create a grid
    grid_layout <- grid_gen(dims = dim_size, plane = plane,
                            grid_cell = grid_cell, dim_names = dim_names)

    # flatten matlab-file and grid matrix and combine
    IC_2d <- purrr::map2(
      list(matfile, grid_layout),
      c("N.rw", "grid.nm"),
      ~flatten_matrix(.x, var =.y)
      ) %>%
      purrr::reduce(left_join, by = names(dimnames(grid_layout)))

    if (output == "sum") {
      # aggregate counts over dim fore each grid
      res <- accumulate_cnts(
        IC_2d = IC_2d,
        sum_plane = !! plane,
        title = title,
        species = species,
        file_name = file_name,
        grid = grid_cell,
        scalar = scalar,
        dim_names = dim_names
        )
      return(res)
      }

    if (output == "complete") {
      # complete counts from selected grid-cells
      res <- filter(IC_2d, grid.nm == grid_sel) %>%
        mutate(
          sample.nm = title,
          N.rw = as.double(N.rw),
          t.nm = 1e-3 * !! plane,
          species.nm = species,
          file.nm = file_name
          )
      return(res)
      }
  }

  if (grid_cell == 1) {
    # aggregate counts over dim fore each pixel i.e. raster image
    res <- image_cnts(
      IC_3d = matfile,
      plane = plane,
      title = title,
      species = species,
      file_name = file_name
      )

    if (as_name(plane) != "depth") {
      # make size of depth dimension comparable to the other dimensions
      dim_vars <- quos(height, width, .named =  TRUE)
      gr_var <- dim_vars[which(
        !(sapply(dim_vars, as_name) %in% as_name(plane))
        )]

      res <- mutate(
        arrange(res, !!! gr_var),
        depth = rep(
          as.numeric(cut(1:dim_size["depth"], 64)),
          dim_size[sapply(gr_var, as_name)]
          )
        ) %>%
        group_by(!!! gr_var, depth) %>%
        summarise(
          across(tidyselect::vars_select_helpers$where(is.numeric), sum),
          across(tidyselect::vars_select_helpers$where(is.character), unique),
          .groups = "drop"
          )
      return(res)
    }
    return(res)
  }
}

# image counts
image_cnts <- function(IC_3d, plane, title, species, file_name){

  plane <- as_name(plane)
  # dim for aggregation
  dim_vc <- list(height = c(2, 3), width = c(1, 3), depth = c(1, 2))

  exec("apply", IC_3d, c(unname(unlist(dim_vc[plane]))), sum) %>%
    flatten_matrix(var = "N.rw") %>%
    mutate(
      grid.nm = row_number(),
      t.nm = 1e-3 * dim(IC_3d)[which(plane == names(dimnames(IC_3d)))],
      species.nm = species,
      file.nm = file_name,
      sample.nm = title,
      dim_name.nm = plane
      )

}

# summarise count statistics over plane for each grid-specifically
accumulate_cnts <- function(IC_2d, sum_plane, title, species, file_name, grid,
                            scalar, dim_names){

  sum_plane <- enquo(sum_plane)
  dim_vars <- parse_exprs(dim_names) %>% set_names()
  gr_var <- dim_vars[which(sapply(dim_vars, as_name) %in% as_name(sum_plane))]
  stat_var <- dim_vars[which(
    !(sapply(dim_vars, as_name) %in% as_name(sum_plane))
    )]

  IC_2d <-group_by(IC_2d, !!! gr_var, grid.nm) %>%
    summarise(
      dim_name.nm = as_name(sum_plane),
      grid_size.nm = (grid * scalar) ^ 2,
      across(c(!!! stat_var), mean, .names = "mean_{.col}.mt"),
      N.rw = as.numeric(sum(N.rw)),
      t.nm = n() * 1e-3,
      .groups = "keep"
      ) %>%
    mutate(
      t.nm = t.nm * {{ sum_plane }},
      species.nm = species,
      file.nm = file_name,
      sample.nm = title,
      ) %>%
    ungroup() %>%
    rename(dim.nm = !! sum_plane)
  # fold metadata
  point::fold(IC_2d, ".mt")
}

# sub-sample cube over grid of the same dimensions
kronecker_subsample <- function(original) {

  # vectorized sub-sampling where the kronecker produced grid number detonates
  # the sub-sample
  rng <- range(xc) # range over which to sub-sample
  # original[xc ==]

}

# if plane is depth ROI dims should be 2^x
kronecker_subsample_grid <- function(dims, plane, grid_cell, expand = TRUE) {

  plane <- rlang::as_name(plane)

  # ROI size
  if (plane == "depth") {
    grid <- matrix(1, grid_cell, grid_cell)
  } else {
    grid <- matrix(1, grid_cell, dims[3])
  }
  # raster grid
  sub <- subsample_grid(dims = dims, grid_cell = grid_cell, plane = plane)

  if(isTRUE(expand)){
    mt <- kronecker(sub,  grid)
    # expand into the 3th dimension by addition
    cast <- outer(mt, max(xc) * 0:399, FUN = "+") # cast
    mold <- outer(xc, max(xc) * 0:399, FUN = "+") # mold

    purrr::modify(mold, ~as.integer(sum(original[cast == .x])))
    # return expanded
    #dim_labeller(rs, plane = plane, dims = dim_names)
  } else {
    # return (as an example of the layout)
    #dim_labeller(rs, plane = plane, dims = dim_names)
  }
}

# create matrix based on original dims
subsample_grid <- function(dims, grid_cell, plane = "depth") {
  # dependent on the dimensions of aggregation, different dimension matrices
  # are being produced
  if (plane == "depth") {
    matrix(
      1:((dims[1] * dims[2]) / (grid_cell * grid_cell)),
      nrow = dims[1] / grid_cell
    )
  } else if (plane == "height") {
    matrix(1:(dims[1] / grid_cell))
  } else if (plane == "width") {
    matrix(1:(dims[2] / grid_cell))
  } else {
    stop("Dimension uknown.", call. = FALSE)
  }
}

# label array dims
dim_labeller <- function(rs, plane = NULL, dims) {
  # dimensions
  if(!is.null(plane)) dims <- dims[!dims %in% plane]
  dimnames(rs) <- purrr::map2(dims, dim(rs), ~paste(.x, 1:.y, sep = "_")) %>%
    set_names(nm = dims)
  return(rs)
}

# reduce cube to tibble
flatten_matrix <- function(cube, var) {
  as_tibble(cubelyr::as.tbl_cube(cube, met_name = var)) %>%
    mutate(
      across(
        !ends_with(".rw"),
        ~{as.numeric(stringr::str_extract(., pattern = "[0-9]+"))}
        )
     )
}
