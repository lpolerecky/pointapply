#' Read matlab ion count data
#'
#' \code{grid_aggregate} Aggregate ion count data cubes on a 2D grid.
#'
#' @param IC Ion Count data cube
#' @param plane Variable for the dimension over which to be aggregated (vectors
#'  are allowed).
#' @param title Character string for sample name.
#' @param species Character string or vector with chemical species.
#' @param grid_cell Integer indicating size of cells in pixels, only exponent of
#'  base two allowed (default = 64).
#' @param output Character string for type of output; \code{"complete"} or
#'  \code{"sum"}.
#' @param grid_sel Integer selecting an grid-cell for in-depth analysis.
#' @param scalar Numeric converting the pixels to metric dimension of
#'  measurement (default is the conversion used in this study).
#'
#' @return A \code{tibble::\link[tibble:tibble]{tibble}} containing the ion counts
#'  aggregated over the plane of choice, if \code{output = "sum"}. If
#'  \code{output = "complete"} only works in combination with \code{grid_sel} and
#'  produces a pixel-by-pixel dataset for that grid-cell.
#'
#' @export
grid_aggregate <- function(IC, plane, grid_cell = 64, species = NULL,
                           title = character(1), name = character(1),
                           output = "sum", corrected = FALSE,
                           scalar = 40 / 256, save = FALSE) {

  # get original mat file names
  nms <- vapply(IC, attr, character(1), "file")
  nms <- gsub("_cnt", "", nms)
  IC <- set_names(IC, nms)

  # reduce list to include only selected species
  if (!is.null(species)) {
    IC[nms %in% species]
  }

  # dimensions
  dim_names <- c("height", "width", "depth")
  all_dims <- purrr::map(IC, ~set_names(dim(.x), nm = dim_names))

  # species names
  if (is.null(species)) all_species <- nms else all_species <- species

  # execute aggregation
  args <- list(IC = IC, dims = all_dims, species = all_species)
  out <- purrr::pmap_dfr(args, flatten_cube, plane, grid_cell) |>
    dplyr::mutate(
      sample.nm = title,
      file.nm = name,
      grid_size.nm = (grid_cell * scalar) ^ 2, # grid surface
      .before = "grid.nm"
    )

  # make corrections, check point package for documentation of `cor_IC`
  if (isTRUE(corrected)) {
  out <- point::cor_IC(out, .bl_t = 0, .det = "EM")
  }

  # print or save
  if (isTRUE(save)) {
    object_name <- paste(name, grid_cell, title, sep = "_")
    write_point(out, object_name)
    message(
      glue::glue("Aggregated file has been saved with name {object_name}.")
    )
  } else {
    out
  }
}

#-------------------------------------------------------------------------------
# additional supporting functions
#-------------------------------------------------------------------------------
# vectorized for plane
flatten_cube <- function(IC, dims, plane, species, grid_cell, scaler) {
  purrr::map_dfr(
    plane,
    ~flatten_cube_(IC = IC, dims = dims, plane = .x, species, grid_cell, scaler)
  )
}

# cast the ion count data cube into a flat long formatted dataframe
flatten_cube_ <- function(IC, dims, plane, species, grid_cell, scaler) {

  # sub-sample
  x <- subsample(IC, dims, plane, grid_cell)

  # dimensions remaining after flattening
  dimr <- dims[names(dims) != plane]

  # calculate max grid cell per plane
  if (plane == "depth") {
    gc <- as.integer(prod(dimr / grid_cell))
  } else {
    gc <- as.integer(dims[[plane]] / grid_cell)
  }
  # pixels encompassed in single measurement
  px <- prod(dimr) / gc

  # grid numbering and naming
  if(grid_cell > 1) {
    grd <- rep(1:gc, dims[[plane]]) # grid numbering
    dm <- (as.integer(names(x)) - 1L) %/% gc + 1L # dimension numbering
  } else {
    grd <- 1:prod(dimr) # grid numbering (equals pixel numbering)
    dm <- 1
  }

  # cast in tibble
  tibble::tibble(
    grid.nm = grd, # grid number
    dim.nm = dm, # planes of measurement
    dim_name.nm = plane, # names of the planes of measurement
    species.nm = species, # ion species name
    N.rw = x, # raw ion counts
    t.nm = 1e-3 * px * .data$dim.nm, # time (1 pixel per millisecond)
    !!! grid_positions(dims, plane, grid_cell) # new dimension after reduction
  )
}

# sub-sample cube over grid of the same dimensions
subsample <- function(IC, dims, plane, grid_cell) {

  # sum of integers
  sumcounts <- function(x) sum(as.integer(x))

  # matrix based sub-sampling where the margins denote the dimension over which
  # the cube is flattened
  if (grid_cell == 1) {
    # dimensions for aggregation
    dim_vc <- list(height = c(2, 3), width = c(1, 3), depth = c(1, 2))
    # sub-sample
    mt <- apply(IC, dim_vc[[plane]], sumcounts)
    # remove dims and cast vector
    dim(mt) <- NULL
    mt
  # vectorized sub-sampling where the Kronecker produced grid number denotes
  # the sub-sample grid
  } else {
    # grid for aggregation
    grid <- kronecker_subsample_grid(dims, plane, grid_cell)
    # sub-sample
    tapply(IC, grid, sumcounts)
  }
}

# if plane is depth ROI dims should be 2^x
kronecker_subsample_grid <- function(dims, plane, grid_cell, expand = TRUE) {

  # grid cell size
  if (plane == "depth") {
    grid <- matrix(1, grid_cell, grid_cell)
  } else {
    grid <- matrix(1, grid_cell, dims[3])
  }
  # raster grid
  sub <- subsample_grid(dims = dims, grid_cell = grid_cell, plane = plane)

  if(isTRUE(expand)){
    # Kronecker multiplication
    mt <- kronecker(sub,  grid)
    # expand into the 3th dimension
    outer(mt, max(mt) * 0:(dims[[plane]] - 1), `+`)
  } else {
    sub
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

# in case of sub-sampling, new grid positions are calculated
grid_positions <- function(dims, plane, grid_cell) {

  # dimensions remaining after flattening
  dimr <- dims[names(dims) != plane]

  # make grid
  x <- purrr::imap(dimr, ~grid_positions_(.x, .y, grid_cell))
  gr <- tidyr::expand_grid(x[[2]], x[[1]])
  names(gr) <- paste0("mean_", names(dimr), ".mt") |> rev()

  # repeat
  if (grid_cell > 1) {
    gr[rep(seq_len(nrow(gr)), times = dims[[plane]]), ]
  } else {
    gr <- tidyr::expand_grid(1 : dimr[[2]], 1 : dimr[[1]])
    names(gr) <- paste0("mean_", names(dimr), ".mt") |> rev()
    gr
  }
}

grid_positions_ <- function(dim, dim_name, grid_cell) {
  if (dim_name != "depth") {
    seq(0, dim - grid_cell, grid_cell) + grid_cell / 2
  } else {
    dim / 2
  }
}
