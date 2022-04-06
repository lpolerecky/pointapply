#' Manipulate ion count data cubes
#'
#' \code{grid_aggregate} Aggregate ion count data cubes on a 2D grid.
#' \code{grid_select} Select ion count data cubes from a grid.
#' \code{tune_grid_cell} Allows calibration of the grid_cell size.
#'
#'
#' These functions are designed to work with data cubes as produced by the
#' Matlab based program Look At NanoSims (LANS). They allow sub-setting as well
#' as flattening of the cube. This is required for calculating high precision
#' isotope estimates.
#'
#' @param IC list of Ion Count (IC) data cubes.
#' @param plane Character string for the dimension over which to be aggregated
#'  (vectors are allowed).
#' @param grid_cell Integer indicating size of cells in pixels, only exponent of
#'  base two allowed (default = NULL). If NULL then 2D raster images are
#'  created.
#' @param select_cell Integer indicating a grid cell to be extracted.
#' @param species Character string or vector with chemical species. Default is
#'  NULL and is extracted from the IC data cubes. When supplied it will be used
#'  to filter the list of IC data cubes.
#' @param title Character string for file name.
#' @param name Character string for sample name.
#' @param corrected If TRUE then IC is corrected with
#'  \code{point::\link[point:cor_IC]{cor_IC}()}.
#' @param scalar Numeric converting the pixels to metric dimension of
#'  measurement (default is the conversion used in this study).
#' @param save Boolean indicating whether to save the file in the package
#'  internal data store (default = FALSE).
#' @param grid_expr The function `grid_select` or `grid_aggregate` to be tuned.
#' @param tune A numeric vector for grid_cell size tuning (following: 2 ^ x).
#' @param mc.cores An integer value for multi-threading.
#'
#' @return A \code{tibble::\link[tibble:tibble]{tibble}} containing the ion
#'  counts aggregated or extracted over the plane of choice.
#'
#' @export
grid_aggregate <- function(IC, plane, grid_cell = NULL, select_cell = NULL,
                           species = NULL, title = character(1),
                           name = character(1), corrected = FALSE,
                           scalar = 40 / 256, save = FALSE) {

  # check for identifiers if saving directly
  if (isTRUE(save) & title == character(1)) {
    stop("Provide a title for the file to be saved.", call. = FALSE)
  }

  # get original mat file names
  nms <- vapply(IC, attr, character(1), "file")
  nms <- gsub("_cnt", "", nms)
  IC <- set_names(IC, nms)

  # dimensions
  dim_names <- c("height", "width", "depth")
  all_dims <- purrr::map(IC, ~set_names(dim(.x), nm = dim_names))

  # reduce list to include only selected species
  if (!is.null(species)) {
    IC <- IC[nms %in% species]
    all_dims <- all_dims[nms %in% species]
  }

  # species names
  if (is.null(species)) all_species <- nms else all_species <- species

  # grid surface
  gridd <- ifelse(is.null(grid_cell), scalar ^ 2, grid_cell * scalar  ^ 2)

  # function name of inner function
  fun_nm <- rlang::as_name(match.call()[[1]])

  # throw warning when using `select_cell` with `grid_aggregate`
  if (!is.null(select_cell) & fun_nm == "grid_aggregate") {
    warning(paste("The argument `select_cell` is ignored for the function",
    "`grid_aggregate()`"))
  }

  # in case of `grid_select` check whether grid cell exists
  if (!is.null(select_cell) & fun_nm == "grid_select") {
    purrr::walk2(
      plane,
      all_dims,
      ~check_select_cell(select_cell, .x, .y, grid_cell)
    )
  }

  # select the appropriate inner function for either aggregation of selection
  if (fun_nm == "grid_aggregate") {
    fun_nm <- "flatten_cube" ; msg = "Aggregated"
  } else {
    fun_nm <- "extract_cube" ; msg = "Subsetted"
  }

  # execute aggregation or selection
  # args <- list(IC = IC, dims = all_dims, species = all_species)
  out <- purrr::map2_dfr(
    IC,
    seq_along(IC),
    ~rlang::exec(
      .fn = fun_nm,
      IC = .x,
      dims = all_dims[[.y]],
      plane = plane,
      species = all_species[[.y]],
      grid_cell = grid_cell,
      select_cell = select_cell
    )
  ) |>
    dplyr::mutate(
      sample.nm = title,
      file.nm = name,
      grid_size.nm = gridd,
      .before = "grid.nm"
    )

  # make corrections, check point package for documentation of `cor_IC`
  # only distinct metadata sets are taken as they are the same for different
  # elements
  if (isTRUE(corrected)) {
    out <- point::cor_IC(out, .bl_t = 0, .det = "EM")
  }

  # print or save
  if (isTRUE(save)) {
    if (!is.null(grid_cell)) grid_cell <- paste0(grid_cell, "_")
    object_name <- paste0(paste0(name, "_"), grid_cell, title)
    write_point(out, object_name)
    message(
      paste0(msg, " data has been saved with name ", object_name, " .")
    )
  } else {
    out
  }
}
#' @rdname grid_aggregate
#'
#' @export
grid_select <- grid_aggregate

#' @rdname grid_aggregate
#'
#' @export
tune_grid_cell <- function(grid_expr, tune, mc.cores = 1) {

  # catch call and check arguments
  gcall <- rlang::call_match(substitute(grid_expr), grid_aggregate)
  # remove grid_cell arg if needed and extract caller args
  args <- rlang::call_modify(gcall, grid_cell = rlang::zap()) |>
    rlang::call_args()

  # Aggregate and save (in parallel with `mclapply`). By using multiple cores
  # this exercise can be sped up significantly.
  rlang::inject(
    parallel::mclapply(tune, tune_grid_cell, !!!args, mc.cores = mc.cores)
  )
}

#-------------------------------------------------------------------------------
# additional supporting functions
#-------------------------------------------------------------------------------
# vectorized for plane
flatten_cube <- function(IC, dims, plane, species, grid_cell = NULL,
                         select_cell = NULL) {

  # function name of inner function
  fun_nm <- rlang::as_name(match.call()[[1]]) |> paste0("_")

  # args
  args <- list(IC = IC, dims = dims, species = species, grid_cell = grid_cell,
               select_cell = select_cell)

  # not needed for flatten_cube_
  if (fun_nm == "flatten_cube_") args <- args[names(args) != "select_cell"]

  # map over different planes
  purrr::map_dfr(plane, ~rlang::exec(fun_nm, plane = .x, !!! args))
}

# almost identical calls
extract_cube <- flatten_cube

# cast the ion count data cube into a long formatted dataframe but with original
# dimensions retained
extract_cube_ <- function(IC, dims, plane, species, grid_cell, select_cell) {

  # sub-sample
  x <- subsample(IC, dims, plane, grid_cell, select_cell, output = "select")

  # dimensions of grid_cell adapted to new data format
  dimr <- list(height.mt = 1:dim(IC)[1], width.mt = 1:dim(IC)[2],
               depth.mt = 1:dim(IC)[3])
  if (plane == "depth") {
    dimr[["height.mt"]] <- dimr[["width.mt"]] <- 1:grid_cell
    dimr <- tidyr::expand_grid(!!! dimr)
  } else if (plane == "height") {
    dimr[["height.mt"]] <- 1:grid_cell
    dimr <- tidyr::expand_grid(!!! dimr)
  } else if (plane == "width") {
    dimr[["width.mt"]] <- 1:grid_cell
    dimr <- tidyr::expand_grid(!!! dimr)
  }

  # cast in tibble
  out <- tibble::tibble(
    # grid number
    grid.nm = rep(select_cell, each = length(x) / length(select_cell)),
    dim.nm = which(names(dims) == plane), # planes of measurement
    dim_name.nm = plane, # names of the planes of measurement
    species.nm = species, # ion species name
    N.rw = unname(x), # raw ion counts (remove grid names)
    t.nm = 1e-3 # time (1 pixel per millisecond)
  )

  # expand new dimension to length of `select_cell` vector
  dimr <- dimr[rep(seq_len(nrow(dimr)), times = length(select_cell)), ]
  # bind to values
  dplyr::bind_cols(out, dimr)
}

# cast the ion count data cube into a flat long formatted dataframe
flatten_cube_ <- function(IC, dims, plane, species, grid_cell) {

  # sub-sample
  x <- subsample(IC, dims, plane, grid_cell)

  # dimensions remaining after flattening
  dimr <- dims[names(dims) != plane]

  # calculate max grid cell per plane
  if (plane == "depth") {
    gc <- prod(dimr / grid_cell)
  } else {
    gc <- dims[[plane]] / grid_cell
  }

  # grid numbering and naming
  if(!is.null(grid_cell)) {
    grd <- rep(1:gc, dims[[plane]]) # grid numbering
    dm <- (as.numeric(names(x)) - 1) %/% gc + 1 # dimension numbering
    px <- prod(dimr) / gc # pixels encompassed in single measurement
  } else {
    grd <- 1:prod(dimr) # grid numbering (equals pixel numbering)
    px <- dm <- 1
  }

  # cast in tibble
  tibble::tibble(
    grid.nm = grd, # grid number
    dim.nm = dm, # planes of measurement
    dim_name.nm = plane, # names of the planes of measurement
    species.nm = species, # ion species name
    N.rw = unname(x), # raw ion counts (remove grid names)
    t.nm = 1e-3 * px * .data$dim.nm, # time (1 pixel per millisecond)
    !!! grid_positions(dims, plane, grid_cell) # new dimension after reduction
  )
}

# sub-sample cube over grid of the same dimensions
subsample <- function(IC, dims, plane, grid_cell, select_cell =  NULL,
                      output = "subsample") {

  # matrix based sub-sampling where the margins denote the dimension over which
  # the cube is flattened
  if (is.null(grid_cell)) {
    # dimensions for aggregation
    dim_vc <- list(height = c(2, 3), width = c(1, 3), depth = c(1, 2))
    # sub-sample
    mt <- apply(IC, dim_vc[[plane]], sum)
    # remove dims and cast vector
    dim(mt) <- NULL
    mt
  # vectorized sub-sampling where the Kronecker produced grid number denotes
  # the sub-sample grid
  } else {
    # grid for aggregation
    grid <- kronecker_subsample_grid(dims, plane, grid_cell, output)
    # transpose cube if not sampling over depth
    # transpose array height
    if (plane == "height") IC <- aperm(IC, c(2, 3, 1))
    # transpose array width
    if (plane == "width") IC <- aperm(IC, c(1, 3, 2))

    # sub-sample
    if (output == "subsample") {
      c(tapply(IC, grid, sum))
    } else if (output == "select") {
      c(IC[grid %in% select_cell])
    }
  }
}

# if plane is depth ROI dims should be 2^x
kronecker_subsample_grid <- function(dims, plane, grid_cell,
                                     output = "subsample", expand = TRUE) {

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

    if (output == "subsample") {
      # expand into the 3th dimension and create unique grid numbers for each
      # slice
      return(outer(mt, max(mt) * 0:(dims[[plane]] - 1), `+`))

    } else if (output == "select") {
      # expand into the 3th dimension by duplicating the original matrix, so to
      # get identical grid numbers for each slice
      return(outer(mt, rep(1, dims[[plane]])))
    }
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
grid_positions <- function(dims, plane, grid_cell, expand = TRUE) {

  # dimensions remaining after flattening
  dimr <- dims[names(dims) != plane]

  # make grid
  if (!is.null(grid_cell)) {
    x <- purrr::imap(dimr, ~grid_positions_(.x, .y, grid_cell))
    gr <- tidyr::expand_grid(x[[2]], x[[1]])
    names(gr) <- paste0(names(dimr), ".mt") |> rev()
    # repeat
    if (isTRUE(expand)) {
      gr[rep(seq_len(nrow(gr)), times = dims[[plane]]), ]
    } else {
      gr
    }
  } else {
    gr <- tidyr::expand_grid(1 : dimr[[2]], 1 : dimr[[1]])
    names(gr) <- paste0(names(dimr), ".mt") |> rev()
    gr
  }
}

grid_positions_ <- function(dim, plane, grid_cell) {
  if (is.null(grid_cell)) return(1:dim)
  if (plane != "depth" | grid_cell == 1) {
    ceiling(seq(0, dim - grid_cell, grid_cell) + grid_cell / 2)
  } else {
    ceiling(dim / 2)
  }
}

# check for select_cell
check_select_cell <- function(select_cell, plane, dims, grid_cell) {
  grid <- kronecker_subsample_grid(dims, plane, grid_cell, expand = FALSE)
  if (all(!select_cell %in% grid)) {
    stop(paste0("Selected cell does not exist in sampling grid. Use ",
               "`gg_sketch(grid_cell = ",  grid_cell,
               ")` to view the sampling grid."),
         call. = FALSE)
  }
}
