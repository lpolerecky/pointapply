#' Raster images for ion count ratios
#'
#' \code{gg_cnts} Raster images based on the ratio of ion counts.
#'
#' @param title Character string for the analyte ("MEX" or "MON") to be used.
#' @param ion1 A character string constituting the rare isotope ("13C").
#' @param ion2 A character string constituting the common isotope ("12C").
#' @param viri Character string selecting the viriditas color scheme.
#' @param res Pixel resolution.
#' @param grid_cell Pixel size of grid_cells (one side of the square).
#' @param scaler Numeric converting the pixels to metric dimension of
#'  measurement (default is the conversion used in this study).
#' @param compilation Logical whether to plot all three dimensions according to
#'  the grid layout
#'  (see \code{pointapply::\link[pointapply:gg_sketch]{gg_sketch}}).
#' @param save Boolean whether to save the plot as an png.
#' @param .name Name of the raster file.
#'
#' @return \code{ggplot2::\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
gg_cnts <- function(title, ion1, ion2, viri = "D", res = 256,
                    grid_cell = 64, scaler = 40 / 256, compilation = FALSE,
                    save = FALSE, .name = "map_raster_image") {

  # raster data
  IC <-  load_point(.name, title, return_name = TRUE) |>
    rlang::sym()

  # expressions for ion species counts (to be used in wide format dataframe)
  Xt1 <- rlang::parse_expr(paste("Xt.pr", ion1, sep = "."))
  Xt2 <- rlang::parse_expr(paste("Xt.pr", ion2, sep = "."))

  # filter data to only include the species of interest
  IC <- rlang::inject(dplyr::filter(!!IC, species.nm %in% c(ion1, ion2)))

  # make height and width of similar size as grid_cell
  if (isTRUE(compilation)) {
    IC <- dim_aggregate(IC, grid_cell)
    # remove height and width aggregate
  } else {
    IC <- dplyr::filter(IC, dim_name.nm == "depth")
  }

  # correct ion counts
  IC <- point::cor_IC(IC, .bl_t = 0, .det = "EM", .hide = FALSE)

  # reduce dimensions to 2D grid
  IC <- dim_folds(IC, "raster", res, grid_cell)

  # pivot the data frame to a wide format with the two ion species besides each
  # other (see point documentation for `cov_R`)
  IC <- point::cov_R(IC, c(ion1, ion2), .data$sample.nm, .data$file.nm,
                     .data$grid.nm, .data$dim_name.nm, .data$height.mt,
                     .data$width.mt) |>
    # add isotope ratio
    dplyr::mutate(R = !!Xt1 / !!Xt2)

  # build plot
  p <- ggplot2::ggplot(IC, ggplot2::aes(x = .data$width.mt, y = .data$height.mt)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data$R)) +
    ggplot2::scale_y_continuous(
      expression("height"~"("*mu*"m)"),
      trans = "reverse",
      breaks = seq(0, res, grid_cell),
      labels =  function(x) x * scaler,
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      expression("width"~"("*mu*"m)"),
      breaks = seq(0, res, grid_cell),
      labels = function(x) x * scaler,
      expand = c(0, 0)
    ) +
    ggplot2::scale_fill_viridis_c(
      point::R_labeller(ion1, ion2, "expr"),
      option = viri
    ) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = "top")) +
    ggplot2::theme(
      axis.title.y =
        ggplot2::element_text(hjust = ifelse(compilation, 0.65, 0.5)),
      axis.title.x =
        ggplot2::element_text(hjust = ifelse(compilation, 0.35, 0.5))
    ) +
    themes_IC()

  # these arrows demarcate the depth dimension
  if (isTRUE(compilation)) p <- p + depth_arrows(res, grid_cell)

  # save plot
  if (isTRUE(save)) {
    nm <- paste("simple_raster", title)
    save_point(nm, p, width = 10, height = 8, unit = "cm")
  }

  # suppress warning
  withr::local_options(list(warn = -1))

  # print
  p
}

# Depth dimensions are different then on the surface. This function makes the
# depth equal to width and height of the grid_cells selected
dim_aggregate <- function(IC, grid_cell) {

  depth <- dplyr::filter(IC, .data$dim_name.nm == "depth")
  purrr::map_dfr(c("height", "width"), ~dim_aggregate_(IC, grid_cell, .x)) |>
    dplyr::bind_rows(depth)
}

dim_aggregate_ <- function(IC, grid_cell, dim) {
  # dimensions and their respective variables
  dims <- c(height = "height.mt", width = "width.mt")
  # variables
  vars <- c("grid.nm", "t.nm", "N.rw", "depth.mt")

  # transform depth dimension
  dplyr::filter(IC, .data$dim_name.nm == dim) |>
    dplyr::arrange(!! rlang::sym(dims[names(dims) != dim])) |>
    dplyr::group_by(
      .data$species.nm,
      !! rlang::sym(dims[names(dims) != dim]),
      depth.mt = dplyr::ntile(.data$depth.mt, grid_cell)
      ) |>
    dplyr::summarise(
      # sum counts
      dplyr::across(dplyr::any_of(c(vars, unname(dims))), sum),
      dplyr::across(-dplyr::any_of(c(vars,  unname(dims))), unique),
      .groups = "drop"
    )
}
