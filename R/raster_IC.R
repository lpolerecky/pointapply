#' Raster images for ion count ratios
#'
#' \code{gg_cnts} Raster images based on the ratio of ion counts.
#'
#' @param IC Ion count dataframe.
#' @param ion1 A character string constituting the rare isotope ("13C").
#' @param ion2 A character string constituting the common isotope ("12C").
#' @param viri Character string selecting the viriditas color scheme.
#' @param height Pixels of height.
#' @param width Pixels of width.
#' @param grid_cell Pixel size of grid_cells (one side of the square).
#' @param scaler Numeric converting the pixels to metric dimension of
#' measurement (default is the conversion used in this study).
#' @param compilation Logical whether to plot all three dimensions according to
#' the grid layout
#' (see \code{pointapply::\link[pointapply:gg_sketch]{gg_sketch}}).
#'
#' @return \code{ggplot2::\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
gg_cnts <- function(IC, ion1, ion2, viri = "D", res = 256,
                    grid_cell = 64, scaler = 40 / 256, compilation = FALSE) {

  # expressions for ion species counts (to be used in wide format dataframe)
  Xt1 <- rlang::parse_expr(paste("Xt.pr", ion1, sep = "."))
  Xt2 <- rlang::parse_expr(paste("Xt.pr", ion2, sep = "."))

  # check if species exist in data
  stopifnot(c(ion1, ion2) %in% IC$species.nm)
  # filter data to only include the species of interest
  IC <- dplyr::filter(IC, species.nm %in% c(ion1, ion2))

  # try unfolding attributes (point function) to see whether coordinate system
  # is available
  withCallingHandlers(
    warning = function(cnd) {
      IC$width.mt
      IC$height.mt
      IC$depth.mt
    },
    {
      # make metadata available
      IC <- point::unfold(IC)
      # reduce dimensions to 2D grid
      IC <- dim_folds(IC, height.mt, width.mt, depth.mt, "raster", res,
                      grid_cell)
    }
  )

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
    # these arrows demarcate the depth dimension
    depth_arrows(res, grid_cell) +
    ggplot2::scale_y_continuous(
      expression("height"~"("*mu*"m)"),
      limits = c(res + ifelse(compilation, grid_cell * 1.5, 0), 0),
      trans = "reverse",
      breaks = seq(0, res, grid_cell),
      labels =  function(x) x * scaler,
      expand = c(0, 0)
      ) +
    ggplot2::scale_x_continuous(
      expression("width"~"("*mu*"m)"),
      limits = c(0, res + ifelse(compilation, grid_cell * 1.5, 0)),
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
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.y =
        ggplot2::element_text(hjust = ifelse(compilation, 0.65, 0.5)),
      axis.title.x =
        ggplot2::element_text(hjust = ifelse(compilation, 0.35, 0.5))
    ) +
    themes_IC()

  # suppress warnings related to uneven raster
  suppressWarnings(print(p))
}
