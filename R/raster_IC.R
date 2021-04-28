#' Raster images for ion count ratios
#'
#' \code{gg_cnts} Raster images based on the ratio of ion counts.
#'
#' @param IC Ion count dataframe.
#' @param ion1 A character string constituting the rare isotope ("13C").
#' @param ion2 A character string constituting the common isotope ("12C").
#' @param ttl Character string or expression for plot title.
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
gg_cnts <- function(IC, ion1, ion2, ttl, viri, height = 256, width = 256,
                    grid_cell = 64, scaler = 40 / 256, compilation = FALSE) {

  Xt1 <- rlang::parse_expr(paste("Xt.pr", ion1, sep = "."))
  Xt2 <- rlang::parse_expr(paste("Xt.pr", ion2, sep = "."))

  IC <- point::cov_R(IC, c(ion1, ion2), file.nm, sample.nm, dim_name.nm,
                     height, width, grid.nm) %>%
    mutate(R = !!Xt1 / !!Xt2)

  ggplot(IC, aes(x = .data$width, y = .data$height)) +
    geom_raster(aes(fill = .data$R)) +
    scale_y_continuous(
      expression("height"~"("*mu*"m)"),
      limits = c(height + ifelse(compilation, grid_cell * 1.5, 0) + 1, 0),
      trans = "reverse",
      breaks = seq(1, height + 1, grid_cell),
      labels =  function(x) (x - 1) * scaler,
      expand = c(0, 0)
      ) +
    scale_x_continuous(
      expression("width"~"("*mu*"m)"),
      limits = c(0, width + ifelse(compilation, grid_cell * 1.5, 0) + 1),
      breaks = seq(1, width + 1, grid_cell),
      labels = function(x) (x - 1) * scaler,
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
      axis.title.y = ggplot2::element_text(hjust = ifelse(compilation, 0.65, 0.5)),
      axis.title.x = ggplot2::element_text(hjust = ifelse(compilation, 0.35, 0.5))
      )
}
