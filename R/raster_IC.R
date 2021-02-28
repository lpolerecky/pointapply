#' Raster images for ion count ratios
#'
#' \code{gg_cnts} Raster images based on the ratio of ion counts.
#'
#' @param .data Dataframe.
#' @param .ion1 A character string constituting the rare isotope ("13C").
#' @param .ion2 A character string constituting the common isotope ("12C").
#' @param ttl Character string or expression for plot title.
#' @param viri Character string selecting the viriditas color scheme.
#' @param scaler Numeric converting the pixels to metric dimension of
#' measurement (default is the conversion used in this study).
#' @param compilation Logical whether to plot all three dimensions according to
#' the sketch ...
#'
#' @return \code{\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
gg_cnts <- function(.data, ion1, ion2, ttl, viri, height = 256, width = 256,
                    grid_cell = 64, scaler = 40 / 256, compilation = FALSE) {

  Xt1 <- rlang::parse_expr(paste("Xt.pr", ion1, sep = "."))
  Xt2 <- rlang::parse_expr(paste("Xt.pr", ion2, sep = "."))

  .data <- point::cov_R(.data, c(ion1, ion2), file.nm, sample.nm, dim_name.nm,
                        height, width, grid.nm) %>%
    mutate(R = !!Xt1 / !!Xt2)

  ggplot(.data, aes(x = width, y = height)) +
    geom_raster(aes(fill = R)) +
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
    scale_fill_viridis_c(
      point::R_labeller(ion1, ion2, "expr"),
      option = viri
      ) +
    coord_fixed() +
    guides(fill = guide_colorbar(title.position = "top")) +
    theme_classic() +
    theme(
      axis.title.y = element_text(hjust = ifelse(compilation, 0.65, 0.5)),
      axis.title.x = element_text(hjust = ifelse(compilation, 0.35, 0.5))
      )
}
