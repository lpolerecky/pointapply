#' Scatter plots for very dens datasets
#'
#' \code{twodens} Scatter plots with 2D density as color and alpha scale to
#' visualise very dens data
#'
#' @param IC Ion count data.
#' @param x Variable for the x axis.
#' @param y Variable for the y axis.
#' @param xlab Character string or expression for x axis label.
#' @param ylab Character string or expression for x axis label.
#' @param ttl Character string or expression for plot title.
#' @param downsample A numeric supplying the fraction for down-sampling of the
#' original data frame.
#' @param gr A grouping variable.
#' @param x_lim Numeric range for x axis (default = NULL).
#' @param y_lim Numeric range for y axis (default = NULL).
#' @param unit A character string for the units of the grouping variable. The
#' string \code{"um"} is used for the grid cell size comparison plots. The
#' string \code{"dim"} is used for all other plots, and defaults to the grouping
#' variable name.
#' @param sds Numeric settings the seeds for down-sampling via `set.seed()`.
#' @param geom A character string for the geometry mapped on the aesthetics.
#' Options are  \code{"point"} for normal scatter plots, and \code{"dens2d"} for
#' density contour lines.
#' @param facet_sc The \code{scales} parameter of `ggplot2::facet_grid`
#'
#' @return \code{ggplot2::\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
twodens <- function(IC, x, y, xlab, ylab, ttl, downsample, gr, x_lim = NULL,
                    y_lim = NULL, unit = "um", sds = 22, geom = "point",
                    facet_sc = "fixed"){

  gr <- enquo(gr)
  set.seed(sds)

  # grouping
  if (!is.null(rlang::get_expr(gr))) {

    IC <- group_by(IC, !!gr)
    ran <- range(count(IC, !!gr)$n)

    if (unit == "um") {
      IC <- mutate(IC, !!gr:= round(!!gr, 1))
      lab <- eval(
        rlang::call2(
          "label_bquote",
          substitute(.(x)~mu*"m"^2, lst(x =  rlang::get_expr(gr)))
        )
      )
    }
    if (unit == "dim") lab <- ggplot2::label_value
  } else {
    ran <- range(count(IC)$n)
  }

  # downsample
  IC <- slice_sample(IC, prop = downsample)

  # calculate density
  IC <- mutate(
    IC,
    h_x = if_else(
      MASS::bandwidth.nrd({{x}}) == 0,
      0.1,
      MASS::bandwidth.nrd({{x}})
      ),
    h_y = if_else(
      MASS::bandwidth.nrd({{y}}) == 0,
      0.1,
      MASS::bandwidth.nrd({{y}})
      ),
    dens = get_density(
      {{x}}, {{y}},
      h = c(.data$h_x, .data$h_y),
      n = log(ran[2]) / log(n()) * 100
      ),
    # Adjust point alpha
    alpha_sc = (ran[1] / n()) / 4
  )

  p <- ggplot(IC, aes(x = {{x}}, y = {{y}}))

  if (geom == "point") {
    p <- p + geom_point(aes(color = .data$dens, alpha =  .data$alpha_sc))
    }
  if (geom == "dens2d")  {
    p <- p + ggplot2::stat_density_2d(
      aes(fill = ..ndensity..),
      geom = "raster",
      contour = FALSE,
      contour_var = "count",
      show.legend = FALSE,
      h = c(IC$h_x, IC$h_y)
      ) +
      scale_fill_distiller(
        limits = c(0.01, 1),
        breaks = seq(0.01, 1, length.out = 100),
        palette = "YlOrRd",
        direction = 1,
        na.value = "transparent"
      )
    }
  p <- p + scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_color_distiller(
      breaks = seq(0, 1, length.out = 100),
      palette = "YlOrRd",
      direction = 1,
      na.value = "transparent",
      guide = FALSE
      ) +
    scale_alpha_identity(guide = FALSE, limits = c(1e-5, 1)) +
    labs(
      title = ttl,
      x = xlab,
      y = ylab
      ) +
    ggplot2::theme_bw() +
    themes_IC

  if (!is.null(x_lim)) p <- p + ggplot2::xlim(x_lim)
  if (!is.null(y_lim)) p <- p + ggplot2::xlim(y_lim)

  if (!is.null(rlang::get_expr(gr))) {
    return(p + facet_wrap(vars(!!gr), labeller = lab, scales = .data$facet_sc))
    } else {
    return(p)
    }
}


# denisty calculation function (https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/)
get_density <- function(x, y, h, n) {
  density_out <- MASS::kde2d(x, y, h, n)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}
