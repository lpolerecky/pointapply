#' Scatter plots for very dens datasets
#'
#' \code{gg_dens} Scatter plots with 2D density as color and alpha scale to
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
#' @param flag A variable representing a flag for outliers.
#'
#' @return \code{ggplot2::\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
gg_dens <- function(IC, x, y, xlab = ggplot2::waiver(),
                    ylab = ggplot2::waiver(), ttl = "", gr = NULL,
                    downsample = 1, x_lim = NULL, y_lim = NULL, unit = "dim",
                    sds = 22, geom = "point", facet_sc = "fixed", flag = NULL){

  gr <- enquo(gr)
  set.seed(sds)
  x <- enquo(x)
  y <- enquo(y)
  flag <- enquo(flag)

  # grouping
  if (!is.null(rlang::get_expr(gr))) {

    IC <- group_by(IC, !!gr)
    ran <- range(count(IC, !!gr)$n)

    if (unit == "um") {
      IC <- mutate(IC, !!gr:= round(!!gr, 1))
      lab <- eval(
        rlang::call2(
          "label_bquote",
          substitute(.(x)~mu*"m"^2, lst(x =  rlang::get_expr(gr))),
          .ns = "ggplot2"
        )
      )
    }
    if (unit == "dim") lab <- ggplot2::label_value
  } else {
    ran <- range(count(IC)$n)
  }


  # down-sample
  IC <- slice_sample(IC, n =  min(count(IC)$n) * downsample)

  # calculate 2D density
  IC <- point::twodens(IC, !! x, !! y, !! gr, .flag = !! flag)

  p <- ggplot(data = IC, aes(x = !! x , y = !! y ))

  plot_width <- as.numeric(ggplot2::ggplotGrob(p)$widths[1]) *
    ceiling(log2(nrow(distinct(IC, !! gr))))

  if (geom == "point") {
    p <- dens_point(p, flag, IC, plot_width)
    }

  if (geom == "dens2d")  {
    p <- p +
      ggplot2::stat_density_2d(
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

  p <- p +
    scale_x_continuous(
      name = xlab,
      expand = c(0, 0),
      limits = x_lim,
      labels = scales::label_scientific(2),
      breaks = scales::pretty_breaks(3)
      ) +
    scale_y_continuous(
      name = ylab,
      expand = c(0, 0),
      limits = y_lim,
      labels = scales::label_scientific(2),
      breaks = scales::pretty_breaks(3)
      ) +
    ggtitle(ttl) +
    themes_IC(base = ggplot2::theme_bw())

  if (!is.null(rlang::get_expr(gr))) {
    return(p + facet_wrap(vars(!! gr), labeller = lab, scales = facet_sc))
    } else {
     return(p)
     }
}

# density calculation function (https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/)
get_density <- function(x, y, h, n) {
  density_out <- MASS::kde2d(x, y, h, n)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}

dens_point <- function (p, flag, IC, width) {
  # colours
  div_col <- c("#8E063B", "#BB7784", "#D6BCC0", "#E2E2E2", "#BEC1D4", "#7D87B9",
               "#023FA5") # colorspace::diverge_hcl(7, rev = TRUE)
  p <- p +
    geom_point(aes(color = .data$dens, alpha = .data$alpha_sc))
  if (is_symbol(get_expr(flag))) {
    p <- p +
      ggplot2::scale_color_gradientn(
        "",
        breaks = range(IC$dens),
        labels =  c("divergent", "confluent"),
        colors = div_col,
        na.value = "transparent",
        guide = guide_colourbar(ticks = FALSE, barwidth = width)
      )
  } else {
    p <- p +
      scale_color_distiller(
        "",
        breaks = range(IC$dens),
        labels =  c("low density", "high density"),
        palette = "YlOrRd",
        direction = 1,
        na.value = "transparent",
        guide = guide_colourbar(ticks = FALSE, barwidth = width)
        )
  }
  # alpha
  p <- p +
    scale_alpha_identity(guide = "none", limits = c(1e-5, 1))
  return(p)
}

