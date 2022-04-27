#' Scatter plots for very dens datasets
#'
#' \code{gg_dens} Scatter plots with 2D density as color and alpha scale to
#' beter visualise very dens data.
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
#' @param labels Either "scientific" or "standard".
#' @param facet_sc The \code{scales} parameter of `ggplot2::facet_grid`
#' @param flag A variable representing a flag for outliers.
#' @param save Boolean whether to save the plot as an png.
#'
#' @return \code{ggplot2::\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
gg_dens <- function(IC, x, y, xlab = ggplot2::waiver(),
                    ylab = ggplot2::waiver(), ttl = "", gr = NULL,
                    downsample = 1, x_lim = NULL, y_lim = NULL, unit = "dim",
                    sds = 22, geom = "point", facet_sc = "fixed",
                    labels = "standard", flag = NULL, save = FALSE){

  set.seed(sds)

  # grouping
  gr <- enquo(gr)

  # variables
  x <- enquo(x)
  y <- enquo(y)
  flag <- enquo(flag)

  # grouping
  if (!is.null(rlang::get_expr(gr))) {

    IC <- dplyr::group_by(IC, !!gr)
    ran <- range(count(IC, !!gr)$n)

    # facet labels
    if (unit == "um") {
      IC <- dplyr::mutate(IC, !!gr:= round(!!gr, 1))
      lab <- eval(
        rlang::call2(
          "label_bquote",
          substitute(.(x)~mu*"m"^2, list(x =  rlang::get_expr(gr))),
          .ns = "ggplot2"
        )
      )
    } else if (unit == "dim") {
      lab <- ggplot2::label_value
    }

  } else {
    ran <- range(count(IC)$n)
  }

  # down-sample
  IC <- dplyr::slice_sample(IC, n =  min(count(IC)$n) * downsample)

  # calculate 2D density
  IC <- point::twodens(IC, !! x, !! y, !! gr, .flag = !! flag)

  # base plot
  p <- ggplot2::ggplot(data = IC, ggplot2::aes(x = !! x , y = !! y ))

  # legend width
  plot_width <- as.numeric(ggplot2::ggplotGrob(p)$widths[1]) *
    ceiling(log2(nrow(dplyr::distinct(IC, !! gr))))

  if (geom == "point") {
    p <- dens_point(p, flag, IC, plot_width)
  }

  if (geom == "dens2d")  {
    p <- p +
      ggplot2::stat_density_2d(
        ggplot2::aes(fill = ..ndensity..),
        geom = "raster",
        contour = FALSE,
        contour_var = "count",
        show.legend = FALSE,
        h = c(IC$h_x, IC$h_y)
      ) +
      ggplot2::scale_fill_distiller(
        limits = c(0.01, 1),
        breaks = seq(0.01, 1, length.out = 100),
        palette = "YlOrRd",
        direction = 1,
        na.value = "transparent"
      )
  }

  # axis labels
  if (labels == "scientific") {
    labs <- scales::label_scientific()
  } else {
    labs <- ggplot2::waiver()
  }

  p <- p + ggplot2::scale_x_continuous(
      name = xlab,
      limits = x_lim,
      breaks = scales::breaks_pretty(n = 5, min.n = 2),
      labels = labs
    ) +
    ggplot2::scale_y_continuous(
      name = ylab,
      limits = y_lim,
      breaks = scales::breaks_pretty(n = 5, min.n = 3),
      labels = labs
    ) +
    ggplot2::ggtitle(ttl) +
    themes_IC(base = ggplot2::theme_bw())

  if (!is.null(rlang::get_expr(gr))) {
    p <- p + ggplot2::facet_wrap(
      vars(!! gr),
      labeller = lab,
      scales = facet_sc
    )
  }

  if (isTRUE(save)) {
    # save plot
    nm <- paste("point", gsub(" ", "_", ttl))
    save_point(nm, p, width = 12, height = 8, unit = "cm")

    message(
      paste0("Residuals plot has been saved with name ", nm, " .")
    )
  }
  p
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

  # base
  p <- p + ggplot2::geom_point(
    ggplot2::aes(color = .data$dens, alpha = .data$alpha_sc)
  )

  if (rlang::is_symbol(rlang::get_expr(flag))) {
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
      ggplot2::scale_color_distiller(
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
  p + ggplot2::scale_alpha_identity(guide = "none", limits = c(1e-5, 1))
}

