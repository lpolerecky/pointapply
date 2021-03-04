#' Heatmaps and summary plots for model performance.
#'
#' \code{heat_map} generates a heatmap for model accuracy evaluation along
#' two continuous variables and up to two categorical values. Instead
#' \code{heat_sum} summarise the heatmap over opne of the two continuous and
#' crossed variables.
#'
#' @param simu Ion count data.
#' @param x Variable (continuous variable number one) isotope offset in paper.
#' @param y Variable (continuous variable number two) excess ionization
#' efficiency in paper.
#' @param stat Numeric for the model accuracy (between 0 and 1).
#' @param grp1 Nominal factorial number one; outlier detection method in paper.
#' @param grp2 Nominal factorial number one; for intra-isotope variability in
#' paper.
#' @param conversion Named vector for the conversion of the ionization trend to
#' excess ionization efficiency.
#' @param ttl Character string or expression for the plot title.
#' @param x_lab Character string or expression for the x-axis label.
#' @param y_lab Character string or expression for the y-axis label.
#' @param x_sec Character string or expression for the secondary x-axis label.
#' @param y_sec Character string or expression for the secondary y-axis label.
#' @param trans_base Isotope value for the scale transformation for fractional
#' size of the isotope anomaly.
#' @param trans_n Initial sample size for the scale transformation for
#' fractional size of the isotope anomaly.
#'
#' @return \code{\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
heat_map <- function(simu, x, y, stat, grp1, grp2, conversion, ttl, x_lab,
                     y_lab, x_sec = NULL, trans_base = -2, trans_n = 10){

  grp1 <- enquo(grp1)
  grp2 <- enquo(grp2)

  # create rectangle annotation
  rcts <- rect_fun(xaxis = pull(simu, {{x}}), yaxis = pull(simu, {{y}}))

  # transformation of secondary axis
  if (!is.null(x_sec)){
    tr <- rayleigh_trans(trans_base, trans_n)
    second_axis <- ggplot2::sec_axis(
      trans = ~tr$transform(.) * -1 ,
      name = x_sec,
      breaks = scales::pretty_breaks(3)
      )
  } else {
    second_axis <- ggplot2::waiver()
    }

  # plot
  p <- ggplot(simu, aes(x = {{x}}, y = {{y}}, fill = {{stat}})) +
    geom_tile()

  # facets
  if (!all(purrr::map_lgl(list(grp1, grp2), ~is.null(rlang::get_expr(.x))))) {
    p <- p + facet_grid(
      rows = vars(!!grp1),
      cols = vars(!!grp2),
      labeller = ggplot2::label_parsed
      )
    }

  p + scale_x_continuous(
      labels = scales::label_number(),
      expand = c(0, 0),
      # secondary x axis
      sec.axis = second_axis
      ) +
    scale_y_continuous(
      breaks = unname(conversion),
      labels = names(conversion),
      expand = c(0, 0)
      ) +
    scale_fill_distiller("classification \n accuracy", palette = "YlOrRd") +
    ggplot2::coord_fixed(ratio = rcts$r_cell[1]) +
    ggplot2::geom_rect(
      data = rcts,
      aes(xmin = .data$xl, xmax = .data$xu, ymin = .data$yl, ymax = .data$yu, color = .data$col_type),
      fill = "transparent",
      inherit.aes = FALSE
      ) +
    ggplot2::scale_size(range = c(0.5, 1.5)) +
    ggplot2::scale_color_identity() +
    ggrepel::geom_text_repel(
      data = rcts,
      aes(x = .data$x, y = .data$y, label = .data$lbl),
      inherit.aes = FALSE,
      size = 3,
      min.segment.length = 1e-3,
      nudge_x = -7,
      nudge_y = 25
      ) +
    labs(title = ttl, x = x_lab, y = y_lab) +
    ggplot2::theme_bw() +
    themes_IC
}
#' @rdname heat_map
#'
#' @export
heat_sum <- function(simu, x, y, stat, grp1, grp2, conversion, ttl, x_lab,
                     y_lab, x_sec, y_sec, trans_base = -20, trans_n = 6){

  # default colors
  def_cols <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")

  # transformation of secondary axis
  tr <- rayleigh_trans(trans_base, trans_n)

  # specificity background rectangle
  tb_rec <-tibble(
    xmin = -1.5,
    xmax = 1.5,
    ymin = -0.1,
    ymax = 1.1,
    flag = "Specificity"
    )

  # background labels
  tb_txt <- tibble(
    x = rep(-17, 2),
    y = rep(0.25, 2),
    type.nm = c("asymmetric", "symmetric"),
    flag = rep("Sensitivity", 2)
    )

  ggplot(
    simu,
    aes(
      x = {{x}},
      y = {{y}},
      fill = {{stat}},
      linetype = {{stat}},
      group = {{stat}}
      )
    ) +
  ggplot2::geom_rect(
    data = tb_rec,
    aes(
      xmin = .data$xmin,
      xmax = .data$xmax,
      ymin = .data$ymin,
      ymax = .data$ymax
      ),
    fill = def_cols[1],
    inherit.aes = FALSE
    ) +
  geom_text(
    data = tb_txt,
    aes(
      x = .data$x,
      y = .data$y,
      label = .data$type.nm
      ),
    size = 3,
    inherit.aes = FALSE
    ) +
  geom_line() +
  ggplot2::scale_linetype(guide = FALSE) +
  geom_point(shape = 21) +
  # Rayleigh model secondary axis
  scale_x_continuous(
    breaks = seq(-20, 0,5),
    sec.axis =
      ggplot2::sec_axis(
        trans = ~tr$transform(.) * -1,
        x_sec,
        breaks = c(0.15, 0.1, 0.05)
      )
    ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(3),
    sec.axis = ggplot2::dup_axis(name = y_sec),
    expand = c(-0.1, 0.1)
    ) +
  ggplot2::scale_fill_manual("", values = c("white", "grey")) +
  facet_grid(
    rows = vars({{grp1}}),
    cols = vars({{grp2}}),
    scales = "free_x",
    space = "free_x"
    ) +
  labs(title = ttl, x = x_lab, y = y_lab) +
  ggplot2::theme_bw() +
  themes_IC +
  ggplot2::theme(
    legend.position = "right",
    legend.key = ggplot2::element_blank(),
    strip.placement = 'outside',
    strip.background.x = ggplot2::element_blank(),
    strip.background.y = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_blank(),
    strip.text.y = ggplot2::element_blank()
    )

}

#-------------------------------------------------------------------------------
# not exportet
#-------------------------------------------------------------------------------

rect_fun <- function(xaxis, yaxis){

  # dimensions of simulated ion data
  dims_sim <- lst(
    n_iso = n_distinct(xaxis),
    min_iso = min(xaxis),
    max_iso = max(xaxis),
    var_iso = diff(c(.data$max_iso, .data$min_iso)),
    n_trend = n_distinct(yaxis),
    min_trend = min(yaxis),
    max_trend = max(yaxis),
    var_trend = diff(c(.data$max_trend, .data$min_trend)),
    r_cell = .data$var_iso / .data$var_trend
  )

  # annotation for plot
  rcts <- tibble(
    x = c(
      dims_sim$max_iso,
      dims_sim$var_iso / 2,
      dims_sim$var_iso / 2
    ),
    xl = c(
      dims_sim$max_iso + ((dims_sim$var_iso / dims_sim$n_iso) / 2),  # hom
      dims_sim$min_iso + ((dims_sim$var_iso / dims_sim$n_iso) / 2),  # het
      dims_sim$min_iso + ((dims_sim$var_iso / dims_sim$n_iso) / 2) # em
    ),
    xu = c(
      dims_sim$max_iso - ((dims_sim$var_iso / dims_sim$n_iso) / 2), # hom
      dims_sim$max_iso + ((dims_sim$var_iso / dims_sim$n_iso) / 2), # het
      dims_sim$max_iso - ((dims_sim$var_iso / dims_sim$n_iso) / 2) # em
    ),
    y = c(
      -dims_sim$var_trend / 2,
      -dims_sim$var_trend / 2,
      dims_sim$min_trend
    ),
    yl = c(
      dims_sim$min_trend - ((dims_sim$var_trend / dims_sim$n_trend) / 2), # hom
      dims_sim$min_trend - ((dims_sim$var_trend / dims_sim$n_trend) / 2), # het
      dims_sim$min_trend + ((dims_sim$var_trend / dims_sim$n_trend) / 2) # em
    ),
    yu = c(
      dims_sim$max_trend - ((dims_sim$var_trend / dims_sim$n_trend) / 2), # hom
      dims_sim$max_trend - ((dims_sim$var_trend / dims_sim$n_trend) / 2), # het
      dims_sim$min_trend - ((dims_sim$var_trend / dims_sim$n_trend) / 2) # em
    ),
    lbl = c(
      "homogeneous",
      "heterogeneous",
      "stable ionization"
    ),
    col_type = c("black", "black", "red"),
    r_cell = dims_sim$r_cell
  )
}


# Rayleigh scale (with sign conversion)
# base is the initial isotope offset to be considered
# frac is the initial fraction of the samples to be considered
rayleigh_trans <- function(base, frac){

  force(base)
  force(frac)
  rayleigh_t <- function(x) (sign(x) * sqrt(abs(x / base))) / frac
  rayleigh_i <- function(x) ((base * x) ^ 2 * sign(x)) * frac

  scales::trans_new(paste0("rayleigh", format(base), "-",format(frac)) , rayleigh_t, rayleigh_i)

}

