#' Combination plot
#'
#' \code{gg_effect} Combine raster images for ion count ratios with high
#' precision isotope ratios.
#'
#' @param IC Ion count data.
#' @param grid_sel Integer selecting an grid-cell for in-depth analysis.
#' @param image Dataframe for ion raster map.
#' @param ions A character vector of three ions involved in the analysis.
#' @param ion1_thr Character string for ion in the enumerator of ion ratio of
#' the map.
#' @param ion2_thr Character string for ion in the denominator of ion ratio of
#' the map.
#' @param thr Numeric threshold value for filter selection.
#' @param ion1_R A character string constituting the rare isotope ("13C").
#' @param ion2_R A character string constituting the common isotope ("12C").
#' @param Xt Variable for ion count rates.
#' @param N Variable for ion counts.
#' @param colors Color palette to identify matrix and inclusion.
#'
#' @return \code{\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
heat_map <- function(simu, x, y, stat, grp1, grp2, conversion, ttl, x_lab,
                     y_lab){

  # create rectangle annotation
  rcts <- rect_fun(xaxis = pull(simu, {{x}}), yaxis = pull(simu, {{y}}))

  # plot
  p <- ggplot(simu, aes(x = {{x}}, y = {{y}}, fill = {{stat}})) +
    geom_tile()

  # facets
  if (!all(sapply(list(grp1, grp2), is.null))) {
    p <- p + facet_grid(
      rows = vars({{grp1}}),
      cols = vars({{grp2}}),
      labeller = label_parsed
    )
  }

  p + scale_x_continuous(
      labels = scales::label_number(),
      expand = c(0, 0)
      ) +
    scale_y_continuous(
      breaks = unname(conversion),
      labels = names(conversion),
      expand = c(0, 0)
      ) +
    scale_fill_distiller("classification \n accuracy", palette = "YlOrRd") +
    coord_fixed(ratio = rcts$r_cell[1]) +
    geom_rect(
      data = rcts,
      aes(xmin = xl, xmax = xu, ymin = yl, ymax = yu, color = col_type),
      fill = "transparent",
      inherit.aes = FALSE
    ) +
    scale_size(range= c(0.5, 1.5)) +
    scale_color_identity() +
    ggrepel::geom_text_repel(
      data = rcts,
      aes(x = x, y = y, label = lbl),
      inherit.aes = FALSE,
      size = 3,
      min.segment.length = 1e-3,
      nudge_x = -7,
      nudge_y = 25
      ) +
    labs(title = ttl, x = x_lab, y = y_lab) +
    theme_bw() +
    themes_IC
}
#' @rdname heat_map
#'
#' @export
heat_sum <- function(simu, x, y, stat, grp1, grp2, conversion, ttl, x_lab,
                     y_lab, x_sec, y_sec){

  # default colors
  def_cols <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")

  # transformation with hypothetical inclusion of -20 and a frac size of 1/6
  tr <- rayleigh_trans(-20, 6)

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
  geom_rect(
    data = tb_rec,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
      ),
    fill = def_cols[1],
    inherit.aes = FALSE
    ) +
  geom_text(
    data = tb_txt,
    aes(
      x = x,
      y = y,
      label = type.nm
      ),
    size = 3,
    inherit.aes = FALSE
    ) +
  geom_line() +
  scale_linetype(guide = FALSE) +
  geom_point(shape = 21) +
  # Rayleigh model secondary axis
  scale_x_continuous(
    breaks = seq(-20, 0,5),
    sec.axis =
      sec_axis(
        trans = ~tr$transform(.) * -1,
        x_sec,
        breaks = c(0.15, 0.1, 0.05)
      )
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(3),
    sec.axis =  dup_axis(name = y_sec),
    expand = c(-0.1, 0.1)
  ) +
  scale_fill_manual("", values = c("white", "grey")) +
  facet_grid(
    rows = vars({{grp1}}),
    cols = vars({{grp2}}),
    scale = "free_x",
    space = "free_x"
    ) +
  labs(title = ttl, x = x_lab, y = y_lab) +
  theme_bw() +
  themes_IC +
  theme(
    legend.position = "right",
    legend.key = element_blank(),
    strip.placement = 'outside',
    strip.background.x = element_blank(),
    strip.background.y = element_blank(),
    strip.text.x = element_blank(),
    strip.text.y = element_blank()
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
    var_iso = diff(c(max_iso, min_iso)),
    n_trend = n_distinct(yaxis),
    min_trend = min(yaxis),
    max_trend = max(yaxis),
    var_trend = diff(c(max_trend, min_trend)),
    r_cell = var_iso / var_trend
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

