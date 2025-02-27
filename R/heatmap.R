#' Heatmaps and summary plots for model performance
#'
#' \code{heat_map} generates a heatmap for model accuracy evaluation along
#' two continuous variables and up to two categorical values. Instead
#' \code{heat_sum} summarise the heatmap over opne of the two continuous and
#' crossed variables.
#'
#' @param simu Performance for test statistics on intra-analysis isotope
#'  variability ("intra") or inter-analysis isotope variability ("inter").
#' @param x Variable (continuous variable number one) isotope offset in paper.
#' @param y Variable (continuous variable number two) excess ionization
#'  efficiency in paper.
#' @param stat Numeric for the model accuracy (between 0 and 1).
#' @param grp1 Nominal factorial number one; outlier detection method in paper.
#' @param grp2 Nominal factorial number one; for intra-isotope variability in
#'  paper.
#' @param conversion Named vector for the conversion of the ionization trend to
#'  excess ionization efficiency.
#' @param ttl Character string or expression for the plot title.
#' @param x_lab Character string or expression for the x-axis label.
#' @param y_lab Character string or expression for the y-axis label.
#' @param x_sec Character string or expression for the secondary x-axis label.
#' @param trans_base Isotope value for the scale transformation for fractional
#'  size of the isotope anomaly.
#' @param trans_n Initial sample size for the scale transformation for
#'  fractional size of the isotope anomaly.
#' @param save Save the produced plot.
#'
#' @return \code{\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
heat_map <- function(simu, x, y, stat, grp1, grp2, conversion, ttl, x_lab,
                     y_lab, x_sec = NULL, trans_base = -2, trans_n = 10,
                     save = FALSE) {

  # load simulated data (intra-analysis isotope variability)
  sens <- load_point("simu", "sens_IC_intra", return_name = TRUE) |>
    rlang::sym()

  if (simu == "intra") {

    # load test statistics (intra-analysis isotope variability)
    CD <- load_point("simu", "CooksD_IC_intra", return_name = TRUE) |>
      rlang::sym()
    CM <- load_point("simu", "Cameca_IC_intra", return_name = TRUE) |>
      rlang::sym()

    # bind the two diagnostic treatments of simulated ion (Cameca and Cooks D)
    IC  <- rlang::inject(list(!!CM, !!CD)) |>
      rlang::set_names(nm = c("Cameca", "CooksD")) |>
      dplyr::bind_rows(.id = "stat.nm")

    # calculate accuracy of model classification
    IC <- dplyr::group_by(IC, .data$stat.nm, .data$type.nm, .data$trend.nm,
                          .data$force.nm) %>%
      dplyr::summarise(
        accuracy =
          acc_fun(unique(.data$force.nm), .data$p_R_Xt.sm, ntot = dplyr::n()),
        .groups = "drop"
      )

    # rename methods, make nice labels
    IC <- dplyr::mutate(
      IC,
      stat.nm =
        factor(
          .data$stat.nm,
          levels = c("Cameca", "CooksD"),
          labels = c(sigma[R]~"rejection", "Cooks~D")
        )
    )

  } else if (simu == "inter") {

    # load test statistics (intra-analysis isotope variability)
    inter <- load_point("simu", "nlme_IC_inter", return_name = TRUE) |>
      rlang::sym()

    # calculate accuracy of model classification
    IC <- dplyr::group_by(eval(inter), .data$trend.nm, .data$execution,
                          .data$type.nm, .data$anomaly.nm) |>
      dplyr::summarise(
        accuracy = acc_fun(unique(.data$anomaly.nm), .data$p_M_R_Xt.sm, ntot = dplyr::n()),
        .groups = "drop"
      )

  }

  # statistics on the single common isotope
  X <- point::stat_X(
    eval(sens),
    .data$trend.nm,
    .N = rlang::sym("N.sm"),
    .X = rlang::sym("Xt.sm"),
    .stat = c("RS", "hat_RS")
  ) |>
    filter(.data$species.nm == "12C")

  # named vector for secondary axis
  conversion <- set_names(
    X$trend.nm,
    nm = sprintf("%.0f", X$RS_Xt.sm - X$hat_RS_N.sm)
  )

  # grouping
  grp1 <- enquo(grp1)
  grp2 <- enquo(grp2)

  # create rectangle annotation
  rcts <- rect_fun(
    xaxis = dplyr::pull(IC, {{x}}),
    yaxis = dplyr::pull(IC, {{y}})
  )

  # transformation of secondary axis
  if (!is.null(x_sec)) {

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
  p <- ggplot2::ggplot(
    data = IC,
    mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, fill = {{ stat }})
  ) +
    ggplot2::geom_tile()

  # facets
  if (!all(purrr::map_lgl(list(grp1, grp2), ~is.null(rlang::get_expr(.x))))) {
    p <- p + ggplot2::facet_grid(
      rows = vars(!! grp1),
      cols = vars(!! grp2),
      labeller = ggplot2::label_parsed
    )
  }

  p <- p + ggplot2::scale_x_continuous(
      labels = scales::label_number(),
      expand = c(0, 0),
      # secondary x axis
      sec.axis = second_axis
    ) +
    ggplot2::scale_y_continuous(
      breaks = unname(conversion),
      labels = names(conversion),
      expand = c(0, 0)
      ) +
    ggplot2::scale_fill_distiller(
      "classification \n accuracy", palette = "YlOrRd"
    ) +
    ggplot2::coord_fixed(ratio = rcts$r_cell[1]) +
    ggplot2::geom_rect(
      data = rcts,
      aes(
        xmin = .data$xl,
        xmax = .data$xu,
        ymin = .data$yl,
        ymax = .data$yu,
        color = .data$col_type
        ),
      fill = "transparent",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_size(range = c(0.5, 1.5)) +
    ggplot2::scale_color_identity() +
    ggrepel::geom_text_repel(
      data = rcts,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$lbl),
      inherit.aes = FALSE,
      size = 3,
      min.segment.length = 1e-3,
      nudge_x = -7,
      nudge_y = 25
    ) +
    ggplot2::labs(title = ttl, x = x_lab, y = y_lab) +
    themes_IC(base = ggplot2::theme_bw())

  if (isTRUE(save)) {
    # name
    nm <- paste0("heat_sens_", simu)
    # dimensions
    if (simu == "intra") {
      width <- 16
      height <- 14
    } else if (simu == "inter") {
      width <- 12
      height <- 10.5
    }
    # save
    save_point(nm, p, width = width, height = height, unit = "cm")

    message(
      paste0("The performance of the intra-analysis isotope variability test is",
      " saved with name: '",nm , "'.")
    )
  }
  # and return
  p
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
    lbl = c("specificity", "sensitivity", "stable ionization"),
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

  scales::trans_new(
    paste0("rayleigh", format(base), "-",format(frac)),
    rayleigh_t,
    rayleigh_i
    )

}

# this holds the cut-off p value used in the study
acc_fun <- function(x, y, ntot) {
  if(x != 0) return(sum(y < 0.001) / ntot)
  if(x == 0) return(1 - (sum(y < 0.001) / ntot))
}

