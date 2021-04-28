#' In-depth analysis of one grid_cell
#'
#' \code{gg_effect} This plot function is used to produce the scatterplots of
#' Figure 9 and Supplementary Figure 8.
#'
#' @param IC Ion count data.
#' @param image Data frame for ion raster map.
#' @param ion1_thr Character string for ion in the enumerator of ion ratio of
#' the map.
#' @param ion2_thr Character string for ion in the denominator of ion ratio of
#' the map.
#' @param thr Numeric threshold value for filter selection.
#' @param ion1_R A character string constituting the rare isotope ("13C").
#' @param ion2_R A character string constituting the common isotope ("12C").
#' @param Xt Variable for ion count rates (default = Xt.pr).
#' @param N Variable for ion counts (default = N.rw).
#' @param colors Color palette to identify matrix and inclusion (default =
#' \code{ c("#FFEDA0", "#FEB24C")}).
#'
#' @return \code{ggplot2::\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
gg_point <- function(IC, image, ion1_thr, ion2_thr, thr, ion1_R, ion2_R,
                     colors = c("#FFEDA0", "#FEB24C"), .X = Xt.pr, .N = N.pr,
                     .species = species.nm, .t = t.nm) {
  # unique ions
  ions <- unique(c(ion1_thr, ion2_thr, ion1_R, ion2_R))
  # user-supplied variables
  args <- enquos(.X = .X, .N = .N, .species = .species, .t = .t)
  # variables for threshold ion ratio
  args_thr <- arg_builder(args, "X")
  # variables for isotope ratio
  args_R <- all_args(args, ion1_R, ion2_R, chr = FALSE)
  # ions
  args_ion <- purrr::map(ions, ~quo_updt(args[[".X"]], post = .x)) %>%
    rlang::set_names(nm = ions)

  N.rw <- quo_updt(args[[".N"]], post = "rw", update_post = TRUE)

  # ratio bounds
  lower1 <- quo_updt(args[[".X"]], pre = "lower", post = ion1_R)
  lower2 <- quo_updt(args[[".X"]], pre = "lower", post = ion2_R)
  upper1 <- quo_updt(args[[".X"]], pre = "upper", post = ion1_R)
  upper2 <- quo_updt(args[[".X"]], pre = "upper", post = ion2_R)

  im_ROI <-filter(
    image,
    .data$width %in% unique(IC$width),
    .data$height %in% unique(IC$height)
    )  %>%
    point::cov_R(ions, height, width, grid.nm, sample.nm) %>%
    summarise(
      height = .data$height,
      width = .data$width,
      R_depth = !! args_ion[[ion1_thr]] / !! args_ion[[ion2_thr]],
      flag = if_else(.data$R_depth >= thr, "inclusion", "matrix"),
      ntot = n()
      ) %>%
    group_by(flag) %>%
    # pixels for domains and fractions
    mutate(
      pxl = n_distinct(.data$height, .data$width),
      frac = .data$pxl / .data$ntot
      ) %>%
    ungroup()

  # combine image and counts
  tb_inc <- left_join(IC, im_ROI, by = c("width", "height")) %>%
    group_by(.data$sample.nm, .data$species.nm, .data$flag, .data$depth) %>%
    summarise(
      pxl = unique(.data$pxl),
      frac = unique(.data$frac),
      !! args[[".N"]] := sum(!! N.rw),
      .groups = "drop"
      ) %>%
    # count rates
    mutate(!! args[[".X"]] := !! args[[".N"]] / (.data$pxl * 1e-3))

  # sum stats
  tb_X <- point::stat_X(tb_inc, sample.nm, flag, frac, .t = depth) %>%
    group_by(.data$flag, .data$species.nm) %>%
    mutate(
      t.nm = row_number(),
      "lower_{{.X}}" :=
        if_else(
          !! args_thr[["M_X"]] - 2 * !! args_thr[["S_X"]] < 0,
          0,
          !! args_thr[["M_X"]] - 2 * !! args_thr[["S_X"]]
          ),
      "upper_{{.X}}" := !! args_thr[["M_X"]] + 2 * !! args_thr[["S_X"]]
      ) %>%
    ungroup()

  # label names
  tb_wide <- point::cov_R(tb_X, ions, sample.nm, flag, frac) %>%
    mutate(
      flag = paste(
        .data$flag,
        paste("(f = ", sprintf(fmt = "%.3f", .data$frac), ")")
        )
      )

  # ranges
  range_x <- c(0, max(pull(tb_wide, !! upper2)))
  range_y <- c(0, max(pull(tb_wide, !! upper1)))

  # plot title
  ttl <- substitute(
    a~"("*b > c *")" ,
    list(
      a = unique(tb_wide$sample.nm),
      b = point::R_labeller(ion1_thr, ion2_thr, "expr"), c =  thr)
    )

  # labels for ion count rates
  xlab <- substitute(
    a ~ "(ct sec"^"-"*")",
    list(a = point::ion_labeller(ion2_R, "expr"))
    )
  ylab <- substitute(
    a ~ "(ct sec"^"-"*")",
    list(a = point::ion_labeller(ion1_R, "expr"))
    )

  ggplot(
    tb_wide,
    aes(x = !! args_R[["M_X2"]], y = !! args_R[["M_X1"]], fill = .data$flag)
    ) +
    ggplot2::geom_errorbar(
      aes(ymin = ifelse(!! lower1, !! lower1, 0), ymax = !! upper1),
      width = 0
      ) +
    ggplot2::geom_errorbarh(
      aes(xmin = !! lower2, xmax = !! upper2),
      height = 0
      ) +
    geom_point(shape = 21, size = 5) +
    ggtitle(ttl) +
    scale_y_continuous(
      ylab,
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      limits = range_y
      ) +
    scale_x_continuous(
      xlab,
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      limits = range_x
      ) +
    ggplot2::scale_fill_manual("", values = colors) +
    themes_IC(base = ggplot2::theme_bw())
  }
