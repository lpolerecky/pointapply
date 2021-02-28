#' Combination plot
#'
#' \code{gg_effect} Combine raster images for ion count ratios with high
#' precision isotope ratios.
#'
#' @param IC Ion count data.
#' @param grid_sel Integer selecting an grid-cell for in-depth analysis.
#' @param image Dataframe for ion raster map.
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
gg_point <- function(IC, image, ion1_thr, ion2_thr, thr,
                     ion1_R, ion2_R, Xt = Xt.pr, N = N.rw,
                     colors = c("#FFEDA0", "#FEB24C")) {
  # unique ions
  ions <- unique(c(ion1_thr, ion2_thr, ion1_R, ion2_R))
  Xt <- enquo(Xt) # of the ion image
  N <- enquo(N) # raw count of the full stack
  args <- purrr::map(
    ions,
    ~rlang::parse_expr(paste(rlang::as_name(Xt), .x , sep = "."))
    ) %>%
    rlang::set_names(nm = ions)
  M_Xt <- rlang::parse_expr(paste("M", rlang::as_name(Xt), sep = "_"))
  S_Xt <- rlang::parse_expr(paste("S", rlang::as_name(Xt), sep = "_"))
  M_Xt1 <- rlang::parse_expr(paste("M", rlang::as_name(args[[ion1_R]]), sep = "_"))
  M_Xt2 <- rlang::parse_expr(paste("M", rlang::as_name(args[[ion2_R]]), sep = "_"))
  lower1 <- rlang::parse_expr(paste("lower", ion1_R , sep = "."))
  lower2 <- rlang::parse_expr(paste("lower", ion2_R , sep = "."))
  upper1 <- rlang::parse_expr(paste("upper", ion1_R , sep = "."))
  upper2 <- rlang::parse_expr(paste("upper", ion2_R , sep = "."))

  im_ROI <-filter(
    image,
    width %in% unique(IC$width),
    height %in% unique(IC$height)
    )  %>%
    point::cov_R(ions, height, width, grid.nm, sample.nm) %>%
    summarise(
      height = height,
      width = width,
      R_depth = !!args[[ion1_thr]] / !!args[[ion2_thr]],
      flag = if_else(R_depth >= thr, "inclusion", "matrix"),
      ntot = n()
      ) %>%
    group_by(flag) %>%
    mutate(
      pxl = n_distinct(height, width),
      frac = pxl / ntot
      ) %>%
    ungroup # pixels for domains and fraction

  # combine image and counts
  tb_inc <- left_join(IC, im_ROI, by = c("width", "height")) %>%
    group_by(sample.nm, species.nm, flag, depth) %>%
    summarise(
      pxl = unique(pxl),
      frac = unique(frac),
      N.pr = sum(!!N),
      .groups = "drop"
      ) %>%
    # count rates
    mutate(Xt.pr = N.pr / (pxl * 1e-3))

  # sum stats
  tb_Xt <- point::stat_Xt(tb_inc, sample.nm, flag, frac, .t = depth) %>%
    group_by(flag, species.nm) %>%
    mutate(
      t.nm = row_number(),
      lower = !!M_Xt - !!S_Xt,
      upper = !!M_Xt + !!S_Xt
      ) %>%
    ungroup()

  # label names
  tb_wide <- cov_R(tb_Xt, ions, sample.nm, flag, frac) %>%
    mutate(
      flag = paste(flag, paste("(f = ", sprintf(fmt = "%.3f", frac), ")"))
      )

  # ranges
  range_x <- c(0, max(pull(tb_wide, !!upper2)))
  range_y <- c(0, max(pull(tb_wide, !!upper1)))

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

  ggplot(tb_wide, aes(x = !!M_Xt2, y = !!M_Xt1, fill = flag)) +
    geom_errorbar(
      aes(ymin = ifelse(!!lower1, !!lower1, 0), ymax = !!upper1),
      width = 0
      ) +
    geom_errorbarh(
      aes(xmin = !!lower2, xmax = !!upper2),
      height = 0
      ) +
    geom_point(shape = 21, size = 5) +
    ggtitle(ttl) +
    scale_y_continuous(
      ylab,
      expand = expansion(mult = c(0,0.1)),
      limits = range_y
      ) +
    scale_x_continuous(
      xlab,
      expand = expansion(mult = c(0,0.1)),
      limits = range_x
      ) +
    scale_fill_manual("", values = colors) +
    themes_IC +
    theme_bw()
}
