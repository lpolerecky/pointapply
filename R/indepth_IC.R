#' In-depth analysis of IC data cubes
#'
#' \code{gg_point} This plot function is used to produce the scatter plots of
#' Figure 8 and Supplementary Figure 9.
#'
#' @param title Character string for the analyte ("MEX" or "MON") to be used.
#' @param ion1_thr Character string for ion in the enumerator of ion ratio of
#'  the map.
#' @param ion2_thr Character string for ion in the denominator of ion ratio of
#'  the map.
#' @param thr Numeric threshold value for filter selection.
#' @param ion1_R A character string constituting the rare isotope ("13C").
#' @param ion2_R A character string constituting the common isotope ("12C").
#' @param colors Colour palette to identify matrix and inclusion (default =
#'  \code{ c("#8E063B", "#023FA5")}).
#' @param .X Variable for ion count rates (default = NULL).
#' @param .N Variable for ion counts (default = NULL).
#' @param .species Variable for species names (default = NULL).
#' @param .t Variable for time increment (default = NULL).
#'
#' @return \code{ggplot2::\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
gg_point <- function(title, grid_cell, ion1_thr, ion2_thr, thr, ion1_R, ion2_R,
                     colors = c("#8E063B", "#023FA5"), .X = NULL, .N = NULL,
                     .species = NULL, .t = NULL) {

  # raster data
  IC <-  load_point("map_full_grid_64", title, return_name = TRUE) |>
    rlang::sym()
  # filter grid_cell
  IC <- dplyr::filter(eval(IC), .data$grid.nm == grid_cell)
  # correct ion counts
  IC <- point::cor_IC(IC, .bl_t = 0, .det = "EM", .hide = FALSE)

  image <- load_point("map_raster_image", title, return_name = TRUE) |>
    rlang::sym()
  # correct ion counts
  image <- point::cor_IC(eval(image), .bl_t = 0, .det = "EM", .hide = FALSE)

  # unique ions
  ions <- unique(c(ion1_thr, ion2_thr, ion1_R, ion2_R))
  # Quoting the call (user-supplied expressions) and complete if NULL
  args <- point:::inject_args(
    IC,
    enquos(.X = .X, .N = .N, .species = .species, .t = .t),
    type = c("processed", "group")
  )
  # variables for threshold ion ratio
  args_thr <- point:::arg_builder(args, "X")
  # variables for isotope ratio
  args_R <- point:::all_args(args, ion1_R, ion2_R, chr = FALSE)
  # ions
  args_ion <- purrr::map(ions, ~point:::quo_updt(args[[".X"]], post = .x)) %>%
    rlang::set_names(nm = ions)

  N.rw <- point:::quo_updt(args[[".N"]], post = "rw", update_post = TRUE)

  # ratio bounds
  lower1 <- point:::quo_updt(args[[".X"]], pre = "lower", post = ion1_R)
  lower2 <- point:::quo_updt(args[[".X"]], pre = "lower", post = ion2_R)
  upper1 <- point:::quo_updt(args[[".X"]], pre = "upper", post = ion1_R)
  upper2 <- point:::quo_updt(args[[".X"]], pre = "upper", post = ion2_R)

  # region of interest on image
  im_ROI <- dplyr::filter(
    image,
    # which species
    species.nm %in% ions,
    # ROI
    .data$width.mt %in% unique(IC$width.mt),
    .data$height.mt %in% unique(IC$height.mt)
    )  %>%
    # wide format conversion
    point::cov_R(ions, .data$height.mt, .data$width.mt, .data$grid.nm,
                 .data$sample.nm) %>%
    dplyr::summarise(
      height.mt = .data$height.mt,
      width.mt = .data$width.mt,
      R_depth = !! args_ion[[ion1_thr]] / !! args_ion[[ion2_thr]],
      flag = dplyr::if_else(.data$R_depth >= thr, "inclusion", "matrix"),
      ntot = dplyr::n()
    ) %>%
    dplyr::group_by(.data$flag) %>%
    # pixels for domains and fractions
    dplyr::mutate(
      pxl = dplyr::n_distinct(.data$height.mt, .data$width.mt),
      frac = .data$pxl / .data$ntot
    ) %>%
    dplyr::ungroup()

  # combine image and counts
  tb_inc <- dplyr::left_join(IC, im_ROI, by = c("width.mt", "height.mt")) %>%
    dplyr::group_by(
      .data$sample.nm,
      .data$species.nm,
      .data$flag,
      .data$depth.mt
    ) %>%
    dplyr::summarise(
      pxl = unique(.data$pxl),
      frac = unique(.data$frac),
      !! args[[".N"]] := sum(!! N.rw),
      .groups = "drop"
      ) %>%
    # count rates
    dplyr::mutate(!! args[[".X"]] := !! args[[".N"]] / (.data$pxl * 1e-3))

  # sum stats
  tb_X <- point::stat_X(tb_inc, .data$sample.nm, .data$flag, .data$frac,
                        .t = .data$depth.mt) %>%
    dplyr::group_by(.data$flag, .data$species.nm) %>%
    dplyr::mutate(
      t.nm = dplyr::row_number(),
      "lower_{{.X}}" :=
        dplyr::if_else(
          !! args_thr[["M_X"]] - 2 * !! args_thr[["S_X"]] < 0,
          0,
          !! args_thr[["M_X"]] - 2 * !! args_thr[["S_X"]]
        ),
      "upper_{{.X}}" := !! args_thr[["M_X"]] + 2 * !! args_thr[["S_X"]]
    ) %>%
    dplyr::ungroup()

  # label names
  tb_wide <- point::cov_R(tb_X, ions, sample.nm, flag, frac) %>%
    mutate(component = if_else(.data$flag == "inclusion", "B", "A"))
  labels <- purrr::pmap(
    list(a = tb_wide$flag, b = tb_wide$component, c = tb_wide$frac),
    function(a, b, c) {
      substitute(
        a ~"("*italic(f)[b] == c*")",
        list(a = a , b = b, c = sprintf(fmt = "%.3f", c))
      )
    }
  )

  # ranges
  range_x <- c(0, max(dplyr::pull(tb_wide, !! upper2)))
  range_y <- c(0, max(dplyr::pull(tb_wide, !! upper1)))

  # plot title
  ttl <- substitute(
    a~"("*b > c *")" ,
    list(
      a = unique(tb_wide$sample.nm),
      b = point::R_labeller(ion1_thr, ion2_thr, "expr"), c =  thr)
    )

  # labels for ion count rates
  xlab <- substitute(
    a ~ "(count sec"^"-"*")",
    list(a = point::ion_labeller(ion2_R, "expr"))
    )
  ylab <- substitute(
    a ~ "(count sec"^"-"*")",
    list(a = point::ion_labeller(ion1_R, "expr"))
    )

  ggplot2::ggplot(
    tb_wide,
    ggplot2::aes(
      x = !! args_R[["M_X2"]],
      y = !! args_R[["M_X1"]],
      fill = .data$flag
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ifelse(!! lower1, !! lower1, 0), ymax = !! upper1),
      width = 0
    ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = !! lower2, xmax = !! upper2),
      height = 0
    ) +
    ggplot2::geom_point(shape = 21, size = 5) +
    ggplot2::ggtitle(ttl) +
    ggplot2::scale_y_continuous(
      ylab,
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      limits = range_y,
      labels = scales::label_scientific(2),
      breaks = scales::pretty_breaks(3)
    ) +
    ggplot2::scale_x_continuous(
      xlab,
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      limits = range_x,
      labels = scales::label_scientific(2),
      breaks = scales::pretty_breaks(3)
    ) +
    ggplot2::scale_fill_manual("", values = colors, labels = labels) +
    ggplot2::themes_IC(base = ggplot2::theme_bw()) +
    ggplot2::theme(legend.direction = "vertical")
  }
