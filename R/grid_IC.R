#' Combination plot
#'
#' \code{gg_effect} combines raster images for ion count ratios with high
#'  precision isotope ratios. \code{gg_sketch} draws the layout of the
#'  grid_cells.
#'
#' @param title Character string for the analyte ("MEX" or "MON") to be used.
#' @param ratio A character string constituting the ion ratio, where the two
#'  ions are separated by a dash ("12C14N-40Ca16O").
#' @param grid_print Logical whether to print the grid numbers
#'  (default = FALSE).
#' @param viri Character string selecting the viriditas colour scheme
#'  (default = "A").
#' @param res Resolution of ion map in pixels (default = 256).
#' @param grid_cell Grid-cell size in pixels (default = 64).
#' @param scaler Numeric converting the pixels to metric dimension of
#'  measurement (default is the conversion used in this study).
#' @param label Character string indicating how to name tibble columns
#'  (default = "latex").
#' @param .X Variable for ion count rates (default = Xt.pr).
#' @param .N Variable for ion counts (default = N.pr).
#' @param .species Variable for species names (default = species.nm).
#' @param .t Variable for time increment (default = t.nm).
#' @param .ion1 Character string for rare isotope (default = "13C").
#' @param .ion2 Character string for common isotope (default = "12C").
#' @param save Boolean whether to save the plot as an png.
#'
#' @return \code{\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
gg_effect <- function(title, ratio, grid_print = FALSE, viri = "A",
                      res = 256, grid_cell = 64, scaler = 40 / 256,
                      label = "latex", save = FALSE, .X = NULL, .N = NULL,
                      .species = NULL, .t = NULL, .ion1 = "13C",
                      .ion2 = "12C") {

  # grid data
  IC <- load_point("map_sum_grid", title, grid_cell, return_name = TRUE)  |>
    rlang::sym()

  # quoting the call (user-supplied expressions) and complete if NULL
  args <- rlang::inject(
    point:::inject_args(
      !!IC,
      enquos(.X = .X, .N = .N, .species = .species, .t = .t),
      type = c("processed", "group")
    )
  )

  # new args
  args <- point:::arg_builder(
    point:::all_args(args, .ion1, .ion2, chr = FALSE),
    "model"
  )

  # diagnostics
  IC <- rlang::inject(
    point::diag_R(!!IC, .ion1, .ion2, dim_name.nm, sample.nm, file.nm,
                  grid.nm, .nest = grid.nm, .output = "complete", .meta = TRUE)
  )

  # base raster image for ion ratios
  ions <- strsplit(ratio, "-")
  gg_base <- gg_cnts(
    title,
    ions[[1]][1],
    ions[[1]][2],
    viri = viri,
    res = res,
    grid_cell = grid_cell,
    scaler = scaler,
    compilation = TRUE
  )

  # unfold metadata and select distinct grid-cell per plane
  meta <- point::unfold(IC, merge = FALSE) |>
    dplyr::distinct(.data$dim_name.nm, .data$grid.nm, .keep_all = TRUE) |>
    dplyr::select(.data$depth.mt, .data$width.mt, .data$height.mt)
  # select grid-cell per plane for IC
  IC <- dplyr::distinct(IC, .data$dim_name.nm, .data$grid.nm, .keep_all = TRUE)
  # merge IC and meta
  IC <- dplyr::bind_cols(IC, meta)

  # reduce dimensions to 2D grid
  IC <- dim_folds(IC, "grid", res, grid_cell)

  # convert stats for plotting
  IC <- dplyr::mutate(
    IC,
    # delta notation of R
    del = (!! args[["M_R"]] /  !! args[["hat_M_M_R"]]  - 1) * 1000,
    # t score grid respective to whole plane
    t_score = abs(.data$del / !! args[["hat_RS_M_R"]]),
    # significance code
    sig_code = sig_coder(!! args[["p_R"]], make_lab = FALSE),
    # label of delta notation
    del_lab = paste(sprintf("%.1f", .data$del), .data$sig_code)
  )

  # plotting
  p <- gg_base +
    ggplot2::geom_tile(
      data = IC,
      mapping = ggplot2::aes(x = .data$width.mt, y = .data$height.mt),
      fill = "transparent",
      color = "black",
      size = 1,
      inherit.aes = FALSE
    ) +
    # isotope values
    ggplot2::geom_text(
      data = IC,
      mapping = aes(
        x = .data$width.mt,
        y = .data$height.mt,
        color = .data$t_score,
        label = .data$del_lab
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::scale_color_distiller(
      expression("Inter-analysis isotope var."~italic(p)),
      palette = "OrRd",
      breaks = sapply(c(0.1, 0.01, 0.001) / 2, qt, 16, lower.tail = FALSE),
      labels = c(0.1, 0.01, 0.001),
      direction = 1
    ) +
    ggplot2::guides(color = ggplot2::guide_colorbar(title.position = "top")) +
    ggplot2::labs(
      title = title,
      subtitle =
        substitute(
          "Intra-analysis isotope var."~italic(p)*":"~a,
          list(a = sig_coder())
          )
    ) +
    themes_IC() +
    ggplot2::theme(axis.line = ggplot2::element_blank())

  # whether to return latex parsable labels
  if (! is.null(label)) {

    # latex
    tb_model <- filter(
      point::names_model,
      .data$type == "Restricted Maximum Likelihood optimization"
    )
    # Model args augment
    model_args <- args[paste(tb_model$name, tb_model$derived, sep = "_")]
    ls_latex <- set_names(
      sapply(model_args, as_name),
      point:::tex_labeller(tb_model, tb_model$name, label)
    )

  } else {
    ls_latex <- sapply(model_args, as_name)
  }

  # should grid numbering be plotted ?
  if (isTRUE(grid_print)) {

    # print plot with grid numbering
    p <- p + ggplot2::geom_text(
      data = IC,
      mapping = ggplot2::aes(
        x = .data$width.mt + grid_cell * 0.375,
        y = .data$height.mt + grid_cell * 0.375,
        label = data$grid.nm
      ),
      size = 3,
      inherit.aes = FALSE
    )
  }

  # distill data
  IC <- dplyr::select(IC, .data$dim_name.nm, !!! ls_latex) |>
    dplyr::distinct()

  if (isTRUE(save)) {
    # save plot
    nm <- paste("raster", title, ratio, sep = "_")
    save_point(nm, p, width = 16, height = 14, unit = "cm")

    message(
      paste0("Effects plot has been saved with name ", nm, " .")
    )

    # save diagnostics
    nm <- paste("diag", grid_cell, title, sep = "_")
    write_point(IC, nm)

    message(
      paste0("Diagnostics has been saved with name ", nm, " .")
    )
  }

  # print plot
  print(p)

  # return data
  IC
}
#' @rdname gg_effect
#'
#' @export
gg_sketch <- function(grid_cell = 64, res = 256, scaler = 40 / 256, save) {

  # dimensions
  dims <- c("height" = res, "width" = res, depth = 1)
  # grid numbers
  numb <- purrr::map(
    names(dims),
    ~c(kronecker_subsample_grid(
      dims,
      .x,
      grid_cell,
      expand = FALSE
    ))
  ) |>
    rlang::set_names(names(dims))

  # dimension names
  nms <- purrr::map(names(dims), ~rlang::rep_along(numb[[.x]], .x))

  # grid positions
  pos <- purrr::map_dfr(
    names(dims),
    ~c(grid_positions(
      dims,
      .x,
      grid_cell,
      expand = FALSE
    ))
  )

  # combine
  sketch <- tibble::tibble(
    grid.nm = purrr::flatten_int(numb),
    dim_name.nm = purrr::flatten_chr(nms),
    !!pos,
    sample.nm = "sketch"
  )

  fold <- dim_folds(sketch, "grid", res, grid_cell)

  p <- ggplot2::ggplot(fold, ggplot2::aes(x = .data$width.mt, y = .data$height.mt)) +
    ggplot2::geom_tile(fill = "transparent", color = "black") +
    ggplot2::geom_text(aes(label = .data$grid.nm)) +
    ggplot2::coord_fixed(clip = "off") +
    depth_arrows(res, grid_cell) +
    ggplot2::scale_y_continuous(
      expression("height"~"("*mu*"m)"),
      trans = "reverse",
      breaks = seq(0, res, grid_cell),
      labels =  function(x) x * scaler,
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      expression("width"~"("*mu*"m)"),
      breaks = seq(0, res, grid_cell),
      labels = function(x) x * scaler,
      expand = c(0, 0)
    ) +
    ggplot2::ggtitle("Grid layout") +
    ggplot2::theme(axis.line = ggplot2::element_blank()) +
    themes_IC(base = ggplot2::theme_void())

  if (isTRUE(save)) {
    # save plot
    save_point("grid_sketch", p, width = 16, height = 14, unit = "cm")

    message(
      paste0("Grid layout figure has been saved with name `grid_sketch`.")
    )
  }
}

#-------------------------------------------------------------------------------
# supportive functions
#-------------------------------------------------------------------------------
# Calculate distance for side plots (width and height aggregation plane). This
# function collapses the third dimension (`dim3`) to a 2D object.
# res = resolution and geom = geometry for ggplot object
dim_folds <- function(IC, geom, res, grid_cell){

  # the mutation differs depending on the chosen geom
  calcs <- rlang::exprs(
    grid = as.numeric(res) + as.numeric(grid_cell),
    raster = .data$depth.mt + as.integer(res) +  as.integer(grid_cell / 2)
  )

  # transform 3rd dimension into 2D grid and finally remove the 3rd dimension
  dplyr::mutate(
    IC,
    height.mt =
      dplyr::if_else(.data$dim_name.nm == "height", !! calcs[[geom]],
                     .data$height.mt),
    width.mt =
      dplyr::if_else(.data$dim_name.nm == "width", !! calcs[[geom]],
                     .data$width.mt)
  ) |>
    dplyr::select(-.data$depth.mt)

}

# placing arrows for depth
depth_arrows <- function(res, grid_cell) {

  min_arrow <- res + grid_cell / 2
  max_arrow <- res + grid_cell * 1.5
  direction <- 1.035156 * res
  text_level <- 1.074219 * res

  list(
    ggplot2::annotate(
      "segment",
      x = min_arrow,
      xend = max_arrow,
      y = direction,
      yend = direction,
      arrow = ggplot2::arrow(
        length = ggplot2::unit(0.01, "npc"),
        type = "closed"
        )
      ),
    ggplot2::annotate(
      "segment",
      x = direction,
      xend = direction,
      y = min_arrow,
      yend = max_arrow,
      arrow = ggplot2::arrow(
        length = ggplot2::unit(0.01, "npc"),
        type = "closed"
        )
      ),
    ggplot2::annotate(
      "text",
      x = res + grid_cell,
      y = text_level,
      label = "depth"
      ),
    ggplot2::annotate(
      "text",
      x = text_level,
      y = res + grid_cell,
      label = "depth",
      angle = 90
      )
    )
}
