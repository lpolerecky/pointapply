#' Combination plot
#'
#' \code{gg_effect} Combine raster images for ion count ratios with high
#' precision isotope ratios.
#'
#' @param ls Named list containing dataframes for planes of ion counts for
#' high precision isotope ratios.
#' @param ls_im Named list containing dataframes for planes of ion counts for
#' ion raster map.
#' @param ttl Character string or expression for plot title.
#' @param ratio A character string constituting the ion ratio, where the two
#' ions are separated by a dash ("12C14N-40Ca16O").
#' @param grid_print Logical whether to print the grid numbers
#' (default = FALSE).
#' @param viri Character string selecting the viriditas color scheme
#' (default = "A").
#' @param res Resolution of ion map in pixels (default = 256).
#' @param grid_cell Grid-cell size in pixels (default = 64).
#' @param scaler Numeric converting the pixels to metric dimension of
#' measurement (default is the conversion used in this study).
#'
#' @return \code{\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
gg_effect <- function(ls, ls_im, ttl, ratio, grid_print = FALSE, viri = "A",
                      res = 256, grid_cell = 64, scaler = 40 / 256,
                      Xt = "Xt.pr", grid = "grid.nm"){

  M_R <- rlang::parse_expr(paste("M_R", Xt, sep = "_"))
  GrM_R <- rlang::parse_expr(paste("M_R", grid, sep = "."))

  im <- dim_folds(ls_im, height, width, depth, ttl, "raster", res, grid_cell)

  # base rater image for ion ratios
  gg_base <- gg_cnts(
    im,
    stringr::str_split(ratio, "-", simplify = TRUE)[,1],
    stringr::str_split(ratio, "-", simplify = TRUE)[,2],
    viri = viri,
    height = res,
    width = res,
    grid_cell = grid_cell,
    scaler = scaler,
    compilation = TRUE
    )

  # dataframe for high precision R
  data <- dim_folds(ls, mean_height.mt, mean_width.mt, mean_depth.mt, ttl,
                    "tile", res, grid_cell)

  data <- mutate(
    data,
    del = (!!M_R /  !!GrM_R  - 1) * 1000,
    t_score = abs(del / RS_R_inter),
    sig_code = sig_coder(p_F, make_lab = FALSE),
    del_lab = paste(sprintf("%.1f", del), sig_code)
    ) %>%
    rename(x = mean_width.mt, y = mean_height.mt)

  gg_final <- function(data) {
    gg_base +
      geom_tile(
        data = data,
        aes(x = x, y = y),
        fill = "transparent",
        color = "black",
        size = 1,
        inherit.aes = FALSE
        ) +
      coord_fixed(clip = "off") +
      depth_arrows(res, grid_cell) +
      geom_text(
        data = data,
        aes(
          x = x,
          y = y,
          color = t_score,
          label = del_lab
          ),
        inherit.aes = FALSE
        ) +
      scale_color_distiller(
        expression("Inter-isotope var. Pr("*delta^13*"C (\u2030) > |t|)"),
        palette = "OrRd",
        breaks = sapply(c(0.1, 0.01, 0.001) / 2, qt, 16, lower.tail = FALSE),
        labels = c(0.1, 0.01, 0.001),
        direction = 1,
        ) +
      guides(color = guide_colorbar(title.position = "top")) +
      labs(
        title = ttl,
        subtitle =
          substitute(
            "Intra-isotope var. Pr("*delta^13*"C (\u2030) > |F|):"~a,
            list(a = miscutils::sig_coder())
            )
         ) +
      theme(
        legend.position = "top",
        rect = element_rect(
          fill = "transparent",
          color = "transparent"
          ),
        panel.background =  element_rect(
          fill = "transparent",
          color = "transparent"
          ),
        axis.line = element_blank()
        ) +
      themes_IC
  }

  grid_loc <- list(
    geom_text(
      data = data,
      aes(
        x = x + grid_cell * 0.375,
        y = y + grid_cell * 0.375,
        label = ROI
      ),
      size = 3,
      inherit.aes = FALSE
    )
  )

  if (grid_print) {
    suppressWarnings(return(list(data, {print(gg_final(data) + grid_loc)})))
    } else {
      suppressWarnings(return(list(data, print(gg_final(data)))))
      }
}
#' @rdname gg_effect
#'
#' @export
gg_sketch <- function(res = 256, grid_cell = 64, scaler = 40 / 256) {

  # dimensions
  dim_names <- c("height", "width", "depth")

  ls_sketch <- purrr::map(
    dim_names,
    ~grid_gen(c(256, 256, 400), rlang::parse_expr(.x), grid_cell, dim_names)
    ) %>%
    rlang::set_names(nm = dim_names) %>%
    purrr::map(flatten_matrix, var = "grid.nm") %>%
    purrr::map(~summarise(group_by(.x, grid.nm), across(.fns = mean), .groups = "drop")) %>%
    purrr::map(tibble::add_column, sample.nm = "sketch")

  df <- dim_folds(ls_sketch, height, width,  depth, "sketch", "tile", res, grid_cell)
  ggplot(df, aes(x = width, y = height)) +
    geom_tile(fill = "transparent", color = "black") +
    geom_text(aes(label = grid.nm)) +
    coord_fixed(clip = "off") +
    depth_arrows(res, grid_cell) +
    scale_y_continuous(
      expression("height"~"("*mu*"m)"),
      limits = c(res + grid_cell * 1.5 + 1, 0),
      trans = "reverse",
      breaks = seq(1, res + 1, grid_cell),
      labels =  function(x) (x - 1) * scaler,
      expand = c(0, 0)
      ) +
    scale_x_continuous(
      expression("width"~"("*mu*"m)"),
      limits = c(0, res + grid_cell * 1.5 + 1),
      breaks = seq(1, res + 1, grid_cell),
      labels = function(x) (x - 1) * scaler,
      expand = c(0, 0)
      ) +
    ggtitle("Grid layout") +
    theme_classic() +
    theme(
      legend.position = "top",
      rect = element_rect(
        fill = "transparent",
        color = "transparent"
      ),
      panel.background =  element_rect(
        fill = "transparent",
        color = "transparent"
      ),
      axis.line = element_blank()
    ) +
    themes_IC
}


#-------------------------------------------------------------------------------
# supportive functions
#-------------------------------------------------------------------------------


# Calculate distance for side plots (width and height aggregation plane)
dim_folds <- function(ls, dim1, dim2, dim3, ttl, geom, res, grid_cell){

  dim1 <- enquo(dim1)
  dim2 <- enquo(dim2)
  dim3 <- enquo(dim3)

  ls <- purrr::map(ls, ~filter(.x, sample.nm == ttl))

  if (geom == "tile") {
    ls <- purrr::map_at(
      ls,
      "height",
      ~{mutate(.x, !! dim1 := res + grid_cell) %>%
          select(-!! dim3)}
    ) %>%
      purrr::map_at(
        "width",
        ~{mutate(.x, !! dim2 := res + grid_cell) %>%
            select(-!! dim3)}
      )
  }

  if (geom == "raster") {
    ls <- purrr::map_at(
      ls,
      "height",
      ~{mutate(.x, !! dim1  := !! dim3 + res + grid_cell / 2) %>%
          select(-!! dim3)}
    ) %>%
      purrr::map_at(
        "width",
        ~{mutate(.x, !! dim2  := !! dim3 + res + grid_cell / 2) %>%
            select(-!! dim3)}
      )
  }

  bind_rows(ls)

}


depth_arrows <- function(res, grid_cell) {

  min_arrow <- res + grid_cell / 2
  max_arrow <- res + grid_cell * 1.5
  direction <- 1.035156 * res
  text_level <- 1.074219 * res

  list(
    annotate(
      "segment",
      x = min_arrow,
      xend = max_arrow,
      y = direction,
      yend = direction,
      arrow = arrow(length = unit(0.01, "npc"), type = "closed")
      ),
    annotate(
      "segment",
      x = direction,
      xend = direction,
      y = min_arrow,
      yend = max_arrow,
      arrow = arrow(length = unit(0.01, "npc"), type = "closed")
      ),
    annotate("text", x = res + grid_cell, y = text_level, label = "depth"),
    annotate("text", x = text_level, y = res + grid_cell, label = "depth", angle = 90)
    )
}
