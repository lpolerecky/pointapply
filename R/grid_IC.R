#' Combination plot
#'
#' \code{gg_effect} combines raster images for ion count ratios with high
#' precision isotope ratios. \code{gg_sketch} draws the layout of the
#' grid_cells.
#'
#' @param ls Named list containing data frames for planes of ion counts for
#' high precision isotope ratios.
#' @param ls_im Named list containing data frames for planes of ion counts for
#' ion raster map.
#' @param ttl Character string or expression for plot title.
#' @param ratio A character string constituting the ion ratio, where the two
#' ions are separated by a dash ("12C14N-40Ca16O").
#' @param grid_print Logical whether to print the grid numbers
#' (default = FALSE).
#' @param viri Character string selecting the viriditas colour scheme
#' (default = "A").
#' @param res Resolution of ion map in pixels (default = 256).
#' @param grid_cell Grid-cell size in pixels (default = 64).
#' @param scaler Numeric converting the pixels to metric dimension of
#' measurement (default is the conversion used in this study).
#' @param label Character string indicating how to name tibble columns
#' (default = "latex").
#' @param .X Variable for ion count rates (default = Xt.pr).
#' @param .N Variable for ion counts (default = N.pr).
#' @param .species Variable for species names (default = species.nm).
#' @param .t Variable for time increment (default = t.nm).
#' @param .ion1 Character string for rare isotope (default = "13C").
#' @param .ion2 Character string for common isotope (default = "12C").
#'
#' @return \code{\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
gg_effect <- function(ls, ls_im, ttl, ratio, grid_print = FALSE, viri = "A",
                      res = 256, grid_cell = 64, scaler = 40 / 256,
                      label = "latex", .X = Xt.pr, .N = N.pr,
                      .species = species.nm, .t = t.nm, .ion1 = "13C",
                      .ion2 = "12C"){

  args <- enquos(.X = .X, .N = .N, .species = .species, .t = .t)
  args <- arg_builder(all_args(args, .ion1, .ion2, chr = FALSE), "model")

    im <- dim_folds(ls_im, height, width, depth, ttl, "raster", res, grid_cell)

  # base raster image for ion ratios
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

  # unfold metadata
  ls <- purrr::map(ls, point::unfold)
  # data frame for high precision R
  IC <- dim_folds(ls, mean_height.mt, mean_width.mt, mean_depth.mt, ttl, "tile",
                  res, grid_cell)

  IC <- mutate(
    IC,
    del = (!! args[["M_R"]] /  !! args[["hat_M_M_R"]]  - 1) * 1000,
    t_score = abs(.data$del / !! args[["hat_RS_M_R"]]),
    sig_code = sig_coder(!! args[["p_R"]], make_lab = FALSE),
    del_lab = paste(sprintf("%.1f", .data$del), .data$sig_code)
    ) %>%
    rename(x = .data$mean_width.mt, y = .data$mean_height.mt)

  gg_final <- function(IC) {
    gg_base +
      geom_tile(
        data = IC,
        aes(x = .data$x, y = .data$y),
        fill = "transparent",
        color = "black",
        size = 1,
        inherit.aes = FALSE
        ) +
      depth_arrows(res, grid_cell) +
      geom_text(
        data = IC,
        aes(
          x = .data$x,
          y = .data$y,
          color = .data$t_score,
          label = .data$del_lab
          ),
        inherit.aes = FALSE
        ) +
      scale_color_distiller(
        expression("Inter-isotope var."~italic(p)),
        palette = "OrRd",
        breaks = sapply(c(0.1, 0.01, 0.001) / 2, qt, 16, lower.tail = FALSE),
        labels = c(0.1, 0.01, 0.001),
        direction = 1,
        ) +
      ggplot2::guides(color = ggplot2::guide_colorbar(title.position = "top")) +
      labs(
        title = ttl,
        subtitle =
          substitute(
            "Intra-isotope var."~italic(p)*":"~a,
            list(a = sig_coder())
            )
         ) +
      themes_IC() +
      ggplot2::theme(axis.line = ggplot2::element_blank())
  }

  grid_loc <- list(
    geom_text(
      data = IC,
      aes(
        x = .data$x + grid_cell * 0.375,
        y = data$y + grid_cell * 0.375,
        label = data$grid.nm
      ),
      size = 3,
      inherit.aes = FALSE
    )
  )

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
      tex_labeller(tb_model, tb_model$name, label)
      )
    } else {
      ls_latex <- sapply(model_args, as_name)
      }

  if (grid_print) {
    {print(gg_final(IC) + grid_loc)}
    return(distinct(select(IC, .data$dim_name.nm, !!! ls_latex)))
    } else {
      suppressWarnings(print(gg_final(IC)))
      return(distinct(select(IC, .data$dim_name.nm, !!! ls_latex)))
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
    purrr::map(
      ~summarise(
        group_by(.x, grid.nm),
        across(.fns = mean),
        .groups = "drop"
        )
      ) %>%
    purrr::map(tibble::add_column, sample.nm = "sketch")

  fold <- dim_folds(ls_sketch, height, width,  depth, "sketch", "tile", res,
                    grid_cell)
  ggplot(fold, aes(x = .data$width, y = .data$height)) +
    geom_tile(fill = "transparent", color = "black") +
    geom_text(aes(label = .data$grid.nm)) +
    ggplot2::coord_fixed(clip = "off") +
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
    ggplot2::theme(axis.line = ggplot2::element_blank()) +
    themes_IC(base = ggplot2::theme_void())
}

#-------------------------------------------------------------------------------
# supportive functions
#-------------------------------------------------------------------------------
# Calculate distance for side plots (width and height aggregation plane)
dim_folds <- function(ls, dim1, dim2, dim3, ttl, geom, res, grid_cell){

  dim1 <- enquo(dim1)
  dim2 <- enquo(dim2)
  dim3 <- enquo(dim3)

  ls <- purrr::map(ls, ~filter(.x, .data$sample.nm == ttl))

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
      ~{mutate(.x, !! dim1 := !! dim3 + res + grid_cell / 2) %>%
          select(-!! dim3)}
      ) %>%
      purrr::map_at(
        "width",
        ~{mutate(.x, !! dim2 := !! dim3 + res + grid_cell / 2) %>%
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

# Build new quosures and names for calcs
arg_builder <- function(args, stat, ion = NULL, append = NULL){

  if (stat == "X") arg_names <- point::names_stat_X
  if (stat == "R") arg_names <- point::names_stat_R
  if (stat == "model") arg_names <- point::names_model

  if (stat == "model") pre <-  NULL else pre  <- "."

  # no origin of variable names
  if (!"origin" %in% colnames(arg_names)) arg_names$origin <- NA_character_

  arg_names <- mutate(
    arg_names,
    origin = if_else(is.na(.data$origin), .data$derived, .data$origin),
    label =
      if_else(
        .data$origin == .data$derived,
        paste0(paste(.data$name, .data$origin, sep = "_"), append),
        paste0(paste(.data$name, .data$derived, sep = "_"), append)
      ),
    name =
      if_else(
        .data$origin == .data$derived,
        .data$name,
        paste(.data$name, .data$derived, sep = "_")
      )
  )

  # quosure update
  args <- purrr::map2(
    arg_names$origin,
    arg_names$name,
    ~quo_updt(args[[paste0(pre, .x)]], pre = .y)
  )
  # wide format with ions
  if (!is.null(ion)) args <- purrr::map(args, quo_updt, post = ion)
  # set names
  set_names(args, nm = arg_names$label)
}

# Function which updates quosures for subsequent tidy evaluation
quo_updt <- function(my_q, pre = NULL, post = NULL, update_post = FALSE){

  # Get expressions
  old_expr <- get_expr(my_q)
  # Get text
  old_chr <- expr_text(old_expr)

  # Update
  if (update_post & stringr::str_detect(old_chr , "\\.") ){
    old_chr <- stringr::str_split(old_chr, "\\.")[[1]][1]
  }

  # Separators
  if (is.null(pre) & is.null(post)) {
    warning("Quosure not updated")
    return(my_q)
  }
  if (!is.null(pre) & is.null(post)) {
    new_chr <- paste0(pre, "_", old_chr)
  }
  if (is.null(pre) & !is.null(post)) {
    new_chr <- paste0(old_chr, ".", post)
  }
  if (!is.null(pre) & !is.null(post)) {
    new_chr <- paste0(pre, "_", old_chr, ".", post)
  }

  # New expression from character (remove whitespace)
  new_expr <- parse_expr(stringr::str_replace_all(new_chr, " ", ""))
  # Update old quosure
  set_expr(my_q, new_expr)
}

# Select arguments
all_args <- function(args, ion1, ion2, except = NULL, chr = TRUE) {
  args_Xt <- purrr::flatten(
    purrr::map2(
      c(ion1, ion2),
      seq_along(c(ion1, ion2)),
      ~arg_builder(args, "X", .x, .y)
    )
  )
  args_R <- list2(
    !!! arg_builder(args, "R"),
    R = quo_updt(args[[".X"]], pre = "R"),
    ratio = parse_quo("ratio.nm", env = quo_get_env(args[[".X"]]))
  )
  args <- append(args_Xt, args_R)
  if (isTRUE(chr)) {
    vc_args <- sapply(args, as_name)
    return(vc_args[!vc_args %in% except])
  } else {
    return(args)
  }
}

# latex labeller function
tex_labeller <- function(vars, stat, label){
  if (!"origin" %in% colnames(vars)) vars$origin <- vars$derived
  names_vars <- filter(vars, .data$name %in% stat) %>%
    # if variable has a stat component
    mutate(
      derived =
        if_else(
          stringr::str_detect(.data$derived, "[[:punct:]]"),
          stringr::str_extract("M_R", "(?<=[[:punct:]])[[:alpha:]]"),
          .data$derived
        )
    )
  purrr::pmap_chr(
    list(
      var = names_vars$derived,
      org = names_vars$origin,
      stat = names_vars$name
    ),
    point::stat_labeller,
    label = label
  )
}
