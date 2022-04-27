#' Barplots for hypothesis testing
#'
#' \code{hyp_class} Barplot for the classification of failing to reject a zero
#' hypothesis (Fig. 3 and Supplementary Fig. 5)
#'
#' @param IC Ion count data
#' @param sec_vc Named vector for secondary axis.
#' @param ttl  Character string or expression for plot title.
#' @param save Boolean whether to save the plot as an png.
#'
#' @return \code{ggplot2::\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
hyp_class <- function(IC, sec_vc, ttl, save = FALSE) {

  sec_vc <- rlang::set_names(
    sprintf("%.0f", sec_vc$N) ,
    nm = sec_vc$grid_size.nm
  )

  IC <- dplyr::distinct(
    IC,
    .data$grid_size.nm,
    .data$grid.nm,
    .keep_all = TRUE
  ) %>%
    dplyr::add_count(.data$grid_size.nm, name = "ntot") %>%
    dplyr::count(.data$grid_size.nm, .data$hyp, .data$ntot) %>%
    dplyr::mutate(freq = .data$n / .data$ntot) %>%
    dplyr::filter(stringr::str_detect(.data$hyp, "H0"))

  p <- ggplot2::ggplot(
    data = IC,
    mapping = ggplot2::aes(x = .data$grid_size.nm, y = .data$freq)
  ) +
    ggplot2::geom_bar(stat ="identity", position = "dodge") +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(names(sec_vc)),
      trans = "log2",
      labels = scales::label_number(accuracy = 0.1),
      sec.axis = ggplot2::dup_axis(
        name = expression(bar(N)~"("^13*C*" count)"),
        labels = unname(sec_vc)
        )
      ) +
    ggplot2::labs(
      title = ttl,
      x = expression("grid-cell ("*mu*m^2*")"),
      y = expression("frequency H"[0])
      ) +
    themes_IC(base = ggplot2::theme_classic())

  if (isTRUE(save)) {
    # save plot
    nm <- gsub(" ", "_", casefold(ttl))
    nm <- paste("bar", nm, "class", sep = "_")
    save_point(nm, p, width = 7, height = 7, unit = "cm")

    message(
      paste0("Hypothesis test plot has been saved with name ", nm, " .")
    )
  }
  p
}
