#' Barplots for hypothesis testing
#'
#' \code{hyp_class} Barplot for the classification of failing to reject a zero
#' hypothesis (Fig. 3 and Supplementary Fig. 5)
#'
#' @param IC Ion count data
#' @param sec_vc Named vector for secondary axis.
#' @param ttl  Character string or expression for plot title.
#'
#' @return \code{ggplot2::\link[ggplot2:ggplot]{ggplot}}.
#'
#' @export
hyp_class <- function(IC, sec_vc, ttl) {

  distinct(IC, grid_size.nm, grid.nm, .keep_all = TRUE) %>%
    add_count(grid_size.nm, name = "ntot") %>%
    count(grid_size.nm, hyp, ntot) %>%
    mutate(freq = n / ntot) %>%
    filter(stringr::str_detect(hyp, "H0")) %>%
    ggplot(aes(x = grid_size.nm, y = freq)) +
    geom_bar(stat ="identity", position = "dodge") +
    scale_x_continuous(
      breaks = as.numeric(names(sec_vc)),
      trans = "log2",
      labels = scales::label_number(accuracy = 0.1),
      sec.axis = dup_axis(
        name = expression(bar(N)~"("^13*C*" count)"),
        labels = unname(sec_vc)
        )
      ) +
    labs(
      title = ttl,
      x = expression("grid-cell ("*mu*m^2*")"),
      y = expression("frequency H"[0])
      ) +
    theme_classic() +
    themes_IC
}
