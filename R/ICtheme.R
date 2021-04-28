themes_IC <- function(base = ggplot2::theme_classic(), ...) {
  `%+replace%` <- ggplot2::`%+replace%`
  base %+replace%
  ggplot2::theme(
    plot.title =  ggplot2::element_text(size = 11, face = "bold",
                                        vjust = 0.2, hjust = 0),
    plot.subtitle =  ggplot2::element_text(size = 9, hjust = 0.5),
    axis.text =  ggplot2::element_text(size = 8),
    axis.title =  ggplot2::element_text(size = 9),
    legend.text =  ggplot2::element_text(size = 8),
    legend.title =  ggplot2::element_text(size = 9),
    strip.text =   ggplot2::element_text(size = 9),
    legend.box = "horizontal",
    legend.position = "top",
    rect =  ggplot2::element_rect(
      fill = "transparent",
      color = "transparent"
      ),
    panel.background = ggplot2::element_rect(
      fill = "transparent",
      color = "transparent"
      ),
    plot.background = ggplot2::element_rect(
      fill = "transparent",
      color = "transparent"
      ),
    legend.background = ggplot2::element_rect(
      fill = "transparent",
      color = "transparent"
      )
    )
}


