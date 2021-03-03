#' Accuracy of model classification
#'
#' This function can be used in \code{dplyr::\link[dplyr:summarise]{summarise}}
#' to calculate the accuracy of model classification for the intra- and inter-
#' isotope test.
#'
#' @param x Numeric value for the isotope value of the synthetic value.
#' @param y p value.
#' @param ntot Numeric for total number of model runs.
#'
#' @return Numeric vector for accuracy.
#'
#' @export
acc_fun <- function(x, y, ntot){
  if(x != 0) return(sum(y < 0.001) / ntot)
  if(x == 0) return(1 - (sum(y < 0.001) / ntot))
}
