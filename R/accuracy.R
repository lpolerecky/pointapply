#' Accuracy of model classification
#'
#' @return Numeric vector
#'
#' @export
acc_fun <- function(x , y, ntot){
  if(x != 0) return(sum(y < 0.001) / ntot)
  if(x == 0) return(1 - (sum(y < 0.001) / ntot))
}
