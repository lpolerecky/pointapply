#' Create significance codes for p values
#'
#' The function \code{sig_coder()} produces significance stars associated with p
#' values
#'
#' @param x p value (numeric) (default = NULL)
#' @param make_lab Logical whether to return a legend for the significance
#'  stars (default = TRUE).
#'
#' @return Character string encoding for the significance star associated with
#' the p value. If argument \code{make_lab = TRUE} then a list is return, where
#' the second element contains the legend.
#'
#' @export
sig_coder <- function(x = NULL, make_lab = TRUE) {

  NA_cross <- "\u2715"
  sig_code <- c("", "\u02d9", "*", "**", "***")
  sig_val <- c(1, 0.1, 0.05, 0.01, 0.001, 0)
  tb_sigs <- tibble::tibble(
    upper = sig_val[-length(sig_val)],
    lower = sig_val[-1],
    sig_code = sig_code
  )

  if (!is.null(x)) {

    # find p value from table
    p_finder <- function(x) {

      if (is.na(x)) return(NA_cross)
      if (x == 0) return("***")

      dplyr::pull(
        dplyr::filter(tb_sigs, .data$upper >= x & .data$lower < x),
        "sig_code"
      )
    }

    # vectorize p finding
    stars <- purrr::map_chr(x, p_finder)
  }

  # make legend
  if (isTRUE(make_lab)) {
    star_labs <- stringr::str_c(
      purrr::map2_chr(
        sig_val[-1],
        c(sig_code[-1], ""),
        ~paste(.x, .y)
      ),
      collapse = " "
    )
    # add cross for NA values if exists

    if (any(is.na(x))) star_labs <- paste(star_labs, "missing:", NA_cross)
    # if no value only return label
    if (!is.null(x)) list(stars, star_labs) else star_labs
  } else {
    stars
  }
}
