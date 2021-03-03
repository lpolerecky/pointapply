#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Tee pipe operator
#'
#' See \code{magrittr::\link[magrittr:tee]{\%T>\%}} for details.
#'
#' @name %T>%
#' @rdname tee
#' @keywords internal
#' @export
#' @importFrom magrittr %T>%
#' @usage lhs \%T>\% rhs
NULL
