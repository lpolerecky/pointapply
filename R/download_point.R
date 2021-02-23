#' Write point ion count data
#'
#'  A wrapper around zen4R in order to download the large matlab files as well as processed data cubes.
#'
#' @export
download_point <- function (type = "processed", on_build = FALSE) {

  if (on_build) {
    path <- usethis::proj_path("inst/extdata")
    } else {
     path_ext <- fs::path_package("pointapply", "extdata")
     path_int <- fs::path_package("pointapply", "data")
     }

  # get the data
  pointdata <- zen4R::download_zenodo("10.5281/zenodo.4557485", path = path_ext )

  # extract matlab
  ls_mat <- list.files(path_ext, pattern = "GLENDON.zip", full.names = TRUE)
  purrr::walk(ls_mat, ~unzip(.x, exdir = tools::file_path_sans_ext(.x), junkpaths = TRUE))

  # purge data
  purrr::walk(list.files(path_int, full.names = TRUE), file.remove)
  # extract data rda format
  unzip(list.files(path_ext, pattern = "data.zip", full.names = TRUE), exdir = path_int, junkpaths = TRUE)

}
#' @rdname download_point
#'
#' @export
write_point <- function (obj, on_build = FALSE) {

  obj <- rlang::as_name(obj)

  if (on_build) {
    path <- usethis::proj_path("inst/extdata/data", obj, ext = "rda")
    } else {
      path <- fs::path_package("pointapply", "data",  obj, ext = "rda")
    }
  envir <- parent.frame()
  args <- list2(list = obj, file = path, envir = envir, compress = "xz", version = 2)

  rlang::exec("save", !!!args)
}
#' @rdname download_point
#'
#' @export
save_point <- function (ggplot, name, width, height, unit, on_build = FALSE) {

  if (on_build) {
    path <- usethis::proj_path("paper/graphs", name, ext = "png")
  } else {
    path <- fs::path_package("pointapply", "paper/graphs", name, ext = "png")
  }
  envir <- parent.frame()
  args <- list2(ggplot, file = path, width = width, height = height, unit = unit)

  rlang::exec("ggsave", !!!args)
}
