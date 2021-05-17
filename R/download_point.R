#' Download, write and plot ion count data
#'
#' \code{download_point()} is a wrapper around zen4R in order to download the
#' large matlab files as well as processed data cubes and simulated data.
#' \code{write_point()} stores processed  matlab files in the correct directory.
#' \code{save_point()} stores ggplots in the correct directory for usage in the
#' paper.
#' \code{load_point()} load .rda files of the package.
#'
#' @param obj Character string of object to store.
#' @param ggplot ggplot object to be stored.
#' @param name Character string for graphic based on ggplot or file to be
#' loaded.
#' @param width Numeric value for width of graphic based on ggplot.
#' @param height Numeric value for height of graphic based on ggplot.
#' @param unit Character string indicating dimension units for graphic based
#' on ggplot.
#' @param type Character string for type of file to be loaded.
#' @param grid_cell Numeric value of grid_cell size in pixels.
#' @param return_name Logical whether to return file names.
#' @param on_build Logical whether download is loaded during build
#' (default = FALSE).
#' @param type_ms Character string for type manuscript, either \code{"preprint"}
#' or \code{"paper"}.
#'
#' @return Raw matlab files are stored in the directories
#' `extdata/2020-08-20-GLENDON` and `extdata/2020-08-28-GLENDON`, whereas
#' processed data is stored in the directory `data`. Figures are stored in the
#' directory `paper/2020-08-20-GLENDON`
#'
#' @export
download_point <- function (type = "all") {

  path_int <- fs::path_package("pointapply", "data")
  # create extdata if no existing
  if (!fs::dir_exists(fs::path(fs::path_package("pointapply"), "extdata"))) {
    fs::dir_create(fs::path_package("pointapply"), "extdata")
    }
  path_ext <- fs::path_package("pointapply", "extdata")

  # check if dirs are present in extdata
  lgl_ext <- purrr::map_lgl(
    c("2020-08-28-GLENDON", "2020-08-20-GLENDON"),
    ~fs::dir_exists(fs::path(path_ext, .x))
    )
  # check if rda files are present in data
  lgl_int <- length(list.files(path_int, pattern = ".rda$")) > 0

  if (all(lgl_int, lgl_ext)) return(invisible())

  # purge other data
  purrr::walk(list.files(path_int, full.names = TRUE), file.remove)

  # get the data
  if (length(list.files(path_ext, pattern = ".zip$")) == 0) {
    pointdata <- zen4R::download_zenodo("10.5281/zenodo.4748667",
                                        path = path_ext)
    }

  if (type == "all" | type == "raw") {
    # extract matlab
    ls_mat <- list.files(path_ext, pattern = "GLENDON.zip$", full.names = TRUE)
    purrr::walk(
      ls_mat,
      ~unzip(.x, exdir = tools::file_path_sans_ext(.x), junkpaths = TRUE)
      )
    }

  if (type == "all" | type == "processed") {
    # extract data (.rda format)
    ls_dat <- list.files(path_ext, pattern = "data.zip", full.names = TRUE)
    unzip(ls_dat, exdir = path_int, junkpaths = TRUE)
    }
}
#' @rdname download_point
#'
#' @export
write_point <- function (obj, on_build = FALSE) {

  if (on_build) {
    path <- fs::path_wd("inst/extdata/data", obj, ext = "rda")
    } else {
      path <- fs::path_package("pointapply", "data",  obj, ext = "rda")
      }
  envir <- parent.frame()
  args <- rlang::list2(obj, file = path, envir = envir, compress = "xz",
                       version = 2)

  rlang::exec("save", !!!args)
}
#' @rdname download_point
#'
#' @export
save_point <- function (name, ggplot = ggplot2::last_plot(), width, height,
                        unit, on_build = FALSE, type_ms = "preprint") {

  if (on_build) {
    path <- fs::path_wd()
      } else {
      path <- fs::path_package("pointapply")
      }

  args <- rlang::list2(
    filename = fs::path(path, "vignettes/figures", name, ext = "png"),
    plot = ggplot,
    width = width,
    height = height,
    unit = unit
    )
  rlang::exec("ggsave", !!! args)

  if (on_build) {
    # copy whole dir to paper
    fs::dir_copy(
      fs::path(path, "vignettes/figures"),
      fs::path(path, "inst/paper", type_ms, "figures"),
      overwrite = TRUE
      )
      }
}
#' @rdname download_point
#'
#' @export
load_point <- function(type, name, grid_cell, return_name = FALSE, on_build){

  name <- tidyr::crossing(grid_cell, name) %>%
    dplyr::rowwise() %>%
    dplyr::transmute(
      name =
        ifelse(
          is.null(grid_cell),
          paste(type, name, sep = "_"),
          paste(type, grid_cell, name, sep = "_")
          )
      ) %>%
    dplyr::pull(name)

  if (on_build) {
    fs::path_wd("inst/extdata/data", name, ext = "rda") %>%
      purrr::walk(load, .GlobalEnv)
    } else {
      data(list = name, envir = environment())
      }
  if (return_name) return(name)
}
