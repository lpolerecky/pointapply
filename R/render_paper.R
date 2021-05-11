#' Render the paper
#'
#' \code{render_paper()} is a wrapper around
#' \code{rmarkdown::\link[rmarkdown:render_site]{render_site}} which renders the
#' paper of this study.
#'
#' @param title Character string for title of rendered document.
#' @param output_dir Character string for directory containing the rendered
#' document.
#' @param type_ms Character string for type manuscript, either \code{"preprint"}
#' or \code{"paper"}.
#' @param copy_figures Logical wether to copy figures to directory
#' (default = TRUE).
#' @param on_build Logical whether download is loaded during build
#' (default = FALSE).
#'
#' @return Rendered documents including a tex, docx and pdf file. Depending on
#' the argument \code{copy_figures} the call returns figures in png format
#' within its own directory (figures).
#' @export
render_paper <- function(
  title = "Schobbenetal_SIMS_method",
  output_dir = fs::path_wd(),
  type_ms = "preprint",
  copy_figures = TRUE,
  on_build = FALSE
  ){

  # download data when needed
  if (!on_build) download_point()

  # paths
  if (on_build) {
    path <- fs::path_wd("inst/paper")
    IC <- fs::path_wd("inst/extdata/data")
    pkg <- fs::path_wd("packages", ext ="bib")
    R <- fs::path_wd("rversion", ext ="bib")
    fs::file_copy(
      pkg,
      fs::path(path, type_ms, "packages", ext = "bib"),
      overwrite = TRUE
    ) # bib
    fs::file_copy(
      R,
      fs::path(path,  type_ms, "rversion", ext = "bib"),
      overwrite = TRUE
    ) # bib
    } else {
      path <- fs::path_package("pointapply", "paper")
      IC <- fs::path_package("pointapply", "data")
      }

  # make links
  fs::link_create(IC, fs::path(path,  type_ms, "data")) # data
  fs::link_create(
    fs::path(path, "templates"),
    fs::path(path,  type_ms, "templates")
    ) # templates

  # make bookdown_yml
  cat(
    paste0("book_filename: \"", title, "\""),
    "delete_merged_file: true",
    "rmd_files: [\"index.Rmd\", \"main.Rmd\", \"SI.Rmd\",  \"references.Rmd\"]",
    paste0("output_dir: ", output_dir) ,
    # "clean: [\"\"]",
    file = fs::path(path, "preprint", "_bookdown.yml"),
    sep = "\n"
    )

  # render paper
  rmarkdown::render_site(
    input = fs::path(path, "/preprint"),
    encoding = 'UTF-8'
    )

  # copy figures
  fs::dir_copy(
    fs::path(path, type_ms, "figures"),
    fs::path(output_dir, "figures"),
    overwrite = TRUE
    )
}
