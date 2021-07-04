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
#'
#' @return Rendered documents including a tex, docx and pdf file. Depending on
#' the argument \code{copy_figures} the call returns figures in png format
#' within its own directory (figures).
#' @export
render_paper <- function(
  title = "Schobbenetal_SIMS_method",
  output_dir = fs::path_wd(),
  type_ms = "preprint",
  copy_figures = TRUE
  ){


  # paths
  paper <- fs::path("paper", type_ms)
  # dirs
  paper_dirs <- fs::path_package("pointapply", c(paper, "data"))
  # bibs
  purrr::walk(c("packages", "rversion"), ~bib_copy(paper_dirs[1], .x))

  # make links
  fs::link_create(
    fs::path_package("pointapply", "paper", "templates"),
    fs::path_package("pointapply", "paper", type_ms, "templates")
    ) # templates

  # make bookdown_yml
  cat(
    paste0("book_filename: \"", title, "\""),
    "delete_merged_file: true",
    "rmd_files: [\"index.Rmd\", \"main.Rmd\", \"SI.Rmd\",  \"references.Rmd\"]",
    paste0("output_dir: ", output_dir) ,
    # "clean: [\"\"]",
    file = fs::path_package("pointapply", "paper", type_ms, "_bookdown.yml"),
    sep = "\n"
    )

  # render paper
  rmarkdown::render_site(
    input = fs::path_package("pointapply", "paper", type_ms),
    encoding = 'UTF-8'
    )

  # copy figures
  fs::dir_copy(
    fs::path_package("pointapply", "paper", type_ms, "figures"),
    fs::path(output_dir, "figures"),
    overwrite = TRUE
    )
}
#' @rdname render_paper
#'
#' @export

edit_paper <- function(type_ms = "preprint") {
  path <- fs::path_package("pointapply", "paper", type_ms)
  fs::path(path, c("index", "main", "SI"), ext = "Rmd") %>%
    fs::file_show()
}

# copy bib files
bib_copy <- function(path_new, bib) {
  bib <- fs::path(bib, ext = "bib")
  if (fs::file_exists(bib)) {
    fs::file_copy(
      fs::path_package("pointapply", bib),
      fs::path(path_new, bib),
      overwrite = TRUE
    )
    }
  }


