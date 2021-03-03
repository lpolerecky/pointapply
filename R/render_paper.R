#' Render the paper
#'
#' \code{render_paper()} is a wrapper around
#' \code{rmarkdown::\link[rmarkdown:rendersite]{rendersite}} which renders the
#' paper of this study.
#'
#' @param title Character string for title of rendered document.
#' @param output_dir Character string for directory containing the rendered
#' document.
#' @param copy_figures Logical wether to copy figures to directory
#' (default = TRUE).
#' @param on_build Logical whether download is loaded during build
#' (default = FALSE).
#'
#' @return Rendered documents including a tex, docx and pdf file. Depending on
#' the argument \code{copy_figures} the call returns figures in png format
#' within its own directory (figures).
#' @export
render_paper <- function(title = "Schobbenetal_SIMS_method",
                         output_dir = "/home/amandus/Documents/work/manuscripts/Schobbenetal_SIMS_method",
                         copy_figures = TRUE,
                         on_build = TRUE){

  if (on_build) {
    path <- usethis::proj_path("inst/paper/preprint")
    } else {
      path <- fs::path_package("pointapply", "paper/preprint")
    }

  # make bookdown_yml
  cat(
    paste0("book_filename: \"",title,"\""),
    "delete_merged_file: true",
    "rmd_files: [\"index.Rmd\", \"main.Rmd\", \"SI.Rmd\", \"references.Rmd\"]",
    paste0("output_dir: ", output_dir) ,
    # "clean: [\"\"]",
    file = paste(path, "_bookdown.yml", sep = "/"),
    sep = "\n"
    )
  # render paper
  rmarkdown::render_site(input = path, encoding = 'UTF-8')
  # copy figures
  fs::dir_copy(
    paste(path, "figures/", sep = "/"),
    paste(output_dir, "figures", sep = "/"),
    overwrite = TRUE
    )
}
