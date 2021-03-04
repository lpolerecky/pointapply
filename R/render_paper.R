#' Render the paper
#'
#' \code{render_paper()} is a wrapper around
#' \code{rmarkdown::\link[rmarkdown:render_site]{render_site}} which renders the
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
  # download data when needed
  if (!on_build) download_point()

  # paths
  if (on_build) {
    path <- usethis::proj_path("inst/paper")
    IC <- usethis::proj_path("inst/extdata/data")
    templ <- usethis::proj_path("inst/paper/templates")
    figs <- usethis::proj_path("inst/paper/figures")
    } else {
      path <- fs::path_package("pointapply", "paper")
      IC <- fs::path_package("pointapply", "data")
      templ <- fs::path_package("pointapply", "paper/templates")
      figs<- fs::path_package("pointapply", "paper/figures")
      }

  # make links
  if (!fs::file_exists(paste0(path, "/preprint/data"))) file.symlink(IC, paste0(path, "/preprint/data")) # data
  if (!fs::file_exists(paste0(path, "/preprint/templates"))) file.symlink(templ, paste0(path, "/preprint/templates")) # templates
  if (!fs::file_exists(paste0(path, "/preprint/figures"))) file.symlink(figs, paste0(path, "/preprint/figures")) # figures

  # make bookdown_yml
  cat(
    paste0("book_filename: \"",title,"\""),
    "delete_merged_file: true",
    "rmd_files: [\"index.Rmd\", \"main.Rmd\", \"SI.Rmd\",  \"references.Rmd\"]",
    paste0("output_dir: ", output_dir) ,
    # "clean: [\"\"]",
    file = paste(path, "preprint", "_bookdown.yml", sep = "/"),
    sep = "\n"
    )
  # render paper
  rmarkdown::render_site(input = paste0(path, "/preprint"), encoding = 'UTF-8')
  # copy figures
  fs::dir_copy(figs, paste(output_dir, "figures", sep = "/"), overwrite = TRUE)
}
