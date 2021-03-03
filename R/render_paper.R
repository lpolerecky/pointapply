#' Render the paper
#'
#' \code{render_paper()} is a wrapper around \code{rmarkdown::render_site} which
#' renders the paper of this study.
#'
#' @param on_build Logical whether download is loaded during build
#' (default = FALSE).
#'
#' @export
render_paper <- function(title = "Schobbenetal_SIMS_method",
                         output_dir = "/home/amandus/Documents/work/manuscripts/Schobbenetal_SIMS_method",
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
    "rmd_files: [\"index.Rmd\", \"main.Rmd\", \"references.Rmd\"]",
    paste0("output_dir: ", output_dir) ,
    file = paste(path, "_bookdown.yml", sep = "/"),
    sep = "\n"
    )

  rmarkdown::render_site(input = path, encoding = 'UTF-8')
}
