
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pointapply

<!-- badges: start -->

<!-- badges: end -->

The R package `pointapply` contains the code and data to reconstruct the
publication: Martin Schobben, Michiel Kienhuis, and Lubos Polerecky.
2021. *New methods to detect isotopic heterogeneity with Secondary Ion
Mass Spectrometry*, preprint on [Eartharxiv](https://eartharxiv.org/).

This paper assess the performance of the application of the `diag_R()`
and `eval_diag` function of the sister package `point` in detecting
isotope heterogeneity in natural substrates.

# Render the paper

Use the function `pointapply::render_paper()` and specify the directory
to save the paper.

``` r
# Knit
pointapply::render_paper(title = "Schobbenetal_SIMS_method", 
                         output_dir = "mydir")
```

## Credits

The construction of the R (R Core Team 2020) package *pointapply* and
associated documentation was aided by the packages; *devtools* (H.
Wickham, Hester, and Chang 2020), *roxygen2* (H. Wickham et al. 2020),
*knitr* (Xie 2021 , 2015), *rmarkdown* (Allaire et al. 2020; Xie,
Allaire, and Grolemund 2018).

The book: *R packages: organize, test, document, and share your code*,
by
(<span class="citeproc-not-found" data-reference-id="Wickham2015">**???**</span>)
is a great guide for learning how to build packages.

In addition, this package relies on a set of external packages from the
tidyverse universe, including: *dplyr* (Wickham et al. 2021), *tidyr*
(H. Wickham 2020b), *tibble* (Müller and Wickham 2021), *stringr*
(Wickham 2019), *magrittr* (Bache and Wickham 2014), *ggplot2* (Wickham
2016), and *purrr* (Henry and Wickham 2020a) for internal functioning.
The package *rlang* (Henry and Wickham 2020b) was used for tidy
evaluation.

Some specialised specialised packages where used, notably; *R.matlab*
for loading the matlab LANS files (Bengtsson 2018) and *cubelyr* (H.
Wickham 2020a) for flattening the 3D arrays.

The paper was written with *bookdown* (Xie 2020, 2016), *rticles*
(Allaire et al. 2021) and *bibtex* (Francois 2017).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MartinSchobben/pointapply")
```

## Reconstruct the paper from scratch

The data figures can be constructed with the functions contain in this
package. The following vignettes detail all these operations in a
coherent story line.

**Data:**

  - Synthetic data for validation of model performance and Figure 2
    (`vignette("simulation")`).
  - Real data reading and processing and Supplementary Figure 1
    (`vignette("data")`).

Because of the excessive file-sizes of ion count data, the data is
stored externally on [Zenodo](https://doi.org/10.5281/zenodo.4564170),
and can be accessed with the function `download_point()`.

**Figures:**

  - Evaluation of model performance; Figures 4–6
    (`vignette("performance")`).
  - Raster images and scatter plots of real SIMS
    <sup>13</sup>C/<sup>12</sup>C analyses; Figures 7–9 and
    Supplementary Figures 7 and 8 (`vignette("raster")`).
  - Regression diagnostics; Figure 3 and Supplementary Figures 3–5
    (`vignette("regression")`).
  - Accuracy of SIMS isotope analysis; Figure 3 and Supplementary
    Figures 2 and 6 (`vignette("accuracy")`).

# References

<div id="refs" class="references">

<div id="ref-rmarkdown1">

Allaire, JJ, Yihui Xie, Jonathan McPherson, Javier Luraschi, Kevin
Ushey, Aron Atkins, Hadley Wickham, Joe Cheng, Winston Chang, and
Richard Iannone. 2020. *Rmarkdown: Dynamic Documents for R*.
<https://github.com/rstudio/rmarkdown>.

</div>

<div id="ref-rticles">

Allaire, JJ, Yihui Xie, R Foundation, Hadley Wickham, Journal of
Statistical Software, Ramnath Vaidyanathan, Association for Computing
Machinery, et al. 2021. *Rticles: Article Formats for R Markdown*.
<https://CRAN.R-project.org/package=rticles>.

</div>

<div id="ref-magrittr">

Bache, Stefan Milton, and Hadley Wickham. 2014. “magrittr: A
Forward-Pipe Operator for R.”
<https://cran.r-project.org/package=magrittr>.

</div>

<div id="ref-R.matlab">

Bengtsson, Henrik. 2018. *R.matlab: Read and Write Mat Files and Call
Matlab from Within R*. <https://CRAN.R-project.org/package=R.matlab>.

</div>

<div id="ref-bibtex">

Francois, Romain. 2017. “bibtex: Bibtex Parser.”
<https://cran.r-project.org/package=bibtex>.

</div>

<div id="ref-purrr">

Henry, Lionel, and Hadley Wickham. 2020a. *Purrr: Functional Programming
Tools*. <https://CRAN.R-project.org/package=purrr>.

</div>

<div id="ref-rlang">

———. 2020b. *Rlang: Functions for Base Types and Core R and ’Tidyverse’
Features*. <https://CRAN.R-project.org/package=rlang>.

</div>

<div id="ref-tibble">

Müller, Kirill, and Hadley Wickham. 2021. *Tibble: Simple Data Frames*.
<https://CRAN.R-project.org/package=tibble>.

</div>

<div id="ref-rversion">

R Core Team. 2020. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-ggplot2">

Wickham, Hadley. 2016. *Ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. <https://ggplot2.tidyverse.org>.

</div>

<div id="ref-stringr">

———. 2019. “stringr: Simple, Consistent Wrappers for Common String
Operations.” <https://cran.r-project.org/package=stringr>.

</div>

<div id="ref-cubelyr">

———. 2020a. *Cubelyr: A Data Cube ’Dplyr’ Backend*.
<https://CRAN.R-project.org/package=cubelyr>.

</div>

<div id="ref-tidyr">

———. 2020b. *Tidyr: Tidy Messy Data*.
<https://CRAN.R-project.org/package=tidyr>.

</div>

<div id="ref-roxygen2">

Wickham, Hadley, Peter Danenberg, Gábor Csárdi, and Manuel Eugster.
2020. *Roxygen2: In-Line Documentation for R*.
<https://CRAN.R-project.org/package=roxygen2>.

</div>

<div id="ref-dplyr">

Wickham, Hadley, Romain François, Lionel Henry, and Kirill Müller. 2021.
*Dplyr: A Grammar of Data Manipulation*.
<https://CRAN.R-project.org/package=dplyr>.

</div>

<div id="ref-devtools">

Wickham, Hadley, Jim Hester, and Winston Chang. 2020. *Devtools: Tools
to Make Developing R Packages Easier*.
<https://CRAN.R-project.org/package=devtools>.

</div>

<div id="ref-knitr2">

Xie, Yihui. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Boca
Raton, Florida: Chapman; Hall/CRC. <https://yihui.org/knitr/>.

</div>

<div id="ref-bookdown2">

———. 2016. *Bookdown: Authoring Books and Technical Documents with R
Markdown*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://github.com/rstudio/bookdown>.

</div>

<div id="ref-bookdown1">

———. 2020. *Bookdown: Authoring Books and Technical Documents with R
Markdown*. <https://github.com/rstudio/bookdown>.

</div>

<div id="ref-knitr1">

———. 2021. *Knitr: A General-Purpose Package for Dynamic Report
Generation in R*. <https://yihui.org/knitr/>.

</div>

<div id="ref-rmarkdown2">

Xie, Yihui, J. J. Allaire, and Garrett Grolemund. 2018. *R Markdown: The
Definitive Guide*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown>.

</div>

</div>
