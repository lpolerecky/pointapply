#' Sensitivity test of diagnostics
#'
#' @param yield Ionization yield.
#' @param R Isotope ratio.
#' @param reps Repetitions.
#' @param ion1 Rare ion.
#' @param ion2 Common ion.
#' @param reference Reference standard.
#' @param diag Name of the diagnostics test (either CooksD or Cameca).
#' @param type Type of diagnostics (either "intra"- or "inter"-analysis).
#' @param save Boolean whether to save the data.
#' @param mc_cores Number of cores to use for parallel execution.
#'
#' @return A tibble with synthetic data or diagnostic results.
#'
#' @export
#'
test_sensitivity <- function(yield, R, reps, ion1 = "13C", ion2 = "12C",
                             reference = "VPDB", diag = "sens",type = "intra",
                             save = FALSE, mc_cores = 1) {

  # types of R variation
  if (type == "intra") {
    scenario <- c("symmetric", "asymmetric")
  } else if (type == "inter") {
    scenario <- "ideal"
  }

  # seeds for number generation
  n_tot <- prod(sapply(list(yield, scenario, R), length))
  seed <- 1:n_tot

  # intra-analysis isotope variability
  if (type == "intra") {

    # cross all possible parameter combinations
    ls_parms <- tidyr::crossing(.sys = yield, .type = scenario, .devR = R) |>
      tibble::add_column(.seed = seed)

    # function wrapper to prepare for mcMap
    inner <- function(.sys, .type, .devR, .seed) {

      # sensitivity run
      x <- point::simu_R(.sys = .sys, .type = .type, .devR = .devR,
                         .seed = .seed, .reps = reps, .ion1 = ion1,
                         .ion2 = ion2, .reference = reference)

      if (diag == "sens") {

        # return sensitivity run
        x

      } else if (diag == "CooksD") {

        # diagnostics CooksD
        point::diag_R(x, "13C", "12C", .data$type.nm, .data$trend.nm,
                      .data$force.nm, .data$spot.nm,
                      X = rlang::sym("Xt.sm"), .N = rlang::sym("N.sm"))

      } else if (diag == "Cameca") {

        # diagnostics Cameca
        x <- point::diag_R(x , "13C", "12C", .data$type.nm, .data$trend.nm,
                           .data$force.nm, .data$spot.nm, .data$bl.nm,
                           X = rlang::sym("Xt.sm"), .N = rlang::sym("N.sm"),
                           .method = "Cameca", .output = "diagnostic")
        # evaluation of performance (intra-analysis isotope test)
        x <- point::eval_diag(x, "13C", "12C", .data$type.nm, .data$trend.nm,
                              .data$force.nm, .data$spot.nm,
                              X = rlang::sym("Xt.sm"),.N = rlang::sym("N.sm"))
        # extract distinct groups omitting block-wise means
        dplyr::distinct(x, .data$type.nm, .data$trend.nm, .data$force.nm,
                        .data$spot.nm, .keep_all = TRUE)

      }
    }

    # parallel execution
    tb <- rlang::inject(
      parallel::mcMap(inner, !!! ls_parms, mc.cores = mc_cores)
    ) |>
      dplyr::bind_rows()

  } else if (type == "inter") {

    # only one diagnostic option
    diag <- "nlme"

    # cross all possible parameter combinations
    ls_parms <- tidyr::crossing(.sys = yield, .baseR = R) %>%
      tibble::add_column(.seed = seed)

    # function wrapper to prepare for mcMap
    inner2 <- function(.sys, .baseR, .seed) {

      # sensitivity run
      point::simu_R(.sys = .sys, .baseR = .baseR, .type = scenario,
                    .seed = .seed, .reps = reps, .ion1 = ion1,
                    .ion2 = ion2, .reference = reference)
    }

    # parallel execution for producing ion count data
    tb <- rlang::inject(
      parallel::mcMap(inner2, !!! ls_parms, mc.cores = mc_cores)
    ) |>
      dplyr::bind_rows()

    # parallel execution for mixed sets of isotope analysis
    tb <- parallel::mcMap(
      function(n) {

      mix_R(simu = tb, R = R, n = n, reps = reps) |>
        tibble::add_column(study.nm  = n) |>
        point:::diag_R("13C", "12C", .data$type.nm, .data$trend.nm,
                       .data$anomaly.nm, .data$study.nm, .data$spot.nm,
                       .nest = rlang::sym("spot.nm"), .X = rlang::sym("Xt.sm"),
                       .N = rlang::sym("N.sm")) |>
          # only keep inter-analysis test statistics
          dplyr::distinct(
            dplyr::across(c(.data$trend.nm, .data$anomaly.nm)),
            .keep_all = TRUE
          )


      },
      1:reps,
      mc.cores = mc_cores
    ) |>
      dplyr::bind_rows()
  }


  if (isTRUE(save)) {
    # if (type == "inter") type <- paste0(type, n)
    nm <- paste("simu", diag, "IC", type, sep = "_")
    write_point(tb, nm)

    message(
      paste0("Simulated data has been saved with name ", nm, ".")
    )
  }
  # and return without saving
  tb
}

# isotope anomaly mixer
mix_R <- function(simu, R, n, reps) {
  purrr::map_dfr(
    R,
    ~mix_R_(
      simu = simu,
      R = .x,
      seed = n,
      reps = reps
    )
  )
}

mix_R_ <- function(simu, R, seed, reps) {
  set.seed(seed)
  spot_samp <- sample.int(reps, size = 1)
  base <- dplyr::filter(simu, .data$base.nm == 0, .data$spot.nm != spot_samp)
  anomaly <- dplyr::filter(simu, .data$base.nm == R, .data$spot.nm == spot_samp)
  dplyr::bind_rows(base, anomaly) |>
    dplyr::mutate(anomaly.nm = R, .before = .data$base.nm)
}
