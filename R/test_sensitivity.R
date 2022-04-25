#' Title
#'
#' @param yield
#' @param R
#' @param reps
#' @param ion1
#' @param ion2
#' @param reference
#' @param diag
#' @param type
#' @param save
#' @param mc_cores
#'
#' @return
#' @export
#'
#' @examples
test_sensitivity <- function(yield, R, reps, ion1 = "13C", ion2 = "12C",
                             reference = "VPDB", diag = "sens",
                             type = "intra", save = FALSE, mc_cores = 1) {

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
        point::diag_R(x, "13C", "12C", type.nm, trend.nm, force.nm, spot.nm,
                      .X = Xt.sm, .N = N.sm)

      } else if (diag == "Cameca") {

        # diagnostics Cameca
        x <- point::diag_R(x , "13C", "12C", type.nm, trend.nm, force.nm,
                           spot.nm, bl.nm, .X = Xt.sm, .N = N.sm,
                           .method = "Cameca", .output = "diagnostic")
        # evaluation of performance (intra-analysis isotope test)
        x <- point::eval_diag(x, "13C", "12C", type.nm, trend.nm, force.nm,
                              spot.nm, .X = Xt.sm, .N = N.sm)
        # extract distinct groups omitting block-wise means
        dplyr::distinct(x, type.nm, trend.nm, force.nm, spot.nm,
                        .keep_all = TRUE)

      }
    }

    # parallel execution
    tb <- rlang::inject(
      parallel::mcMap(inner, !!! ls_parms, mc.cores = mc_cores)
    ) |>
      dplyr::bind_rows()

  } else if (type == "inter") {

    # cross all possible parameter combinations
    ls_parms <- tidyr::crossing(.sys = yield, .baseR = R) %>%
      tibble::add_column(.seed = seed)

    # function wrapper to prepare for mcMap
    inner <- function(.sys, .baseR, .seed) {

      # sensitivity run
      point::simu_R(.sys = .sys, .baseR = .baseR, .type = scenario,
                    .seed = .seed, .reps = reps, .ion1 = ion1,
                    .ion2 = ion2, .reference = reference)
    }

    # parallel execution for producing ion count data
    tb <- rlang::inject(
      parallel::mcMap(inner, !!! ls_parms, mc.cores = mc_cores)
    ) |>
      dplyr::bind_rows()

    # parallel execution for mixed sets of isotope analysis
    tb <- parallel::mcMap(
      function(x, y) {
        x <- mix_R(simu = tb, R = x, n = y, reps = reps)
        if (diag == "sens") {

          # return sensitivity run
          x

        } else if (diag == "nlme") {

          point:::diag_R(x, "13C", "12C", type.nm, trend.nm, anomaly.nm,
                         study.nm, spot.nm, .nest = study.nm,
                         .X = Xt.sm, .N = N.sm)

        }
      },
      R,
      1:reps,
      mc.cores = mc_cores
    ) |>
      dplyr::bind_rows()


  }

  if (isTRUE(save)) {
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
    ),
    .id = "study.nm"
  )
}

mix_R_ <- function(simu, R, seed, reps) {
  set.seed(seed)
  spot_samp <- sample.int(reps, size = 1)
  base <- dplyr::filter(simu, .data$base.nm == 0, .data$spot.nm != spot_samp)
  anomaly <- dplyr::filter(simu, .data$base.nm == R, .data$spot.nm == spot_samp)
  dplyr::bind_rows(base, anomaly) |>
    mutate(anomaly.nm = R, .before = .data$base.nm)
}
