#' Compute the relative bias in the estimated risk ratio as a
#' percentage of the true value across a series of simulation runs
#'
#' @param simulation_results A list of simulation run results produced
#'   by `run_simulation`
#' @param true_risk_ratio The true risk ratio for the association of
#'   exposure with the outcome used in the simulations.
#'
#' @return The percent relative bias of the estimated risk ratio
#'   across the simulation runs.
#' @export
#'
#' @examples
#' probability_exposed <- 0.5
#' probability_stratum_1_given_exposed <- 0.6
#' probability_stratum_1_given_unexposed <- 0.4
#' risk_given_unexposed_stratum_2 <- 0.2
#' results <- lapply(
#'   1:10,
#'   function () {
#'     run_simulation(
#'       n = 100,
#'       probability_exposed,
#'       probability_stratum_1_given_exposed,
#'       probability_stratum_1_given_unexposed,
#'       risk_given_unexposed_stratum_2 = 0.2,
#'       relative_risk_exposed = 2,
#'       relative_risk_stratum_1 = 1.5
#'     )
#'   }
#' )
#' report_relative_bias_risk_ratio(results, 2)
report_relative_bias_risk_ratio <- function(
  simulation_results, true_risk_ratio
  ) {
  if (length(simulation_results) == 0) {
    return()
  }
  approaches <- names(simulation_results[[1]])
  relative_bias <- lapply(
    approaches,
    function(approach) {
      risk_ratio_estimates <- get_result_by_approach(
        simulation_results = simulation_results,
        result = "est",
        approach = approach
      )
      (
        (mean(risk_ratio_estimates, na.rm = TRUE) - true_risk_ratio)
        / true_risk_ratio * 100
      )
    }
  )
  names(relative_bias) <- approaches
  relative_bias
}

#' Compute the empirical coverage percentage of the confidence
#' intervals for the estimated risk ratio across a series of
#' simulation runs
#'
#' @param simulation_results A list of simulation run results produced
#'   by `run_simulation`
#' @param true_risk_ratio The true risk ratio for the association of
#'   exposure with the outcome used in the simulations.
#'
#' @return The empirical coverage percentage of the confidence
#'   intervals for the estimated risk across the simulation runs.
#' @export
#'
#' @examples
#' probability_exposed <- 0.5
#' probability_stratum_1_given_exposed <- 0.6
#' probability_stratum_1_given_unexposed <- 0.4
#' risk_given_unexposed_stratum_2 <- 0.2
#' results <- lapply(
#'   1:10,
#'   function () {
#'     run_simulation(
#'       n = 100,
#'       probability_exposed,
#'       probability_stratum_1_given_exposed,
#'       probability_stratum_1_given_unexposed,
#'       risk_given_unexposed_stratum_2 = 0.2,
#'       relative_risk_exposed = 2,
#'       relative_risk_stratum_1 = 1.5
#'     )
#'   }
#' )
#' report_coverage_risk_ratio(results, 2)
report_coverage_risk_ratio <- function(
  simulation_results, true_risk_ratio
  ) {
  if (length(simulation_results) == 0) {
    return()
  }
  approaches <- names(simulation_results[[1]])
  empirical_coverage <- lapply(
    approaches,
    function(approach) {
      risk_ratio_confidence_intervals <- get_result_by_approach(
        simulation_results = simulation_results,
        result = "ci",
        approach = approach
      )
      mean(
        apply(
          risk_ratio_confidence_intervals,
          2,
          is_true_value_covered,
          true_risk_ratio
        ),
        na.rm = TRUE
      )
    }
  )
  names(empirical_coverage) <- approaches
  empirical_coverage
}

#' Get a component of the results from a single simulation run for a specified approach from a list of simulation results
#'
#' @param simulation_results A list of simulation run results produced
#'   by `run_simulation`
#' @param result A string matching the name of one of the entries of
#'   the list returned by any of the `run_*` functions.
#' @param approach A string matching the name of one of the analysis
#'   approaches.
#'
#' @return A vector of the results.
#' @export
#'
#' @examples
#' probability_exposed <- 0.5
#' probability_stratum_1_given_exposed <- 0.6
#' probability_stratum_1_given_unexposed <- 0.4
#' risk_given_unexposed_stratum_2 <- 0.2
#' results <- lapply(
#'   1:10,
#'   function () {
#'     run_simulation(
#'       n = 100,
#'       probability_exposed,
#'       probability_stratum_1_given_exposed,
#'       probability_stratum_1_given_unexposed,
#'       risk_given_unexposed_stratum_2 = 0.2,
#'       relative_risk_exposed = 2,
#'       relative_risk_stratum_1 = 1.5
#'     )
#'   }
#' )
#' get_result_by_approach(results, "est", "modified_poisson_glm")
get_result_by_approach <- function(
  simulation_results,
  result,
  approach
  ) {
  sapply(
    simulation_results,
    function(simulation_result) {
      simulation_result[[approach]][[result]]
    }
  )
}
