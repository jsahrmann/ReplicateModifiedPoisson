#' Run a single simulation
#'
#' @param n The number of observations in the simulated data set.
#' @param probability_exposed The probability that an observation is
#'   in the 'exposed' group
#' @param probability_stratum_1_given_exposed The conditional
#'   probability that an observation is in the first stratum given
#'   that the observation is in the exposed group.
#' @param probability_stratum_1_given_unexposed The conditional
#'   probability that an observation is in the first stratum given
#'   that the observation is in the unexposed group.
#' @param risk_given_unexposed_stratum_2 The risk of the outcome for
#'   observations that are unexposed and in the second stratum.
#' @param relative_risk_exposed The relative risk of the outcome
#'   associated with exposure.
#' @param relative_risk_stratum_1 The relative risk of the outcome
#'   associated with the first stratum.
#'
#' @return A list with entries `poisson_glm`, `modified_poisson_glm`,
#'   `log_binomial_glm`, and `mantel_haenszel`, each a list with
#'   entries `est`, `ci`, `bias`, and `is_covered`.
#' @export
#'
#' @examples
#' probability_exposed <- 0.5
#' probability_stratum_1_given_exposed <- 0.6
#' probability_stratum_1_given_unexposed <- 0.4
#' risk_given_unexposed_stratum_2 <- 0.2
#' run_simulation(
#'   n = 100,
#'   probability_exposed,
#'   probability_stratum_1_given_exposed,
#'   probability_stratum_1_given_unexposed,
#'   risk_given_unexposed_stratum_2 = 0.2,
#'   relative_risk_exposed = 2,
#'   relative_risk_stratum_1 = 1.5
#' )
run_simulation <- function(
  n,
  probability_exposed,
  probability_stratum_1_given_exposed,
  probability_stratum_1_given_unexposed,
  risk_given_unexposed_stratum_2,
  relative_risk_exposed,
  relative_risk_stratum_1
  ) {
  # Generate data under the given conditions.
  simulated_data <- simulate_data(
    n,
    probability_exposed,
    probability_stratum_1_given_exposed,
    probability_stratum_1_given_unexposed,
    risk_given_unexposed_stratum_2,
    relative_risk_exposed,
    relative_risk_stratum_1
  )
  # Run analyses.
  results_poisson_glm <- run_poisson_glm(
    outcome ~ exposed + stratum, simulated_data)
  results_modified_poisson_glm <- run_modified_poisson_glm(
    outcome ~ exposed + stratum, simulated_data, simulated_data$id)
  results_log_binomial_glm <- run_log_binomial_glm(
    outcome ~ exposed + stratum, simulated_data)
  results_mantel_haenszel <- run_mantel_haenszel(
    simulated_data$exposed, simulated_data$outcome,
    simulated_data$stratum
  )
  # Organize the results in a list, and include evaluations of the
  # estimates.
  list(
    poisson_glm = list(
      est = results_poisson_glm$est,
      ci = results_poisson_glm$ci
    ),
    modified_poisson_glm = list(
      est = results_modified_poisson_glm$est,
      ci = results_modified_poisson_glm$ci
    ),
    log_binomial_glm = list(
      est = results_log_binomial_glm$est,
      ci = results_log_binomial_glm$ci
    ),
    mantel_haenszel = list(
      est = results_mantel_haenszel$est,
      ci = results_mantel_haenszel$ci
    )
  )
}
