#' Compute the relative bias in the estimated risk ratio as a percentage of the true value
#'
#' @param estimated_risk_ratio The estimated risk ratio for the
#'   association of exposure with the outcome returned by any of the
#'   `run_*` analysis functions.
#' @param true_risk_ratio The true risk ratio for the association of
#'   exposure with the outcome.
#'
#' @return The percent relative bias of the estimated risk ratio.
#' @export
#'
#' @examples
#' probability_exposed <- 0.5
#' probability_stratum_1_given_exposed <- 0.6
#' probability_stratum_1_given_unexposed <- 0.4
#' risk_given_unexposed_stratum_2 <- 0.2
#' sim_data <- simulate_data(
#'   n = 100,
#'   probability_exposed,
#'   probability_stratum_1_given_exposed,
#'   probability_stratum_1_given_unexposed,
#'   risk_given_unexposed_stratum_2 = 0.2,
#'   relative_risk_exposed = 2,
#'   relative_risk_stratum_1 = 1.5
#' )
#' poisson_glm_fit <- run_poisson_glm(
#'   outcome ~ exposed + stratum, data = sim_data
#' )
#' compute_bias_risk_ratio(poisson_glm_fit$est, 2)
compute_bias_risk_ratio <- function(
  estimated_risk_ratio, true_risk_ratio
  ) {
  (estimated_risk_ratio - true_risk_ratio) / true_risk_ratio * 100
}

#' Is the true risk ratio contained in the given interval?
#'
#' @param confidence_interval The confidence interval of the risk
#'   ratio for the association of exposure with the outcome returned
#'   by any of the `run_*` analysis functions.
#' @param true_risk_ratio The true risk ratio for the association of
#'   exposure with the outcome.
#'
#' @return A Boolean indicating whether the estimate is contained in
#'   the interval.
#' @export
#'
#' @examples
#' probability_exposed <- 0.5
#' probability_stratum_1_given_exposed <- 0.6
#' probability_stratum_1_given_unexposed <- 0.4
#' risk_given_unexposed_stratum_2 <- 0.2
#' sim_data <- simulate_data(
#'   n = 100,
#'   probability_exposed,
#'   probability_stratum_1_given_exposed,
#'   probability_stratum_1_given_unexposed,
#'   risk_given_unexposed_stratum_2 = 0.2,
#'   relative_risk_exposed = 2,
#'   relative_risk_stratum_1 = 1.5
#' )
#' poisson_glm_fit <- run_poisson_glm(
#'   outcome ~ exposed + stratum, data = sim_data
#' )
#' is_true_value_covered(poisson_glm_fit$ci, 2)
is_true_value_covered <- function(
  confidence_interval, true_risk_ratio
  ) {
  (
    confidence_interval[[1]] < true_risk_ratio
    & true_risk_ratio < confidence_interval[[2]]
  )
}
