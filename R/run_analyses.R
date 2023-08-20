#' Run the Mantel-Haenszel procedure estimating the risk ratio associated with a (potentially stratified) exposure/treatment
#'
#' @param exposure A vector of exposure/treatment indicators.  Assumes
#'   that 1 represents exposed/treated and 0 represents
#'   unexposed/control.
#' @param outcome A vector of outcome indicators.  Assumes that 1
#'   represents the occurrence of the event of interest and 0
#'   represents no event.
#' @param stratum A vector indicating stratum membership.  Default
#'   `NULL` means the data are not stratified.
#' @param conf_level The confidence level of the interval.
#'
#' @return A list with elements `est` containing the estimated risk
#'   ratio and `ci` containing the limits of the 100 * (1 -
#'   conf_level)% confidence interval.
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
#' run_mantel_haenszel(
#'   exposure = sim_data$exposed,
#'   outcome = sim_data$outcome
#' )
#' run_mantel_haenszel(
#'   exposure = sim_data$exposed,
#'   outcome = sim_data$outcome,
#'   stratum = sim_data$stratum
#' )
#' run_mantel_haenszel(
#'   exposure = sim_data$exposed,
#'   outcome = sim_data$outcome,
#'   stratum = sim_data$stratum,
#'   conf_level = 0.99
#' )
run_mantel_haenszel <- function(
  exposure, outcome, stratum = NULL, conf_level = 0.95
  ) {
  if (is.null(stratum)) {
    result <- metafor::rma.mh(
      ai = sum(outcome == 1 & exposure == 1),
      bi = sum(outcome == 0 & exposure == 1),
      ci = sum(outcome == 1 & exposure == 0),
      di = sum(outcome == 0 & exposure == 0),
      measure = "RR",
      level = conf_level * 100
    )
  } else {
    result <- metafor::rma.mh(
      ai = tapply(outcome == 1 & exposure == 1, stratum, sum),
      bi = tapply(outcome == 0 & exposure == 1, stratum, sum),
      ci = tapply(outcome == 1 & exposure == 0, stratum, sum),
      di = tapply(outcome == 0 & exposure == 0, stratum, sum),
      measure = "RR",
      level = conf_level * 100
    )
  }
  list(
    est = exp(unname(result$beta)),
    ci = exp(c(result$ci.lb, result$ci.ub))
  )
}
