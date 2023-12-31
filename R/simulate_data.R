#' Simulate a data set according to Zou (2004, p 703)
#'
#' @param n The number of observations in the data set.
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
#' @return A data frame.
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
simulate_data <- function(
  n,
  probability_exposed,
  probability_stratum_1_given_exposed,
  probability_stratum_1_given_unexposed,
  risk_given_unexposed_stratum_2,
  relative_risk_exposed,
  relative_risk_stratum_1
  ) {
  exposed <- rbinom(n, 1, probability_exposed)
  stratum <- ifelse(
    exposed == 0,
    ifelse(runif(n) < probability_stratum_1_given_unexposed, 1, 2),
    ifelse(runif(n) < probability_stratum_1_given_exposed, 1, 2)
  )
  risk <- compute_risk(
    exposed,
    stratum,
    risk_given_unexposed_stratum_2,
    relative_risk_exposed,
    relative_risk_stratum_1
  )
  outcome <- rbinom(n, 1, risk)
  data.frame(
    id = seq_len(n),
    exposed,
    stratum,
    risk,
    outcome
  )
}

#' Compute the risk of the outcome given an observation's exposure and stratum along with the baseline risk and the relative risks of exposure and belonging to stratum 1.
#'
#' @param exposure A vector of exposure/treatment indicators.  Assumes
#'   that 1 represents exposed/treated and 0 represents
#'   unexposed/control.
#' @param stratum A vector of stratum membership indicators.  Assumes
#'   that strata are coded 1 and 2.
#' @param risk_given_unexposed_stratum_2 The risk of the outcome for
#'   observations that are unexposed and in the second stratum.
#' @param relative_risk_exposed The relative risk of the outcome
#'   associated with exposure.
#' @param relative_risk_stratum_1 The relative risk of the outcome
#'   associated with the first stratum.
#'
#' @return The risk, i.e., the probability of experiencing the
#'   outcome.
#' @export
#'
#' @examples
#' risk_given_unexposed_stratum_2 <- 0.2
#' relative_risk_exposed <- 2
#' relative_risk_stratum_1 <- 1.5
#' data <- expand.grid(
#'   exposed = 1:0,
#'   stratum = 1:2
#' )
#' compute_risk(
#'   data$exposed,
#'   data$stratum,
#'   risk_given_unexposed_stratum_2,
#'   relative_risk_exposed,
#'   relative_risk_stratum_1
#' )
compute_risk <- function(
  exposure,
  stratum,
  risk_given_unexposed_stratum_2,
  relative_risk_exposed,
  relative_risk_stratum_1
  ) {
  risk <- (
    (exposure == 0 & stratum == 2) * risk_given_unexposed_stratum_2
    + (
      (exposure == 1 & stratum == 2) * risk_given_unexposed_stratum_2
      * relative_risk_exposed
    )
    + (
      (exposure == 0 & stratum == 1) * risk_given_unexposed_stratum_2
      * relative_risk_stratum_1
    )
    + (
      (exposure == 1 & stratum == 1) * risk_given_unexposed_stratum_2
      * relative_risk_exposed * relative_risk_stratum_1
    )
  )
  risk
}
