% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_analyses.R
\name{run_log_binomial_glm}
\alias{run_log_binomial_glm}
\title{Fit a log-binomial generalized linear model estimating the risk ratio associated with an exposure/treatment}
\usage{
run_log_binomial_glm(model_formula, data)
}
\arguments{
\item{model_formula}{An R formula specifying the model.  The
exposure/treatment of interest should be the first term on the
right-hand side.  (An intercept is assumed.)}

\item{data}{The data on which to fit the model.}
}
\value{
A list with elements \code{est} containing the estimated risk
ratio and \code{ci} containing the limits of the 95\% confidence
interval.
}
\description{
Fit a log-binomial generalized linear model estimating the risk ratio associated with an exposure/treatment
}
\examples{
probability_exposed <- 0.5
probability_stratum_1_given_exposed <- 0.6
probability_stratum_1_given_unexposed <- 0.4
risk_given_unexposed_stratum_2 <- 0.2
sim_data <- simulate_data(
  n = 100,
  probability_exposed,
  probability_stratum_1_given_exposed,
  probability_stratum_1_given_unexposed,
  risk_given_unexposed_stratum_2 = 0.2,
  relative_risk_exposed = 2,
  relative_risk_stratum_1 = 1.5
)
run_log_binomial_glm(
  outcome ~ exposed + stratum, data = sim_data
)
}
