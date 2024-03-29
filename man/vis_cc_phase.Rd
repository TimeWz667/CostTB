% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vis_when.R
\name{vis_cc_phase}
\alias{vis_cc_phase}
\alias{vis_acc_by_time}
\alias{vis_acc_by_pr}
\alias{vis_ts_by_time}
\alias{vis_ts_by_pr}
\title{Visualise time to catastropoic patient costs}
\usage{
vis_cc_phase(ttcc, fill = F,
  ylab = "Patients with catastrophic costs (\%)")

vis_acc_by_time(ttcc, xlab = "Time since treatment initiation (month)",
  ylab = "Proportaion of patients with catastrophic costs (\%)")

vis_acc_by_pr(ttcc, xlab = "Time since treatment initiation (month)",
  ylab = "Percentage of time in each phase")

vis_ts_by_time(ttcc, xlab = "Time since treatment initiation (month)",
  ylab1 = "Proportaion of patients with catastrophic costs (\%)",
  ylab2 = "Patient costs over 0.2 annuel household income")

vis_ts_by_pr(ttcc, xlab = "Percentage of time in each phase",
  ylab1 = "Proportaion of patients with catastrophic costs (\%)",
  ylab2 = "Patient costs over 0.2 annuel household income")
}
\arguments{
\item{ttcc}{A TimeToCataCosts object}

\item{fill}{fill stacked bar to one or not}

\item{ylab}{label for y axis}
}
\value{
stacked bar plot of CC occurance
}
\description{
Visualise time to catastropoic patient costs
}
\examples{
Costs <- as_costs(Survey)
TTCC <- calc_time_to_cc(Costs, sp = "all spe", indirect = "hc")

vis_cc_phase(TTCC, fill = F)
vis_cc_phase(TTCC, fill = T)

}
