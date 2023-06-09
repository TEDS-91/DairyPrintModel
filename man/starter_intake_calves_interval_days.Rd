% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/starter_intake_calves.R
\name{starter_intake_calves_interval_days}
\alias{starter_intake_calves_interval_days}
\title{Estimates the average calf starter intake in an user defined range of days.}
\usage{
starter_intake_calves_interval_days(day_min, day_max, milk_intake)
}
\arguments{
\item{day_min}{First day for starter intake estimation.}

\item{day_max}{Last day for starter intake estimation.}

\item{milk_intake}{Average milk intake (l).}
}
\value{
Average starter intake for the user defined period of days (kg/day).
}
\description{
Estimates the average calf starter intake in an user defined range of days.
}
\examples{
# Estimate the average starter intake of calves between 1 and 30 days consuming 6 l of milk
starter_intake_calves_interval_days(day_min = 1, day_max = 30, milk_intake = 6)

}
