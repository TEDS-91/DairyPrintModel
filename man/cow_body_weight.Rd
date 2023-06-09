% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lactating_cow_body_weight.R
\name{cow_body_weight}
\alias{cow_body_weight}
\title{Estimates the accumulate body weight losses in an user defined range of days in milk.}
\usage{
cow_body_weight(min_value, max_value, parity)
}
\arguments{
\item{min_value}{First day for weight estimation.}

\item{max_value}{Day limit for weight estimation.}

\item{parity}{Parity: primiparous or multiparous.}
}
\value{
Accumulate body weight losses (kg).
}
\description{
Estimates the accumulate body weight losses in an user defined range of days in milk.
}
\examples{
cow_body_weight(min_value = 1, max_value = 30, parity = "multiparous")
}
