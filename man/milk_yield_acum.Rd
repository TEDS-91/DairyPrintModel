% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/milk_yield.R
\name{milk_yield_acum}
\alias{milk_yield_acum}
\title{Estimates the total milk yield between two days in milk.}
\usage{
milk_yield_acum(min_value, max_value, parity, milk_freq, lambda_milk)
}
\arguments{
\item{min_value}{Initial day in milk.}

\item{max_value}{Final day in milk.}

\item{parity}{Parity: primiparous, secondiparous or multiparous.}

\item{milk_freq}{Observed milk yield (kg).}

\item{lambda_milk}{Hyper parameter to adjust the milk yield for a specific average value.}
}
\value{
Accumulated milk yield between two days in milk (kg).
}
\description{
Estimates the total milk yield between two days in milk.
}
\examples{
milk_yield_acum(min_value = 30,
                max_value = 60,
                parity = "multiparous",
                milk_freq = 2,
                lambda_milk = 2)
}
