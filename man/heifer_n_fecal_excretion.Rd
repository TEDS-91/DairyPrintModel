% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nitrogen_excretion.R
\name{heifer_n_fecal_excretion}
\alias{heifer_n_fecal_excretion}
\title{Estimates the daily fecal nitrogen excretion of heifers.}
\usage{
heifer_n_fecal_excretion(nitrogen_intake)
}
\arguments{
\item{nitrogen_intake}{Nitrogen intake (g/day).}
}
\value{
Daily fecal nitrogen excretion (g).
}
\description{
Estimates the daily fecal nitrogen excretion of heifers.
}
\examples{
heifer_n_fecal_excretion(nitrogen_intake = 120)

}
\seealso{
The equations used here are also available in \href{https://www.journalofdairyscience.org/article/S0022-0302(15)00147-2/fulltext}{Reed et al. (2015)}.
}
