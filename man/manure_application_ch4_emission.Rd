% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manure_aplication_methane.R
\name{manure_application_ch4_emission}
\alias{manure_application_ch4_emission}
\title{Estimates the daily methane emissions from manure application (kg).}
\usage{
manure_application_ch4_emission(Fvfa, area_crop)
}
\arguments{
\item{Fvfa}{Daily concentration of VFAs in the slurry (mmol/kg slurry).}

\item{area_crop}{Land area (ha).}
}
\value{
Daily methane emissions from manure application (kg).
}
\description{
Estimates the daily methane emissions from manure application (kg).
}
\examples{
manure_application_ch4_emission(Fvfa = 1, area_crop = 10)

}
\seealso{
The equation used here is also available in \href{https://elibrary.asabe.org/abstract.asp?aid=27781}{Chianese et al. (2009)}.
}
