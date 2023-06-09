% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/milk_protein_content.R
\name{milk_protein_content}
\alias{milk_protein_content}
\title{Estimates the daily protein content in the milk.}
\usage{
milk_protein_content(parity, days_milk, lambda_prot)
}
\arguments{
\item{parity}{Parity: primiparous, secondiparous, or multiparous.}

\item{days_milk}{Days in milk.}

\item{lambda_prot}{Hyperparameter to adjust the milk protein content for a specific average value.}
}
\value{
Daily milk protein content (\%).
}
\description{
Estimates the daily protein content in the milk.
}
\examples{
milk_protein_content(parity = "multiparous", days_milk = 60, lambda_prot = 1)

}
