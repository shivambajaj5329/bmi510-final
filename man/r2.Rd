% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.R
\name{r2}
\alias{r2}
\title{Calculate R-squared between predicted and true values}
\usage{
r2(pred, truth)
}
\arguments{
\item{pred}{A vector of predicted values}

\item{truth}{A vector of true values}
}
\value{
The R-squared statistic between \code{pred} and \code{truth}
}
\description{
This function calculates the R-squared statistic between a vector of predicted values \code{pred} and a vector of true values \code{truth}.
}
\examples{
r2(c(1,2,3,4), c(1,3,2,5))

}
