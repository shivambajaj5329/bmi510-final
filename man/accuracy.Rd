% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.R
\name{accuracy}
\alias{accuracy}
\title{Calculate accuracy of a binary classification model}
\usage{
accuracy(pred, truth)
}
\arguments{
\item{pred}{A binary vector of predicted labels}

\item{truth}{A binary vector of true labels}
}
\value{
The accuracy of the binary classification model
}
\description{
This function calculates the accuracy of a binary classification model based on the predicted labels \code{pred} and the true labels \code{truth}.
}
\examples{
accuracy(c(1,0,0,0), c(1,0,0,1))
}
