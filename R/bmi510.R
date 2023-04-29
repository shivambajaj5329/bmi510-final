library(pwr)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Randomly sample elements or rows from a vector or data frame
#'
#' This function is a wrapper around the \code{sample} function that can handle both atomic vectors and data frames as input. It randomly samples either \code{n} elements from an atomic vector or \code{n} rows from a data frame, with or without replacement.
#'
#' @param x A vector or data frame to sample from
#' @param n The number of samples or rows to select (default = 1)
#' @param replace Logical. Should sampling be done with replacement? (default = TRUE)
#'
#' @return A vector or data frame containing the sampled elements or rows
#'
#' @examples
#' rando(1:10, n = 5)
#' rando(mtcars, n = 3)
#'
#' @export
rando <- function(x, n = 1, replace = TRUE) {
  if (is.atomic(x)) {
    return(sample(x, n, replace))
  } else if (is.data.frame(x)) {
    return(x[sample(nrow(x), n, replace), ])
  } else {
    stop("Input must be an atomic vector or data frame")
  }
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Check if values in a vector are equal to the minimum value
#'
#' This function accepts an atomic vector \code{x} and returns a logical vector with \code{TRUE} values where \code{x} equals its minimum value.
#'
#' @param x An atomic vector to check
#' @param na.rm Logical. Should missing values be removed? (default = TRUE)
#'
#' @return A logical vector with \code{TRUE} values where \code{x} equals its minimum value
#'
#' @examples
#' is_min(c(3, 5, 1, 3, 2))
#' is_min(c(3, 5, NA, 3, 2))
#'
#' @export
is_min <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x == min(x)
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Check if values in a vector are equal to the maximum value
#'
#' This function accepts an atomic vector \code{x} and returns a logical vector with \code{TRUE} values where \code{x} equals its maximum value.
#'
#' @param x An atomic vector to check
#' @param na.rm Logical. Should missing values be removed? (default = TRUE)
#'
#' @return A logical vector with \code{TRUE} values where \code{x} equals its maximum value
#'
#' @examples
#' is_max(c(3, 5, 1, 3, 2))
#' is_max(c(3, 5, NA, 3, 2))
#'
#' @export
is_max <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x == max(x)
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Replicate a matrix or data frame by repeating rows or columns
#'
#' This function replicates a matrix or data frame \code{x} by repeating its rows \code{M} times or its columns \code{N} times, depending on the parameters passed.
#'
#' @param x A matrix or data frame to replicate
#' @param M The number of times to repeat the rows of \code{x} (default = 1)
#' @param N The number of times to repeat the columns of \code{x} (default = 1)
#'
#' @return A matrix with rows and/or columns replicated M and/or N times
#'
#' @examples
#' rep_mat(mtcars, 2, 1)
#' rep_mat(1:5, 3, 2)
#'
#' @export
rep_mat <- function(x, M = 1, N = 1) {
  if (!is.data.frame(x) && !is.matrix(x)) {
    stop("Input must be a matrix or data frame")
  }
  
  if (M <= 0 || N <= 0) {
    stop("M and N must be positive integers")
  }
  
  if (M == 1 && N == 1) {
    return(x)
  }
  
  if (M > 1 && N == 1) {
    return(x[rep(1:nrow(x), each = M), ])
  }
  
  if (M == 1 && N > 1) {
    return(x[, rep(1:ncol(x), each = N)])
  }
  
  if (M > 1 && N > 1) {
    return(rep_mat(rep_mat(x, M, 1), 1, N))
  }
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Get the classes of variables in a tibble
#'
#' This function takes a tibble \code{x} and returns a character vector containing the classes of each variable in the tibble.
#'
#' @param x A tibble to get the variable classes from
#'
#' @return A character vector containing the classes of each variable in the tibble
#'
#' @examples
#' classes(mtcars)
#'
#' @import tibble
#' @export
classes <- function(x) {
  if (!inherits(x, "tbl_df")) {
    stop("Input must be a tibble")
  }
  
  return(sapply(x, class))
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Scale the numeric variables in a tibble
#'
#' This function takes a tibble \code{x} and scales the numeric variables. By default, the variables are centered and scaled to have unit variance. The function returns a tibble with the same attributes as the input tibble.
#'
#' @param x A tibble to scale
#' @param center Logical. Should the variables be centered? (default = TRUE)
#' @param scale Logical. Should the variables be scaled to have unit variance? (default = TRUE)
#'
#' @return A tibble with the numeric variables centered and/or scaled
#'
#' @examples
#' df_scale(mtcars)
#'
#' @importFrom tibble tibble
#' @importFrom stats scale
#' @export
df_scale <- function(x, center = TRUE, scale = TRUE) {
  if (!inherits(x, "tbl_df")) {
    stop("Input must be a tibble")
  }
  
  num_vars <- sapply(x, is.numeric)
  
  if (!any(num_vars)) {
    return(x)
  }
  
  scaled_vars <- scale(x[, num_vars], center = center, scale = scale)
  
  new_x <- x
  
  new_x[, num_vars] <- scaled_vars
  
  return(new_x)
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate the log-likelihood of a sample under the normal distribution
#'
#' This function calculates the log-likelihood of a sample \code{x} under the normal distribution, with mean \code{mean} and standard deviation \code{sd}.
#'
#' @param x A numeric vector of data
#' @param mean The mean of the normal distribution
#' @param sd The standard deviation of the normal distribution
#'
#' @return The log-likelihood of \code{x} under the normal distribution
#'
#' @examples
#' log_likelihood_norm(rnorm(100), 0, 1)
#'
#' @importFrom stats dnorm
#' @export
log_likelihood_norm <- function(x, mean, sd) {
  sum(log(dnorm(x, mean, sd)))
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate the log-likelihood of a sample under the uniform distribution
#'
#' This function calculates the log-likelihood of a sample \code{x} under the uniform distribution, with minimum value \code{min} and maximum value \code{max}.
#'
#' @param x A numeric vector of data
#' @param min The minimum value of the uniform distribution
#' @param max The maximum value of the uniform distribution
#'
#' @return The log-likelihood of \code{x} under the uniform distribution
#'
#' @examples
#' log_likelihood_unif(runif(100), 0, 1)
#'
#' @importFrom stats dunif
#' @export
log_likelihood_unif <- function(x, min, max) {
  sum(log(dunif(x, min, max)))
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate the log-likelihood of a sample under the chi-squared distribution
#'
#' This function calculates the log-likelihood of a sample \code{x} under the chi-squared distribution, with degrees of freedom \code{df}.
#'
#' @param x A numeric vector of data
#' @param df The degrees of freedom of the chi-squared distribution
#'
#' @return The log-likelihood of \code{x} under the chi-squared distribution
#'
#' @examples
#' log_likelihood_chisq(rchisq(100, 5), 5)
#'
#' @importFrom stats dchisq
#' @export
log_likelihood_chisq <- function(x, df) {
  sum(log(dchisq(x, df)))
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate the log-likelihood of a sample under the F distribution
#'
#' This function calculates the log-likelihood of a sample \code{x} under the F distribution, with degrees of freedom \code{df1} and \code{df2}.
#'
#' @param x A numeric vector of data
#' @param df1 The degrees of freedom of the numerator of the F distribution
#' @param df2 The degrees of freedom of the denominator of the F distribution
#'
#' @return The log-likelihood of \code{x} under the F distribution
#'
#' @examples
#' log_likelihood_f(rf(100, 5, 10), 5, 10)
#'
#' @importFrom stats df
#' @export
log_likelihood_f <- function(x, df1, df2) {
  sum(log(df(x, df1, df2)))
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate the log-likelihood of a sample under the t distribution
#'
#' This function calculates the log-likelihood of a sample \code{x} under the t distribution, with degrees of freedom 
#' @param x A numeric vector of data
#' @param df The degrees of freedom of the t distribution
#'
#' @return The log-likelihood of \code{x} under the t distribution
#'
#' @examples
#' log_likelihood_t(rt(100, 5), 5)
#'
#' @importFrom stats dt
#' @export
log_likelihood_t <- function(x, df) {
  sum(log(dt(x, df)))
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate sensitivity of a binary classification model
#'
#' This function calculates the sensitivity of a binary classification model based on the predicted labels \code{pred} and the true labels \code{truth}.
#'
#' @param pred A binary vector of predicted labels
#' @param truth A binary vector of true labels
#'
#' @return The sensitivity of the binary classification model
#'
#' @examples
#' sensitivity(c(1,1,0,1), c(1,0,0,1))
#'
#' @export
sensitivity <- function(pred, truth) {
  tp <- sum(pred == 1 & truth == 1)
  fn <- sum(pred == 0 & truth == 1)
  return(tp / (tp + fn))
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate specificity of a binary classification model
#'
#' This function calculates the specificity of a binary classification model based on the predicted labels \code{pred} and the true labels \code{truth}.
#'
#' @param pred A binary vector of predicted labels
#' @param truth A binary vector of true labels
#'
#' @return The specificity of the binary classification model
#'
#' @examples
#' specificity(c(1,1,0,1), c(1,0,0,1))
#'
#' @export
specificity <- function(pred, truth) {
  tn <- sum(pred == 0 & truth == 0)
  fp <- sum(pred == 1 & truth == 0)
  return(tn / (tn + fp))
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' Calculate precision of a binary classification model
#'
#' This function calculates the precision of a binary classification model based on the predicted labels \code{pred} and the true labels \code{truth}.
#'
#' @param pred A binary vector of predicted labels
#' @param truth A binary vector of true labels
#'
#' @return The precision of the binary classification model
#'
#' @examples
#' precision(c(1,1,0,1), c(1,0,0,1))
#'
#' @export
precision <- function(pred, truth) {
  tp <- sum(pred == 1 & truth == 1)
  fp <- sum(pred == 1 & truth == 0)
  return(tp / (tp + fp))
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate recall of a binary classification model
#'
#' This function calculates the recall (sensitivity) of a binary classification model based on the predicted labels \code{pred} and the true labels \code{truth}.
#'
#' @param pred A binary vector of predicted labels
#' @param truth A binary vector of true labels
#'
#' @return The recall (sensitivity) of the binary classification model
#'
#' @examples
#' recall(c(1,1,0,1), c(1,0,0,1))
#'
#' @export
recall <- function(pred, truth) {
  tp <- sum(pred == 1 & truth == 1)
  fn <- sum(pred == 0 & truth == 1)
  return(tp / (tp + fn))
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate accuracy of a binary classification model
#'
#' This function calculates the accuracy of a binary classification model based on the predicted labels \code{pred} and the true labels \code{truth}.
#'
#' @param pred A binary vector of predicted labels
#' @param truth A binary vector of true labels
#'
#' @return The accuracy of the binary classification model
#'
#' @examples
#' accuracy(c(1,0,0,0), c(1,0,0,1))
#' @export
accuracy <- function(pred, truth) {
return(mean(pred == truth))
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate F1 score of a binary classification model
#'
#' This function calculates the F1 score of a binary classification model based on the predicted labels \code{pred} and the true labels \code{truth}.
#'
#' @param pred A binary vector of predicted labels
#' @param truth A binary vector of true labels
#'
#' @return The F1 score of the binary classification model
#'
#' @examples
#' f1(c(1,1,0,1), c(1,0,0,1))
#'
#' @export
f1 <- function(pred, truth) {
  p <- precision(pred, truth)
  r <- recall(pred, truth)
  return(2 * p * r / (p + r))
}
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate the minimum n per group needed for a two-sample t-test
#'
#' This function calculates the minimum sample size per group needed for a two-sample t-test, given the expected effect size \code{d} and the desired statistical power \code{power}.
#'
#' @param d The expected Cohen's d
#' @param power The desired statistical power (default 0.8)
#'
#' @return The minimum sample size per group needed for a two-sample t-test
#'
#' @examples
#' minimum_n_per_group(0.5)
#'
#' @importFrom pwr pwr.t.test
#' @export
minimum_n_per_group <- function(d, power = 0.8) {
  alpha <- 0.05
  n <- pwr.t.test(d = d, power = power, sig.level = alpha, type = "two.sample")$n
  return(ceiling(n))
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate R-squared between predicted and true values
#'
#' This function calculates the R-squared statistic between a vector of predicted values \code{pred} and a vector of true values \code{truth}.
#'
#' @param pred A vector of predicted values
#' @param truth A vector of true values
#'
#' @return The R-squared statistic between \code{pred} and \code{truth}
#'
#' @examples
#' r2(c(1,2,3,4), c(1,3,2,5))
#'
#' @export
r2 <- function(pred, truth) {
  SSres <- sum((truth - pred)^2)
  SStot <- sum((truth - mean(truth))^2)
  return(1 - SSres/SStot)
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Calculate adjusted R-squared between predicted and true values
#'
#' This function calculates the adjusted R-squared statistic between a vector of predicted values \code{pred} and a vector of true values \code{truth}, given the number of model parameters \code{n_p}.
#'
#' @param pred A vector of predicted values
#' @param truth A vector of true values
#' @param n_p The number of model parameters, excluding the intercept
#'
#' @return The adjusted R-squared statistic between \code{pred} and \code{truth}
#'
#' @examples
#' adj_R2(c(1,2,3,4), c(1,3,2,5), 1)
#'
#' @export
adj_R2 <- function(pred, truth, n_p) {
  r2_val <- r2(pred, truth)
  n <- length(truth)
  adj_r2 <- 1 - ((1 - r2_val) * (n - 1)) / (n - n_p - 1)
  return(adj_r2)
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

