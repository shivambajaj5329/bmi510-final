# BMI 510 Final Project

Author: Shivam Bajaj  
Reviewers: Dr. Lucas McKay, Sarthak Sathpathy

This repository contains the R package developed for the BMI 510 final project. The package includes a collection of 20 functions designed to perform various operations and analyses.

## Implemented Functions

1. rando - A wrapper around sample that tests whether x is an atomic vector or dataframe-like object and then returns either n samples or n rows as appropriate.
2. is_min = function(x, na.rm = T) - Accepts an atomic vector x and returns a logical with TRUE where x equals its minimum value.
3. is_max = function(x, na.rm = T) - Accepts an atomic vector x and returns a logical with TRUE where x equals its maximum value.
4. rep_mat = function(x, M=1, N=1) - Port of repmat.m in Matlab. Accepts a dataframe or matrix and x and returns a matrix created by replicating the rows (or columns) M (N) times.
5. classes = function(x) - Returns a character vector containing the classes of each variable in a tibble x. (Similar to names.)
6. df_scale = function(x, center = T, scale = T) - Returns a tibble x in which the numeric variables have been scaled with scale. It is not necessary to retain the variable attributes, but it is more useful if you do. (Uses answer from 5.)
7. log_likelihood_norm(x, mean, sd) - Returns the log-likelihood of a sample x under the normal, uniform, chi-squared, f, or t densities, with parameterizations as described in ?dnorm, etc. 
8. log_likelihood_unif(x, min, max) - Returns the log-likelihood of a sample x under the uniform densities, with parameterizations as described in ?dnorm, etc. 
9. log_likelihood_chisq(x, df) - Returns the log-likelihood of a sample x under the chi-squared densities, with parameterizations as described in ?dnorm, etc. 
10. log_likelihood_f(x, df1, df2) - Returns the log-likelihood of a sample x under the f densities, with parameterizations as described in ?dnorm, etc. 
11. log_likelihood_t(x, df) - Returns the log-likelihood of a sample x under the t densities, with parameterizations as described in ?dnorm, etc. 
12. sensitivity(pred,truth) - Returns the sensitivity of your model based on predicted values and ground truth
13. specificity(pred,truth) - Returns the specificity of your model based on predicted values and ground truth
14. precision(pred,truth) - Returns the precision of your model based on predicted values and ground truth
15. recall(pred,truth) - Returns the recall of your model based on predicted values and ground truth
16. accuracy(pred,truth) - Returns the accuracy of your model based on predicted values and ground truth
17. f1(pred,truth) - Returns the f1 score of your model based on predicted values and ground truth
18. minimum_n_per_group(d,power=0.8) - Return the minimum n per group needed for a two-sample t-test. d is the expected Cohen’s d.
19. r2(pred,truth) - Calculate the r-squared statistic between predicted and ground truth continuous variables.
20. adjR2(pred,truth,n_p) - Calculate the adjusted r-squared statistic between predicted and ground truth continuous variables. N_p is the number of model parameters, excluding the intercept.

## Installation and Usage

To use this package in your project, follow these steps:

### Step 1 - Clone the Repository

Clone this repository to your project directory using the following command:

<pre>
```
git clone https://github.com/yourusername/bmi510-final.git
```
</pre>

### Step 2 - Source the Package

In your project's R Script, use this command to source the package:

```R
source("path/to/bmi510-final/package.R")
```

Make sure to replace path/to/bmi510-final with the actual path to the cloned repository in your project.

### Step 3 - Call Functions
You can now call the functions directly in your R script. If you need help understanding how to use the functions or the number of arguments to pass, run help(funcName) in your console or terminal.

### Contributing
If you would like to contribute to the development of this package, please follow these guidelines:
Step 1 - Fork the repository and create a new branch for your feature or bugfix.
Step 2 - Commit your changes to the new branch and push it to your forked repository.
Step 3 - Create a pull request with a clear description of your changes.
