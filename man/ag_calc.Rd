\name{ag_calc}
\alias{ag_calc}
\title{A_g statistic calculation}
\description{
This function takes a dataframe with a to-be-binned variable along with the outcome variable by which the type-2 calculations are based on and returns the A_g statistic described by Masson and Rotello (2009)
}
\usage{
ag_calc(data, varname, outcomevar, bins)
}
\arguments{
  \item{data}{a dataframe that holds the judgment and outcome variables.}
  \item{varname}{the name of the variable that is to be binned.}
  \item{outcomevar}{the name of the outcome variable that the hit and false alarm bin calculations are based on.}
  \item{bins}{a positive integer indicating the number of bins for the calculations.}}
\examples{
# Randomly select 100 integers from 0 - 100.
set.seed(2)
a <- sample(0:100,100,replace=TRUE)

# Randomly select 100 integers of 0 or 1.
set.seed(2)
b <- sample(0:1,100,replace=TRUE)

# Send to data frame.
c <- data.frame(a,b)

# Send to function; output A_g statistic
ag_calc(c, varname = "a", outcomevar = "b", bins = 5)
}
