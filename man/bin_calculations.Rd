\name{bin_calculations}
\alias{bin_calculations}
\title{Bin judgments and perform calculations}
\description{
This function takes a dataframe, bins the judgments by the number given to the program, and performs hit and false alarm rate calculations for the outcome variable according to each bin
}
\usage{
bin_calculations(data, varname, outcomevar, bins)
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

# Send to function; output bins + calculations
bin_calculations(c, varname = "a", outcomevar = "b", bins = 5)
}
