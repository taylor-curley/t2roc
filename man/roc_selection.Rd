\name{roc_selection}
\alias{roc_selection}
\title{Calcuations useful for determining the best number of bins for a type-2 ROC}
\description{
This function takes a dataframe and performs a series of calcuations in a step-wise fashion according to a maximum, minimum, and step numbers of bins. The "optimal" number of bins for a type-2 ROC calcuation.
}
\usage{
roc_selection(data, varname, outcomevar, low_bin, high_bin, step_bin)
}
\arguments{
  \item{data}{a dataframe that holds the judgment and outcome variables.}
  \item{varname}{the name of the variable that is to be binned.}
  \item{outcomevar}{the name of the outcome variable that the hit and false alarm bin calculations are based on.}
  \item{low_bin}{a positive integer indicating the lowest number of bins for the calculations.}
  \item{high_bin}{a positive integer indicating the highest number of bins for the calculations.}
  \item{step_bin}{a positive integer indicating the step-wise progression through bin numbers.}}
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
roc_selection(c, varname = "a", outcomevar = "b", 2, 10, 1)
}
