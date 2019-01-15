# Function: Bin Calculations
#
# Author: taylor.curley@gatech.edu
# Last edited: December 22, 2018
#
# This function bins judgments for further type-2 analyses.
#
# You must specify the name of the dataset ("data"), the name of the variable being binned ("varname"),
# the outcome variable that goes into the calculation of the bins ("outcomevar"), and the number of
# bins that you would like to calculated ("bins").
#
# The output is a table of the bins with hits, false alarms, and their estimates.

bin_calculations <- function(data, varname, outcomevar, bins){

  data$bin <- cut(data[[varname]], breaks = c(seq(0, 100, by = 100/bins)), labels = 1:bins)
  data$bin[data[[varname]]==0] <- 1

  corr_recog <- length(data[[outcomevar]][data[[outcomevar]]==1])
  incorr_recog <- length(data[[outcomevar]][data[[outcomevar]]==0])

  type2_bins <- matrix(nrow = bins, ncol = 7)
  for (x in 1:bins){
    bin <- as.double(unlist(subset(data, bin == x, select = c(outcomevar))))
    hit <- length(bin[bin==1])
    miss <- length(bin[bin==0])
    if (x > 1){
      row <- c(x, hit, miss, (hit/corr_recog), (miss/incorr_recog), (type2_bins[x-1,6])-(type2_bins[x-1,4]),
               (type2_bins[x-1,7])-(type2_bins[x-1,5]))
    } else {
      row <- c(x, hit, miss, (hit/corr_recog), (miss/incorr_recog),1,1)
    }
    type2_bins[x,] <- row
  }

  colnames(type2_bins) <- c("Bin","Hit","Miss","HR","FAR","cumHR","cumFAR")
  type2_bins <- as.data.frame(type2_bins)
  return(type2_bins)
}
