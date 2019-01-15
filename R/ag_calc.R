# Function: A_g calculations
#
# Author: taylor.curley@gatech.edu
# Last edited: December 22, 2018
#
# This function computes the A_g statistic described by Masson & Rotello (2009)


ag_calc <- function(data, varname, outcomevar, bins){

  binned_data <- bin_calculations(data, varname, outcomevar, bins)

  ag <- 0
  for (x in head(unique(binned_data[["Bin"]]),-1)){
    ag <- ag + .5*((binned_data[["cumHR"]][x]+binned_data[["cumHR"]][x+1])*
                     (binned_data[["cumFAR"]][x]-binned_data[["cumFAR"]][x+1]))
  }

  return(ag)

}
