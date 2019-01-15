criterion <- function(data, varname, outcomevar, bins){

  binned_data <- bin_calculations(data, varname, outcomevar, bins)

  z_trans <- scale(c(binned_data[["cumHR"]],binned_data[["cumFAR"]]), center = TRUE, scale = TRUE)
  binned_data$z_cumHR <- z_trans[1:(length(z_trans)/2)]
  binned_data$z_cumFAR <- z_trans[((length(z_trans)/2)+1):length(z_trans)]

  criterion <- matrix(nrow=1,ncol=bins)
  for (j in 1:bins){
    criterion[j] <- -(binned_data[["z_cumHR"]][j]+binned_data[["z_cumFAR"]][j])/2
  }

  return(criterion)
}
