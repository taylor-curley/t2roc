az_calc <- function(data, varname, outcomevar, bins){

  binned_data <- bin_calculations(data, varname, outcomevar, bins)

  z_trans <- scale(c(binned_data[["cumHR"]],binned_data[["cumFAR"]]), center = TRUE, scale = TRUE)
  binned_data$z_cumHR <- z_trans[1:(length(z_trans)/2)]
  binned_data$z_cumFAR <- z_trans[((length(z_trans)/2)+1):length(z_trans)]

  fit <- lm( binned_data[["z_cumHR"]]~ binned_data[["z_cumFAR"]])
  az <- as.numeric(fit$coefficients[1])/sqrt(1+(as.numeric(fit$coefficients[2])^2))

  return(az)
}
