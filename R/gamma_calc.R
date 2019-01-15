gamma_calc <- function(data, varname, outcomevar, bins = NULL){

  require(Hmisc)

  if (is.null(bins)==TRUE){
    gamma_out <- rcorr.cens(data[[varname]], data[[outcomevar]], outx = TRUE)[2]
  } else if (bins == max(length(unique(data[[varname]])))){
    data$bin <- as.numeric(as.factor(data[[varname]]))
    gamma_out <- rcorr.cens(data[["bin"]], data[[outcomevar]], outx = TRUE)[2]
  } else {
    data$bin <- as.numeric(cut(data[[varname]], bins))
    gamma_out <- rcorr.cens(data[["bin"]], data[[outcomevar]], outx = TRUE)[2]
  }

  return(gamma_out)
}
