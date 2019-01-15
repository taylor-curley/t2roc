higham_gamma <- function(data, varname, outcomevar, bins){

  ag <- ag_calc(data, varname, outcomevar, bins)

  hgamma <- 2*ag-1

  return(hgamma)
}
