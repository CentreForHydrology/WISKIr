#' Converts a WISKI time series to an obs dataframe
#' @description Converts the WISKI date/time to a CRHM datetime. If possible, it will also convert the variable name to a CRHM variable name. Note that this may be incorrect, as it is attempting to infer the variable from its units.
#' @param WISKItimeseries Required. Dataframe containing the WISKI time series, as returned by get_WISKI_timeseries_values.
#' @param timezone Optional. The name of the timezone of the data as a character string. This should be YOUR timezone. The default value is "", which is your timezone. You may find a difference in the seconds between using "" and the your timezone. Note that the timezone code is specific to your OS. Under Windows, the code for Central Standard Time is 'America/Regina'. Under Linux, it is 'CST'.
#' @return Returnes a standard CRHM obs dataframe. Note that the WISKI quality code is removed.
#' @author Kevin Shook
#' @seealso \code{\link{get_WISKI_timeseries_values}}
#' @examples
#' WISKI.t <- WISKItoObs(WISKI.timeseries.values)
#' @export


WISKItoObs <- function(WISKItimeseries, timezone=''){
  # converts a WISKI timeseries object to a CRHM obs object
  obs.name <- names(WISKItimeseries)[2]
  
  if (str_detect(obs.name, ignore.case('degree_Celsius')))
    var.name <- 't.1'
  else if (str_detect(obs.name, ignore.case('percent')))
    var.name <- 'rh.1'
  else if (str_detect(obs.name, ignore.case('millim')))
    var.name <- 'p.1' 
  else if (str_detect(obs.name, ignore.case('met')))
    var.name <- 'u.1'
  else{
    cat(obs.name,' not a recognised obs variable\n', sep='')
    var.name <- obs.name
  }
  
  obs <- WISKItimeseries[,c(1,2)]
  names(obs) <- c('datetime', var.name)
  obs$datetime <- as.POSIXct(obs$datetime, tz=timezone)
  return(obs)
}