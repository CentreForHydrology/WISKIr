#' Converts a WISKI time series to an obs dataframe
#' @description Converts the WISKI date/time to a CRHM datetime. If possible, it will also convert the variable name to a CRHM variable name. Note that this may be incorrect, as it is attempting to infer the variable from its units.
#' @param WISKItimeseries Required. Dataframe containing the WISKI time series, as returned by getWISKIvalues.
#' @param timezone Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#' @param varName Optional. Name for the variable. If not specified (the default) then a name will be guessed from the units. Note that many different variables have the same units, so the guess might not be very good!
#' @author Kevin Shook
#' @seealso \code{\link{getWISKIvalues}}
#' @examples
#' FiseraTvalues <- getWISKIvalues('9296042','2013-01-01','2013-06-30')
#' FiseraTobs <- WISKItoObs(FiseraTvalues, timezone='Etc/GMT+7')
#' @export


WISKItoObs <- function(WISKItimeseries, timezone='', varName=''){
  
  if (timezone == ''){
    cat('Error: must specify a timezone\n')
    return(FALSE)
  }
  # converts a WISKI timeseries object to a CRHM obs object
  obs.name <- names(WISKItimeseries)[2]
  if (varName ==''){
    if (stringr::str_detect(obs.name, stringr::fixed('Celsius', ignore_case = TRUE)))
      var.name <- 't.1'
    else if (stringr::str_detect(obs.name, stringr::fixed('percent', ignore_case = TRUE)))
      var.name <- 'rh.1'
    else if (stringr::str_detect(obs.name, stringr::fixed('millim', ignore_case = TRUE)))
      var.name <- 'p.1' 
    else if (stringr::str_detect(obs.name, stringr::fixed('watt', ignore_case = TRUE)))
      var.name <- 'q.1'
    else if (stringr::str_detect(obs.name, stringr::fixed('meter per sec', ignore_case = TRUE)))
      var.name <- 'u.1'
    else{
      cat(obs.name,' not a recognised obs variable\n', sep='')
      var.name <- obs.name
    }
  }
  else{
    var.name <- varName
  }
  obs <- WISKItimeseries[,c(1,2)]
  names(obs) <- c('datetime', var.name)
  obs$datetime <- as.POSIXct(obs$datetime, tz=timezone)
  return(obs)
}