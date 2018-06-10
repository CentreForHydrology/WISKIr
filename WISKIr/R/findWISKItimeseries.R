#' Find all timeseries on WISKI web server for a specified station
#' @description Queries a WISKI web server to get a list of available time series which have the specified station name.
#' @param stationName Required. A character string containing the name of the station. May contain wildcard characters.
#' @param site.url Optional. A character string containing the url of the WISKI web server. Defaults to the Global Institute for Water Security (GIWS) server: \code{'http://giws.usask.ca:8080/'}. As this package is intended for use by the GIWS hydrological community, it is usually unnecessary to specify the web server.
#' @return If unsuccessful, returns \code{FALSE}. If successful, returns a dataframe containing 
#' \item{station_name}{Name of station, specified by \code{stationName}} 
#' \item{station_no}{WISKI number of station} 
#' \item{station_id}{WISKI station ID number} 
#' \item{ts_id}{WISKI time series ID number} 
#' \item{ts_name}{WISKI time series name} 
#' \item{parametertype_id}{WISKI ID number of parameter type of time series} 
#' \item{parametertype_name}{WISKI name of parameter type of time series}
#' \item{stationparameter_name}{Name of parameter in this station}
#' @author Kevin Shook
#' @seealso \code{\link{getWISKImetadata}} \code{\link{getWISKIvalues}}
#' @examples
#  finds all of the time series for Fisera Ridge
#' FiseraTimeseries <- findWISKItimeseries('Fisera*')
#' print(FiseraTimeseries)
#' @export

findWISKItimeseries <- function(stationName, site.url='http://giws.usask.ca:8080/'){
  # finds all time series for a given station - station name can include wild cards
  # returns data frame containing all available time series
  stock <- 'KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesList&datasource=0&format=ascii&station_name='
  stock2 <- "&returnfields=station_name,station_no,ts_id,ts_name,parametertype_id,parametertype_name,stationparameter_name"
  # check parameters
  if (stationName == '') {
    cat("Error: missing stationName'\n")
    return(FALSE)
  }
  
  # find spaces and replace with '%20' -- aaded by Nic Wayand
  stationName <- stringr::str_replace_all(stationName, ' ', '%20')
  
  WISKIstring <- paste(site.url, stock, stationName, stock2, sep = '')
  data <- readLines(WISKIstring)
  
  # convert to data frame
  # split header on tab char
  data.split <- strsplit(data, '\t')
  data.dataframe <- as.data.frame(do.call('rbind',data.split), stringsAsFactors = FALSE)
  variable.names <- data.dataframe[1,] 
  timeseries.dataframe <- data.dataframe[2:nrow(data.dataframe),]
  names(timeseries.dataframe) <- variable.names
  
  return(timeseries.dataframe)
}
