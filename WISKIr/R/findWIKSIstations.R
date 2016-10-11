#' Finds all stations on the Wiski server
#'
#' @param stationName Required. Name of station to search for. May contain wildcards. The defaul is \option{*}, which will search for all of the stations on the server. 
#' @param site.url Optional. A character string containing the url of the WISKI web server. Defaults to the Global Institute for Water Security (GIWS) server: \code{'http://giws.usask.ca:8080/'}. As this package is intended for use by the GIWS hydrological community, it is usually unnecessary to specify the web server.
#'
#' @return If unsuccessful, returns \code{FALSE}. If successful, returns a dataframe containing \item{station_name}{Name of station, specified by \code{stationName}} \item{station_no}{WISKI number of station} \item{station_id}{WISKI station ID number} \item{station_latitude}{Latitude of station} \item{station_longitude}{Longitude of station}
#' @author Kevin Shook
#' @seealso \code{\link{findWISKItimeseries}} \code{\link{findWISKIgroups}} 
#' @export
#'
#' @examples
#' stations <- findWISKIstations()
findWISKIstations <- function(stationName='*', site.url='http://giws.usask.ca:8080/'){
  
  stock <-'KiWIS/KiWIS?service=kisters&type=queryServices&request=getStationList&datasource=0&format=ascii&station_name='
  
  if (stationName == ''){
    cat("Error: missing stationName'\n")
    return(FALSE)
  }
  WISKIstring <- paste(site.url, stock, stationName, sep='')
  data <- readLines(WISKIstring)
  # convert to data frame
  # split header on tab char
  data.split <- strsplit(data, '\t')
  data.dataframe <- as.data.frame(do.call('rbind',data.split), stringsAsFactors = FALSE)
  variable.names <- data.dataframe[1,] 
  stations.dataframe <- data.dataframe[2:nrow(data.dataframe),]
  names(stations.dataframe) <- variable.names
  return(stations.dataframe)
}