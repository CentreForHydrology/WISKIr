#' Gets metadata of a WISKI time series
#' @description Returns metadata of a specified time series.
#' @param timeseries Required. Character vector containing the WISKI time series ID number, which is returned by find_WISKI_timeseries. Cannot contain wild card characters.
#' @param site.url Optional. Optional. A character vector containing the url of the WISKI web server. Defaults to the Global Institute for Water Security (GIWS) server: "http://giws.usask.ca:8080/
#' @details As this packages is intended for use by the Saskatoon hydrological community, it is usually unnecessary to specify the web server.
#' @return Returns a dataframe containing metadata of specified time series.
#' @author Kevin Shook
#' @seealso \code{\link{get_WISKI_timeseries_values}} \code{\link{find_WISKI_timeseries}}
#' @examples
#' WISKI.timeseries.metadata <- get_WISKI_timeseries_metadata('9325042')
#' print(WISKI.timeseries.metadata)
#' @export

get_WISKI_timeseries_metadata <- function(timeSeries, site.url='http://giws.usask.ca:8080/'){
  # reads time series metadata from the WISKI server and returns all metadata
  stock <-'KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesValues&datasource=0&format=csv&ts_id='
  WISKIstring <- paste(site.url, stock, timeSeries,'&metadata=true',sep='')
  
  # split header on tab char
  header <- readLines(WISKIstring, 11)
  # split header on tab char
  header.split <- strsplit(header, ';')
  header.dataframe <- as.data.frame(do.call('rbind',header.split), stringsAsFactors = FALSE)
  names(header.dataframe) <- c('name','value')
  return(header.dataframe)
}