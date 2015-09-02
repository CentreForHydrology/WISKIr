#' Gets metadata of a WISKI time series
#' @description Returns metadata of a specified time series.
#' @param timeSeries Required. Character string containing the WISKI time series ID number, which is returned by \code{findWISKItimeseries}. Cannot contain wild card characters.
#' @param site.url Optional. A character string containing the url of the WISKI web server. Defaults to the Global Institute for Water Security (GIWS) server: \code{'http://giws.usask.ca:8080/'}. As this package is intended for use by the GIWS hydrological community, it is usually unnecessary to specify the web server.
#' @return If unsucessful, returns \code{FALSE}. If successful, returns a dataframe containing the metadata of the specified time series.
#' @author Kevin Shook
#' @seealso \code{\link{getWISKIvalues}} \code{\link{findWISKItimeseries}}
#' @examples
#' # Finds metadata for Fisera Ridge air temperatures (Original time series) 
#' FiseraTmetadata <- getWISKImetadata('9296042')
#' print(FiseraTmetadata)
#' @export

getWISKImetadata <- function(timeSeries, site.url='http://giws.usask.ca:8080/'){
  # reads time series metadata from the WISKI server and returns all metadata
  stock <-'KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesValues&datasource=0&format=csv&ts_id='
  
  # check parameters
  if (timeSeries == ''){
    cat("Error: missing time series'\n")
    return(FALSE)
  }
  WISKIstring <- paste(site.url, stock, timeSeries,'&metadata=true',sep='')
  
  # split header on tab char
  header <- readLines(WISKIstring, 10)
  # split header on tab char
  header.split <- strsplit(header, ';')
  header.dataframe <- as.data.frame(do.call('rbind',header.split), stringsAsFactors = FALSE)
  names(header.dataframe) <- c('name','value')
  
  # remove leading #
  header.dataframe$name <- str_replace(header.dataframe$name, '#', '')
  return(header.dataframe)
}