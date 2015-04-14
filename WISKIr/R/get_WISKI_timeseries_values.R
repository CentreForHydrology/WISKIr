#' Gets values of a WISKI time series
#' @description Returns times, values, and quality codes for a specified interval of a specified time series.
#' @param timeseries Required. Character vector containing the WISKI time series ID number, which is returned by find_WISKI_timeseries. Cannot contain wild card characters.
#' @param startDate Optional. Character vector of the starting date of data being queried. Must be in the form 'yyyy-mm-dd'. The default value is '1900-01-01'.
#' @param endDate Optional. Character vector of the ending date of data being queried. Must be in the form 'yyyy-mm-dd'. The default value is today's date.
#' @param site.url Optional. Optional. A character vector containing the url of the WISKI web server. Defaults to the Global Institute for Water Security (GIWS) server: "http://giws.usask.ca:8080/
#' @details As this packages is intended for use by the Saskatoon hydrological community, it is usually unnecessary to specify the web server.
#' @return Returnes a dataframe with three variables: \item{time}{R time value} \item{variable name}{time series values} \item{QualityCode}{time series quality codes}
#' @author Kevin Shook
#' @seealso \code{\link{get_WISKI_timeseries_metadata}} \code{\link{find_WISKI_timeseries}}
#' @examples
#' WISKI.timeseries.values <- get_WISKI_timeseries_values('9325042','2013-06-01','2013-06-30')
#' summary(WISKI.timeseries.values)
#' @export

get_WISKI_timeseries_values <- function(timeSeries, startDate='', endDate='', site.url='http://giws.usask.ca:8080/'){
  # reads a time series from the WISKI server and returns a dataframe
  # this version reads ASCII data
  # format for WISKI dates must be yyyy-mm-dd
  # if start date and end date are specified, return interval
  # if start date not specified, return all data from beginning
  # if end date not specified, retrun all data to end
  # if neither specified, return all data
  
  stock <-'KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesValues&datasource=0&format=ascii&ts_id='
  WISKIstring <- paste(site.url, stock, timeSeries,sep='')
  WISKIstring <- paste(WISKIstring,'&returnfields=Timestamp,Value,Quality%20Code',sep='')
  #  WISKIstring <- paste(WISKIstring,'&returnfields=Timestamp,Value',sep='') 
  WISKIstring <- paste(site.url, stock, timeSeries,sep='')
  WISKIstring <- paste(WISKIstring,'&returnfields=Timestamp,Value,Quality%20Code',sep='')

  #  WISKIstring <- paste(WISKIstring,'&returnfields=Timestamp,Value',sep='') 
  if (startDate == '')
      startDate <- '1900-01-01'
  
  if(endDate == '')
    endDate <- Sys.Date()
  
  WISKIstring <- paste(WISKIstring,'&from=',startDate,'&to=',endDate,sep='')
  
  # create name for variable from metadata for time series
  variable.name <- get_WISKI_timeseries_variablename(timeSeries)
  
  # replace spaces with underscores in variable name
  variable.name <- str_replace(variable.name, ' ', '_')
  
  data <- read.table(WISKIstring, sep='\t', header=FALSE, skip=3, stringsAsFactors=FALSE)
  names(data) <- c('time', variable.name,'QualityCode')
  
  # convert WISKI date/time to R time
  # example: 2013-05-01T00:00:00.000+00:00
  data$time <- strptime(as.character(data$time), format='%Y-%m-%dT%H:%M:%S')
  # convert data to numeric - missing values will be NA
  data[,2] <- as.numeric(as.character(data[,2]))
  
  return(data)
}