#' Gets values of a WISKI time series
#' @description Returns times, values, and quality codes for a specified interval of a specified time series.
#' @param timeSeries Required. Character string containing the WISKI time series ID number, which is returned by \code{findWISKItimeseries}. Cannot contain wild card characters.
#' @param startDate Optional. Character vector of the starting date of data being queried. Must be in the form \code{'yyyy-mm-dd'}. The default value is \code{'1900-01-01'}.
#' @param endDate Optional. Character vector of the ending date of data being queried. Must be in the form \code{'yyyy-mm-dd'}. The default value is today's date.
#' @param timezone Required. Time zone for the data. Can be any time zone string sed by \code{Java} such as \option{GMT-5}, or \option{MST}. 
#' @param site.url Optional. A character string containing the url of the WISKI web server. Defaults to the Global Institute for Water Security (GIWS) server: \code{'http://giws.usask.ca:8080/'}.  As this package is intended for use by the GIWS hydrological community, it is usually unnecessary to specify the web server.
#' @return  If unsuccessful, returns \code{FALSE}. If successful, returns a dataframe with three variables: \item{time}{R time value} \item{variable name}{time series values} \item{QualityCode}{time series quality codes}
#' @author Kevin Shook
#' @seealso \code{\link{getWISKImetadata}} \code{\link{findWISKItimeseries}}
#' @examples
#' # get values for Fisera Ridge air temperatures (Original time series) 
#' FiseraTvalues <- getWISKIvalues('9296042','2013-01-01','2013-06-30', timezone='CST')
#' summary(FiseraTvalues)
#' @export

getWISKIvalues <- function(timeSeries='', startDate='1900-01-01', 
                                        endDate='', timezone='', site.url='http://giws.usask.ca:8080/'){
  # reads a time series from the WISKI server and returns a dataframe
  # this version reads ASCII data
  # format for WISKI dates must be yyyy-mm-dd
  # if start date and end date are specified, return interval
  # if start date not specified, return all data from beginning
  # if end date not specified, retrun all data to end
  # if neither specified, return all data

  stock <-'KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesValues&datasource=0&format=ascii&dateformat=yyyy-MM-dd%20HH:mm:ss&ts_id='
  WISKIstring <- paste(site.url, stock, timeSeries,sep='')
  WISKIstring <- paste(WISKIstring,'&returnfields=Timestamp,Value,Quality%20Code',sep='')
  # check parameters
  timeSeries <- as.character(timeSeries)
  if (timeSeries == ''){
    cat("Error: missing time series'\n")
    return(FALSE)
  }
  
  if (timezone == ''){
    cat("Error: missing time zone'\n")
    return(FALSE)
  }
  
  if (startDate == '')
    startDate <- '1900-01-01'
  
  if(endDate == '')
    endDate <- Sys.Date()
  
  WISKIstring <- paste(WISKIstring,'&from=',startDate,'&to=',endDate,'&timezone=',timezone,sep='')
  #WISKIstring <- paste(WISKIstring,'&from=',startDate,'&to=',endDate,sep='')
  # create name for variable from metadata for time series
  variable.name <- getWISKIvariablename(timeSeries, site.url)
  
    
  # do something, or tell me why it failed
  fetchdata <- function(url){
    tryCatch(
        ## This is what I want to do:
        data <- utils::read.table(url, sep='\t', header=FALSE, skip=3, stringsAsFactors=FALSE)
        ,
        ## But if an error occurs, do the following: 
        error=function(error_message) {
            message("Skipping this variable becaues no data for this period.")
            message(error_message)
            return(NA)
        }
    )
}
  data <- fetchdata(WISKIstring)
  if(!is.na(NA)) {
  names(data) <- c('time', variable.name,'QualityCode')
  
  # convert WISKI date/time to R time
  # example: 2013-05-01T00:00:00.000+00:00
  data$time <- strptime(as.character(data$time), format='%Y-%m-%d %H:%M:%S')
  # convert data to numeric - missing values will be NA
  data[,2] <- as.numeric(as.character(data[,2]))
  }
  return(data)
}
