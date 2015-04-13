get_WISKI_timeseries_variablename <- function(timeSeries, site.url='http://giws.usask.ca:8080/'){
  # reads time series metadata from the WISKI server and returns a composite variable name
  stock <-'KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesValues&datasource=0&format=ascii&ts_id='
  WISKIstring <- paste(site.url, stock, timeSeries,'&metadata=true',sep='')
  header <- get_WISKI_timeseries_metadata(timeSeries)
  var.name <- paste(header$value[6], header$value[7], sep='.')
  return(var.name)
}
