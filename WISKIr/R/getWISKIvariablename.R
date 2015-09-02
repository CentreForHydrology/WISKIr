getWISKIvariablename <- function(timeSeries, site.url){
  # reads time series metadata from the WISKI server and returns a composite variable name
  header <- getWISKImetadata(timeSeries)
  var.name <- paste(header$value[6], header$value[7], sep='.')
  return(var.name)
}
