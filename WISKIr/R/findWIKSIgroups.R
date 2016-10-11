#' Finds all groups on the Wiski server
#'
#' @param site.url Optional. A character string containing the url of the WISKI web server. Defaults to the Global Institute for Water Security (GIWS) server: \code{'http://giws.usask.ca:8080/'}. As this package is intended for use by the GIWS hydrological community, it is usually unnecessary to specify the web server.
#'
#' @return If unsuccessful, returns \code{FALSE}. If successful, returns a dataframe containing \item{group_id}{ID number of the group} \item{group_name}{WISKI name of the group} \item{gorup_type}{Type of the group}
#' @author Kevin Shook
#' @seealso \code{\link{findWISKIstations}}  \code{\link{findWISKItimeseries}} 
#' @export
#'
#' @examples
#' groups <- findWISKIgroups()
findWISKIgroups <- function(site.url='http://giws.usask.ca:8080/'){
  
  stock <-'KiWIS/KiWIS?service=kisters&type=queryServices&request=getGroupList&datasource=0&format=ascii'

  WISKIstring <- paste(site.url, stock, sep='')
  data <- readLines(WISKIstring)
  # convert to data frame
  # split header on tab char
  data.split <- strsplit(data, '\t')
  data.dataframe <- as.data.frame(do.call('rbind',data.split), stringsAsFactors = FALSE)
  variable.names <- data.dataframe[1,] 
  groups.dataframe <- data.dataframe[2:nrow(data.dataframe),]
  names(groups.dataframe) <- variable.names
  return(groups.dataframe)
}