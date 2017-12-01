fetchdata <- function(url){
  tryCatch(
    ## This is what I want to do:
    expr = utils::read.table(url, sep = '\t', header = FALSE, skip = 3, 
                              stringsAsFactors = FALSE)
    ,
    ## But if an error occurs, do the following: 
    error = function(error_message) {
      message("No data available for this variable for this period.")
      message(error_message)
      return(NA)
    }
  )
}