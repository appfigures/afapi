#' Retrieve all events.
#'
#' \code{getEvents} provides access to all listed events
#' from the appFigures web API
#' 
#' @param curlHandle Provide an instance of the CURLHandle-class
#' from the RCurl package. The default will create a curl handle
#' specific to the function call.
#' 
#' @param verbose Logical. Should details of the web request
#' print to the console? Defaults to \code{FALSE}.
#' 
#' @param orgJSON Logical. Should the JSON string be returned
#' without being converted to R objects? Defaults to \code{FALSE}.
#' 
#' @return A dataframe containing all events.
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/api/reference/v2/events}.
#' 

getEvents <- function(curlHandle, verbose = FALSE,
                      orgJSON = FALSE) {
  
  uri <- paste(BASE_URI, "events", sep = "/")
  parList <- c(format = 'json')
  if (missing(curlHandle)) {
    opts <- list(userpwd = paste(USERNAME, PASSWORD, sep = ":"),
                 httpheader = c('X-Client-Key' = API_KEY),
                 httpauth = 1L, verbose = verbose, ssl.verifypeer = FALSE)
    curlHandle <- getCurlHandle(.opts = opts)
  } else if (class(curlHandle) != "CURLHandle") {
    stop("curlHandle must be of class 'CURLHandle'.")
  } else {
    curlHandle = curlHandle
  }
  jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  if (orgJSON) {
    return(jsonText)
  }
  if (!validate(jsonText)) {
    stop("appFigures API yielded invalid JSON!")
  }
  parseEvents(jsonText)
}


#' Map JSON string to an R data frame.
#'
#' \code{parseEvents} parses the JSON returned by a
#' featured request made to the appFigures web API.
#'

parseEvents <- function(jsonText) {
  jsonText <- gsub("null", "\"NA\"", jsonText)
  datr <- fromJSON(jsonText)
  #output <- data.frame(
  event_id = vapply(datr, `[[`, integer(1), "id")
  caption = vapply(datr, `[[`, character(1), "caption")
  event_date = vapply(datr, `[[`, character(1), "date")
  details = vapply(datr, `[[`, character(1), "details")
  product_id  = vapply(datr,
                       function(x) paste(x$products, collapse = "/"),
                       character(1))
  data.frame(event_id, caption,
             event_date = as.Date(event_date), details,
             product_id, stringsAsFactors = F)
}



#' Create a new event.
#'
#' \code{createEvent} allows the user to add a new event.
#' 
#' @param caption Character. A short caption summarizing the event.
#' 
#' @param date Character with "yyyy-mm-dd" format.
#' 
#' @param details Character. Any additional notes or details
#' the user wants to include describing the event.
#' 
#' @param products A character or numeric vector of product ids
#' that the user wants to associate with the new event.
#' 
#' @param curlHandle Provide an instance of the CURLHandle-class
#' from the RCurl package. The default will create a curl handle
#' specific to the function call.
#' 
#' @param verbose Logical. Should details of the web request
#' print to the console? Defaults to \code{FALSE}.
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/api/reference/v2/events}.
#' 

createEvent <- function(caption, date, details, products,
                        curlHandle, verbose = FALSE) {
  
  uri <- paste(BASE_URI, "events", sep = "/")
  if (missing(curlHandle)) {
    opts <- list(userpwd = paste(USERNAME, PASSWORD, sep = ":"),
                 httpheader = c('X-Client-Key' = API_KEY,
                                'Content-Type' = "application/json"),
                 httpauth = 1L, verbose = verbose, ssl.verifypeer = FALSE)
    curlHandle <- getCurlHandle(.opts = opts)
  } else if (class(curlHandle) != "CURLHandle") {
    stop("curlHandle must be of class 'CURLHandle'.")
  } else {
    curlHandle = curlHandle
  }
  event <- data.frame(caption = caption, date = date,
                    details = details, stringsAsFactors = F)
  event$products <- list(c(as.character(products)) )
  event <- toJSON(event)
  event <- substr(event, 2, nchar(event))
  event <- substr(event, 1, nchar(event) - 1)
  httpPOST(uri, postfields = event, curl = curlHandle)
  NULL
}

