#' Retrieve account usage details.
#'
#' \code{createEvent} provides a detailed breakdown of the past 30
#' days worth of usage as well as a summary of total usage.
#' 
#' @param curlHandle Provide an instance of the CURLHandle-class
#' from the RCurl package. The default will create a curl handle
#' specific to the function call.
#' 
#' @param verbose Logical. Should details of the web request
#' print to the console? Defaults to \code{FALSE}.
#' 
#' @return A data frame
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/usage}.
#' 

getUsage <- function(curlHandle, verbose = FALSE, orgJSON = FALSE) {
    
  parList <- c(format = 'json')
  uri <- paste(BASE_URI, "usage", sep = "/")
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
  parseUsage(jsonText)
}


#' Map JSON string to an R data frame.
#'
#' \code{parseUsage} parses the JSON returned by a
#' usage request made to the appFigures web API.
#'

parseUsage <- function(jsonText) {
  datr <- fromJSON(jsonText)
  formData <- function(lst) {
    hist_total <- lst[["history"]][["total"]]
    reqs <- lst[["history"]][["history"]]
    names(reqs) <- as.Date(names(reqs))
    daily_allowed <- lst[["total"]]
    today <- lst[["used"]]
    remaining <- lst[["remaining"]]
    data.frame(daily_allowed, today, remaining,
               hist_total, reqs,
               stringsAsFactors = FALSE)
  }
  do.call(rbind, lapply(datr, formData))
}


