#' Retrieve archived reports for each of the linked accounts.
#'
#' \code{getArchivedReport} provides access to archived reports
#' from the appFigures web API
#'
#' @param type Character. Choose between \code{"all", "daily",
#' "weekly", "monthly", "monthlyfree", "finance", or "payment"}.
#' Defaults to \code{"all"}.
#' 
#' @param date Character. Only return reports with a timestamp on
#' this date. Defaults to all dates.
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
#' @return A dataframe containing the requested reports.
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/archive}.
#' 

getArchivedReport <- function(type = c("all", "daily", "weekly", "monthly",
                                       "monthlyfree", "finance", "payment"),
                              date, curlHandle, verbose = FALSE,
                              orgJSON = FALSE) {
  
  type <- match.arg(type)
  date <- if (!missing(date)) {
    as.character(as.Date(date))
  }
  parList <- c(type = type, date = date, format = 'json')
  uri <- paste(BASE_URI, "archive", sep = "/")
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
  parseArchiveReport(jsonText)
}



#' Retrieve latest report for each of the linked accounts.
#'
#' \code{getLatestReport} provides access to the latest reports
#' from the appFigures web API
#'
#' @param type Character. Choose between \code{"all", "daily",
#' "weekly", "monthly", "yearly", "finance", "payment"}. Defaults
#' to \code{"all"}.
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
#' @return A dataframe containing the requested reports.
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/archive}.
#' 

getLatestReport <- function(type = c("all", "daily", "weekly", "monthly",
                                     "yearly", "finance", "payment"),
                            curlHandle, verbose = FALSE, orgJSON = FALSE) {
  
  type <- match.arg(type)
  parList <- c(type = type, format = 'flat')
  uri <- paste(BASE_URI, "archive", "latest", sep = "/")
  if (missing(curlHandle)) {
    opts <- list(userpwd = paste(USERNAME, PASSWORD, sep = ":"),
                 httpheader = c('X-Client-Key' = API_KEY),
                 httpauth = 1L, verbose = verbose, ssl.verifypeer = FALSE)
    curlHandle <- getCurlHandle(.opts = opts)
  } else if (class(curlHandle) != "CURLHandle") {
    stop("curlHandle must be of class 'CURLHandle'.")
  } else {
    curlHandle <- curlHandle
  }  
  jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  if (orgJSON) {
    return(jsonText)
  }
  if (!validate(jsonText)) {
    stop("appFigures API yielded invalid JSON!")
  }
  parseArchiveReport(jsonText)
}


#' Retrieve the raw report data.
#'
#' \code{getRawReport} provides access to the raw report data
#' from the appFigures web API
#'
#' @param id Numeric. The id of requested report.
#' 
#' @param curlHandle Provide an instance of the CURLHandle-class
#' from the RCurl package. The default will create a curl handle
#' specific to the function call.
#' 
#' @param verbose Logical. Should details of the web request
#' print to the console? Defaults to \code{FALSE}.
#' 
#' @return A character string containing the raw report.
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/archive}.

getRawReport <- function(id, curlHandle, verbose = FALSE) {
  
  parList <- c(format = 'json')
  uri <- paste(BASE_URI, "archive", "raw", id, sep = "/")
  if (missing(curlHandle)) {
    opts <- list(userpwd = paste(USERNAME, PASSWORD, sep = ":"),
                 httpheader = c('X-Client-Key' = API_KEY),
                 httpauth = 1L, verbose = verbose, ssl.verifypeer = FALSE)
    curlHandle <- getCurlHandle(.opts = opts)
  } else if (class(curlHandle) != "CURLHandle") {
    stop("curlHandle must be of class 'CURLHandle'.")
  } else {
    curlHandle <- curlHandle
  }
  getForm(uri, curl = curlHandle, .params = parList)
}



#' Map JSON string to an R data frame.
#'
#' \code{parseArchiveReport} parses the JSON returned by a
#' featured request made to the appFigures web API.
#'

parseArchiveReport <- function(jsonText) {
  datr <- fromJSON(jsonText)
  out <- datr[[2]]
  names(out) <- c("report_id", "type", "ext_acct_id", "report_date", "import_date",
                  "region", "import_method")
  out$report_date <- as.POSIXct(out$report_date, "UTC",
                                format = "%Y-%m-%dT%H:%M:%S")
  out$import_date <- as.POSIXct(out$import_date, "UTC",
                                format = "%Y-%m-%dT%H:%M:%S")
  out
}
