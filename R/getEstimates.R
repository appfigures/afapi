#' Retrieve Estimates by appFigures-assigned id.
#'
#' \code{getEstimates} requests product specific data from the appFigures API. 
#'
#' @param product_ids Numeric. A vector of appFigures-assigned product ids.
#' 
#' @param country Character. One or more country iso abbreviations that filter
#' which countries estimates data to include. Defaults to "US".
#' 
#' @param end_date Character string or date object. Date of last estimate to be
#' reported. Defaults to today. See Details.
#' 
#' @param start_date Character string or date object. Date of first estimate to be
#' reported. Defaults to 31 days prior to the \code{end_date}. See Details.
#' 
#' @param curlHandle Provide an instance of the CURLHandle-class from the RCurl
#' package. The default will create a curl handle specific to the function call.
#' 
#' @param verbose Logical. Should details of the web request print to the
#' console? Defaults to \code{FALSE}.
#' 
#' @param orgJSON Logical. Should the JSON string be returned without being 
#' converted to R objects? Defaults to \code{FALSE}.
#' 
#' @details Estimates represent a prediction for apps. Both downloads and revenue estimates
#'are displayed.
#' 
#' @return A data frame containing all publicly available fields.
#' 
#' @seealso Official documentation:
#' \url{http://docs.appfigures.com/}.
#' 
#' 
#' 

getEstimates <- function(product_ids, countries = "US", category, end_date, start_date,
                         curlHandle, verbose = FALSE, orgJSON = FALSE) {

  if (missing(product_ids))
    stop("No product id(s) given.")
  if (missing(end_date)) {
    end_date <- as.character(Sys.Date())
  } else end_date <- as.character(as.Date(end_date))
  if (missing(start_date)) {
    start_date <- as.character(as.Date(end_date) - 31)
  } else start_date <- as.character(as.Date(start_date))
  product_ids <- paste(product_ids, collapse = ";")
  

  uri <- paste(BASE_URI, "reports", 'estimates', sep = "/")
  parList <- c(format = "flat",
               group_by = "product,date",
               countries =  countries,
               products = product_ids,
               start = start_date,
               end = end_date)
  
  if (missing(curlHandle)) {
    opts <- list(userpwd = paste(USERNAME, PASSWORD, sep = ":"),
                 httpheader = c('X-Client-Key' = API_KEY), httpauth = 1L,
                 verbose = verbose, ssl.verifypeer = FALSE)
    curlHandle <- getCurlHandle(.opts = opts)
  } else if (!inherits(curlHandle, "CURLHandle")) {
    stop("curlHandle must be of class 'CURLHandle'.")
  } else {
    curlHandle <- curlHandle
  }
  jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  if (orgJSON)
    return(jsonText)
  return(parseEstimates(jsonText))
}

#' Map JSON string to an R data frame.
#'
#' \code{parseEstimates} parses the JSON returned by an app rank request made to the
#'  appFigures web API.
#'

parseEstimates <- function(jsonText) {
  datr <- fromJSON(jsonText)
  rr <- nrow(datr)
  if (rr == 0)
    return(data.frame())
  data.frame(
    date = as.POSIXct(datr$start_date, format = "%Y-%m-%dT%H:%M:%S"),
    product_id = datr$product_id,
    sales = datr$sales,
    revenue = datr$revenue,
    stringsAsFactors = F
  )
}
