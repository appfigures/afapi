#' Search products by name or developer.
#'
#' \code{searchProducts} searches for a product by name or developer
#' in the appFigures web API. 
#'
#' @param term A character string to search for. See Details.
#'
#' @param filter \code{NULL} value or a character string. Acceptable
#' string values are: "ios", "mac", "google", and "amazon". Default
#' \code{NULL} value returns all results.
#' 
#' @param page A numeric value indicating which page of results to show.
#' Defaults to the first page.
#' 
#' @param count A numeric value indicating the number of results to
#' show in a page. Defaults to 25.
#' 
#' @param curlHandle Provide an instance of the CURLHandle-class
#' from the RCurl package. The default will create a curl handle
#' specific to the function call.
#' 
#' @param verbose Logical. Should details of the web request
#' print to the console? Defaults to FALSE.
#' 
#' @param orgJSON Logical. Should the JSON string be returned
#' without being converted to R objects? Defaults to FALSE.
#' 
#' @details For the \code{term} argument, prefix the string
#' with \code{@@name=} or \code{@@developer=} if you want to search those fields
#' specifically. Any spaces included in the \code{term} string
#' will return an error. Instead, separate multiple terms with '+'.
#' 
#' @return A data frame containing all publicly available fields.
#' Fields containing meta data will be added later.
#' 
#' @seealso Official documentation:
#' \url{http://docs.appfigures.com/products#searching_for_products}.
#' 

searchProducts <- function(term, filter = NULL, page = 1, count = 25,
                           curlHandle, verbose = FALSE,
                           orgJSON = FALSE) {
  
  stopifnot(!missing(term))
  if (!is.null(filter)) {
    stopifnot(filter %in% c("ios", "mac", "google", "amazon"))
  }
  
  parList <- c(filter = filter, page = page, count = count)
  uri <- paste(BASE_URI, "products", "search", term, sep = "/")
  if (missing(curlHandle)) {
    opts <- list(userpwd = paste(USERNAME, PASSWORD, sep = ":"),
                 httpheader = c('X-Client-Key' = API_KEY),
                 httpauth = 1L, verbose = verbose, ssl.verifypeer = FALSE)
    curlHandle <- getCurlHandle(.opts = opts)
  } else if (!inherits(curlHandle, "CURLHandle")) {
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
  parseSearch(jsonText)
}



#' Map JSON string to an R data frame.
#'
#' \code{parseSearch} parses the JSON returned by a search
#' request made to the appFigures web API.
#'

parseSearch <- function(jsonText) {
  datr <- fromJSON(jsonText)
  if (length(datr[[1]]) <= 1) {
    datr <- cbind(datr, datr$source, stringsAsFactors = F)
    datr$storefronts <- paste(unlist(datr$storefronts), collapse = "/")
    datr$accessible_features <- paste(unlist(datr$accessible_features), collapse = "/")
    datr$source <- datr$children <- datr$parent <- datr$features <- NULL
    datr$added_timestamp <- NULL
  }
  device <- vapply(datr$devices,
                   function(x) ifelse(identical(x, character(0)), NA_character_, x),
                   character(1))
  price <- datr$price
  price$price <- as.numeric(price$price)
  datr$devices <- device
  datr$price <- NULL
  datr$added_date <- as.POSIXct(datr$added_date, format = "%Y-%m-%dT%H:%M:%S")
  datr$release_date <- as.POSIXct(datr$release_date, format = "%Y-%m-%dT%H:%M:%S")
  datr$updated_date <- as.POSIXct(datr$updated_date, format = "%Y-%m-%dT%H:%M:%S")
  names(datr)[1:2] <- c("product_id", "p_name")
  cbind(datr, price)
}
