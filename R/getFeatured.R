#' Retrieve full featured reports.
#'
#' \code{getFeaturedReport} provides access to detailed featured
#' data from the appFigures web API 
#'
#' @param product_id Numeric. A single appFigures-assigned
#' product id.
#'  
#' @param end_date Character string or date object. Date of last ranks
#' to be reported. Defaults to today. See Details.
#' 
#' @param start_date Character string or date object. Date of first
#' ranks to be reported. Defaults to the last 31 days. See Details.
#' 
#' @param country One or more country iso codes. Defaults to all
#' countries. See Details.
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
#' @details For \code{start_date} and \code{end_date}, if the
#' supplied argument can be interepreted as a date or POSIX
#' object, any hour, minute, or second is ignored. If a string
#' is supplied, it should have the format: 'yyyy-MM-dd'. The largest
#' date range for a feature report is limited to 31 days. Anything
#' larger will return an error.
#' 
#' 
#' For \code{country} arguments, a complete list of supported
#' languages can be found using \code{\link{getStoreData}}.
#' 
#' @return A single R dataframe.
#'  
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/api/reference/v2/featured}.
#' 

getFeaturedReport <- function(product_id, end_date, start_date,
                              country, curlHandle, verbose = FALSE,
                              orgJSON = FALSE) {
  
  if (missing(product_id) || length(product_id) != 1) {
    stop("A single product id must be specified.")
  }
  if (missing(end_date)) {
    end_date <- as.character(Sys.Date())
  } else {
    end_date <- as.character(as.Date(end_date))
  }
  if (missing(start_date)) {
    start_date <- as.character(Sys.Date() - 31)
  } else {
    start_date <- as.character(as.Date(start_date))
  }
  uri <- paste(BASE_URI, "featured", "full", product_id, start_date, end_date,
               sep = "/")
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
  country <- if (!missing(country)) {
    paste(country, collapse = ";")
  }
  parList <- c(format = 'json', countries = country)
  jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  if (orgJSON) {
    return(jsonText)
  }
  if (!validate(jsonText)) {
    stop("appFigures API yielded invalid JSON!")
  }
  parseFeaturedReport(jsonText, product_id)
}


#' Map JSON string to an R data frame.
#'
#' \code{parseFeaturedReport} parses the JSON returned by a
#' featured request made to the appFigures web API.
#'

parseFeaturedReport <- function(jsonText, pid) {
  jsonText <- gsub("null", -999, jsonText)
  datr <- fromJSON(jsonText)
  if (length(datr) == 0) {
    warning("No featured reports found for the date ranges.")
    return(data.frame())
  }
  data.frame(
    product_id = pid,
    featured_id = as.numeric(names(datr)),
    iso2 = vapply(datr, function(node) node$country,
                  vector("character", 1), USE.NAMES = F),
    store = vapply(datr, FUN = function(node) node$base_category$store_id,
                   vector("double", 1), USE.NAMES = F),
    store2 = vapply(datr, FUN = function(node) node$base_category$store,
                    vector("character", 1), USE.NAMES = F),
    device = vapply(datr, FUN = function(node) node$base_category$device_id,
                    vector("double", 1), USE.NAMES = F),
    device2 = vapply(datr, FUN = function(node) node$base_category$device,
                     vector("character", 1), USE.NAMES = F),
    c_id = vapply(datr, FUN = function(node) node$base_category$id,
                  vector("double", 1), USE.NAMES = F),
    c_name = vapply(datr, FUN = function(node) node$base_category$name,
                    vector("character", 1), USE.NAMES = F),
    c_subcat = vapply(datr, FUN = function(node) node$base_category$subtype,
                      vector("character", 1), USE.NAMES = F),
    viewed_from = vapply(datr, FUN = function(node) node$viewed_from,
                         vector("character", 1), USE.NAMES = F),
    crumbs = vapply(datr, function(node) paste(node$crumbs, collapse = "/"),
                    vector("character", 1), USE.NAMES = F),
    start_date = as.Date(vapply(datr, FUN = function(node) node$start_date,
                                vector("character", 1), USE.NAMES = F)),
    end_date = as.Date(vapply(datr, FUN = function(node) node$end_date,
                              vector("character", 1), USE.NAMES = F)),
    high_rank = vapply(datr, function(node) node$highest_position,
                       vector("double", 1), USE.NAMES = F),
    low_rank = vapply(datr, function(node) node$lowest_position,
                      vector("double", 1), USE.NAMES = F),
    stringsAsFactors = F)
}


# ---------------------------------------------------------------------------


#' Retrieve featured summary reports.
#'
#' \code{getFeaturedSummary} provides access to detailed featured
#' data from the appFigures web API 
#'
#' @param product_ids Numeric. A vector of appFigures-assigned
#' product ids. Defaults to all account products.
#'  
#' @param end_date Character string or date object. Date of last ranks
#' to be reported. Defaults to today. See Details.
#' 
#' @param start_date Character string or date object. Date of first
#' ranks to be reported. Defaults to the last 31 days. See Details.
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
#' @details For \code{start_date} and \code{end_date}, if the
#' supplied argument can be interepreted as a date or POSIX
#' object, any hour, minute, or second is ignored. If a string
#' is supplied, it should have the format: 'yyyy-MM-dd'.
#' 
#' @return A list of dataframes, one for each product id.
#' 
#' @details The list of countries included in the request is included as an
#' attribute.
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/api/reference/v2/featured}.
#' 

getFeaturedSummary <- function(product_ids, end_date, start_date, curlHandle,
                               verbose = FALSE, orgJSON = FALSE) {
  
  if (missing(end_date)) {
    end_date <- as.character(Sys.Date())
  } else {
    end_date <- as.character(as.Date(end_date))
  }
  if (missing(start_date)) {
    start_date <- as.character(Sys.Date() - 31)
  } else {
    start_date <- as.character(as.Date(start_date))
  }
  uri <- paste(BASE_URI, "featured", "summary", start_date, end_date, sep = "/")
  product_ids <- if (!missing(product_ids)) {
    paste(product_ids, collapse = ";")
  }
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
  parList <- c(format = 'json', products = product_ids)
  jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  if (orgJSON) {
    return(jsonText)
  }
  if (!validate(jsonText)) {
    stop("appFigures API yielded invalid JSON!")
  }
  output <- parseFeaturedSummary(jsonText)
  message("A list of countries where the product id was featured are
          provided as an attribute to each data frame.")
  output
}


#' Map JSON string to a list of R data frames.
#'
#' \code{parseFeaturedSummary} parses the JSON returned by a
#' featured summary request made to the appFigures web API.
#'

parseFeaturedSummary <- function(jsonText) {
  datr <- fromJSON(jsonText)
  parseLists <- function(lst) {
    paths <- vapply(lst[['paths']],
                    function(node) paste(unlist(node), collapse = "/"),
                    vector("character", 1))
    df <- data.frame(product_id = lst[['product_id']],
                     start_date = as.Date(lst[['start']]),
                     end_date = as.Date(lst[['end']]),
                     paths = paths,
                     stringsAsFactors = F)
    # Vector of countries as an attribute
    attr(df, "countries") <- lst[["countries"]]
    df
  }
  
  lapply(datr, parseLists)
}


# ----------------------------------------------------------------------


#' Retrieve featured count reports.
#'
#' \code{getFeaturedCounts} provides access to detailed featured
#' data from the appFigures web API 
#'
#' @param product_ids Numeric. A vector of appFigures-assigned
#' product ids. Defaults to all account products.
#'  
#' @param end_date Character string or date object. Date of last ranks
#' to be reported. Defaults to today. See Details.
#' 
#' @param count Integer. Max number of entries to show. Defaults to
#' five.
#' 
#' @param granularity Character. Either \code{"weekly" or "daily"}.
#' Defaults to \code{"weekly"}.
#' 
#' @param show_empty Logical. If set to \code{TRUE}, this call will
#' return the last count periods whether or not they have data.
#' Defaults to \code{FALSE}.
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
#' @details For \code{end_date}, if the supplied argument can be
#' interepreted as a date or POSIX object, any hour, minute, or
#' second is ignored. If a string is supplied, it should have
#' the format: 'yyyy-MM-dd'.
#' 
#' @return A list of dataframes, one for each product id.
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/api/reference/v2/featured}.
#' 

getFeaturedCounts <- function(product_ids, end_date, count = 5, show_empty = FALSE,
                              granularity = c("weekly", "daily"), curlHandle,
                              verbose = FALSE, orgJSON = FALSE) {
  # show_empty will always be false for data management
  
  granularity <- match.arg(granularity)
  uri <- paste(BASE_URI, "featured", "counts", sep = "/")
  if (missing(end_date)) {
    end_date <- as.character(Sys.Date())
  } else {
    end_date <- as.character(as.Date(end_date))
  }
  product_ids <- if (!missing(product_ids)) {
    paste(product_ids, collapse = ";")
  }
  show_empty <- tolower(deparse(show_empty))
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
  parList <- c(format = 'json', products = product_ids, end = end_date,
               count = count, show_empty = show_empty, granularity = granularity)
  jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  if (orgJSON) {
    return(jsonText)
  }
  if (!validate(jsonText)) {
    stop("appFigures API yielded invalid JSON!")
  }
  parseFeaturedCounts(jsonText)
}



#' Map JSON string to an R dataframe.
#'
#' \code{parseFeaturedCounts} parses the JSON returned by a
#' featured count request made to the appFigures web API.
#'

parseFeaturedCounts <- function(jsonText) {
  jsonText <- gsub('null', -999, jsonText)
  datr <- fromJSON(jsonText)
  nrs <- vapply(datr[[2]], nrow, numeric(1), USE.NAMES = F)
  for (jj in which(nrs == 0))
    datr$products[[jj]] <- data.frame(product_id = NA_real_, featured_count = NA_real_)
  nrs[nrs == 0] <- 1
  products <- do.call(rbind, datr[[2]])
  start_date <- vapply(datr[[1]], `[`, 1, FUN.VALUE = character(1))
  start_date <- rep(start_date, times = nrs)
  end_date <- vapply(datr[[1]], `[`, 2, FUN.VALUE = character(1))
  end_date <- rep(end_date, times = nrs)
  out <- data.frame(cbind(products, 
                          start_date = start_date,
                          end_date = end_date))
  out[order(out$start_date, decreasing = TRUE), ]
}


# ----------------------------------------------------------------------

#' Retrieve featured history reports.
#'
#' \code{getFeaturedHistory} provides access to detailed featured
#' data from the appFigures web API 
#'
#' @param product_id Numeric. A single appFigures-assigned
#' product ids.
#' 
#' @param feature_id Numeric. A single feature id. This can be found
#' using \code{\link{getFeaturedReport}}.
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
#' @return A list of dataframes, one for each product id.
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/api/reference/v2/featured}.
#' 


getFeaturedHistory <- function(product_id, feature_id, curlHandle, verbose = FALSE,
                               orgJSON = FALSE) {
  
  stopifnot(!missing(product_id), !missing(feature_id))
  uri <- paste(BASE_URI, "featured", "history", product_id, feature_id, sep = "/")
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
  parList <- c(format = 'json')
  jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  if (orgJSON) {
    return(jsonText)
  }
  if (!validate(jsonText)) {
    stop("appFigures API yielded invalid JSON!")
  }
  output <- parseFeaturedHistory(jsonText)
  attr(output, 'product_id') <- product_id
  attr(output, 'feature_id') <- feature_id
  output
}


#' Map JSON string to an R dataframe.
#'
#' \code{parseFeaturedHistory} parses the JSON returned
#' by a featured history request made to the appFigures
#' web API.
#'

parseFeaturedHistory <- function(jsonText) {
  datr <- fromJSON(jsonText)
  datr$time <- as.POSIXct(datr$time,
                        format = "%Y-%m-%dT%H:%M:%S")
  names(datr) <- c("time_stamp", "rank")
  datr[order(datr$time_stamp), ]
}
