#' Retrieve product ranks.
#'
#' \code{getRanks} requests rank data for specific products from the appFigures
#' API. 
#'
#' @param product_ids Numeric. A vector of appFigures-assigned product ids.
#' 
#' @param country Character. One or more country iso abbreviations that filter
#' which countries rank data to include. Defaults to "US".
#' 
#' @param category An atomic character or numeric vector. See Details.
#' 
#' @param end_date Character string or date object. Date of last ranks to be
#' reported. Defaults to today. See Details.
#' 
#' @param start_date Character string or date object. Date of first ranks to be
#' reported. Defaults to 31 days prior to the \code{end_date}. See Details.
#' 
#' @param filter Limit results to ranks in the top N with N being a number
#' between 1 and 400. A filter value of 100 will only show records that are
#' ranked 1 - 100. Defaults to 400.
#' 
#' @param granularity Character. Should ranks be reported on a daily or hourly
#' basis.
#' 
#' @param tz Character. Specifies which timezone to use. Defaults to \code{utc}.
#' 
#' @param curlHandle Provide an instance of the CURLHandle-class from the RCurl
#' package. The default will create a curl handle specific to the function call.
#' 
#' @param verbose Logical. Should details of the web request print to the
#' console? Defaults to \code{FALSE}.
#' 
#' @param orgJSON Logical. Should the JSON string be returned with out being
#' converted to R objects? Defaults to \code{FALSE}.
#' 
#' @param async Logical. Should the given product ids be split up into groups
#' and and run concurrently? Defaults to \code{FALSE}.
#' 
#' @param agroups Integer. The number of concurrent calls to make.
#' 
#' @details The \code{category} argument is not included in the web request.
#' Subsetting the returned rank data on this argument occurs after the data has
#' been transformed into R objects and is included purely as a convenience.
#' Details on store categories can be requested using \code{\link{getStoreData}}.
#' 
#' For \code{start_date} and \code{end_date}, if the supplied argument can be
#' interepreted as a date or POSIX object, any hour, minute, or second is
#' ignored. If a string is supplied, it should have the format: 'yyyy-MM-dd'.
#' 
#' @return Depends on the selected options. If \code{orgJSON = F}, a data.frame is
#' returned. If \code{orgJSON = T}, a character vector of json strings is
#' returned, the length of which is determined by the integer specified by
#' \code{agroups}.
#' 
#' @seealso Official documentation:
#' \url{http://docs.appfigures.com/api/reference/v2/ranks}.
#' 

getRanks <- function(product_ids, country = "US", category, end_date, start_date,
                     filter = 400, granularity = c("daily", "hourly"),
                     tz = c("utc", "est", "user"), curlHandle, verbose = FALSE,
                     orgJSON = FALSE, async = FALSE, agroups = 2) {
  
  if (missing(product_ids))
    stop("No product id(s) given.")
  if (missing(end_date)) {
    end_date <- as.character(Sys.Date())
  } else end_date <- as.character(as.Date(end_date))
  if (missing(start_date)) {
    start_date <- as.character(as.Date(end_date) - 31)
  } else start_date <- as.character(as.Date(start_date))
  
  granularity <- match.arg(granularity)
  tz <- match.arg(tz)
  if (async) {
    stopifnot(length(product_ids) > agroups)
    product_ids <- split(product_ids, cut(seq_along(product_ids), agroups, F))
    product_ids <- lapply(product_ids, paste, collapse = ";")
  } else {
    product_ids <- paste(product_ids, collapse = ";")
  }
  country <- paste(country, collapse = ";")
  uri <- sprintf(paste(BASE_URI, "ranks", "%s", granularity, start_date,
                 end_date, sep = "/"), product_ids)
  parList <- c(format = 'json', tz = tz, countries = country, filter = filter)
  
  if (missing(curlHandle)) {
    opts <- list(userpwd = paste(USERNAME, PASSWORD, sep = ":"),
                 httpheader = c('X-Client-Key' = API_KEY),
                 httpauth = 1L, verbose = FALSE,
                 ssl.verifypeer = FALSE)
    curlHandle <- getCurlHandle(.opts = opts)
  } else if (!inherits(curlHandle, "CURLHandle")) {
    stop("curlHandle must be of class 'CURLHandle'.")
  } else {
    curlHandle <- curlHandle
  }
  if (async) {
    pairz <- mapply(function(k, v) {
      paste(curlEscape(k), curlEscape(v), sep = "=")}, names(parList), parList)
    args = paste(pairz, collapse = "&")
    uris <- mapply(paste, uri, args, sep = "?")
    jsonText <- getURIAsynchronous(uris, .opts = opts)
  } else {
    jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  }
  if (orgJSON)
    return(jsonText)
  if (any(unlist(lapply(jsonText, Negate(validate)))))
    stop("appFigures API yielded invalid JSON!")
  output <- do.call(rbind, lapply(jsonText, parseRanks))
  rownames(output) <- NULL
  if (!missing(category)) {
    if (is.character(category)) {
      output <- output[tolower(output$c_name) %in% tolower(category), ]
    } else if (is.numeric(category)) {
      output <- output[output$c_id %in% category, ]
    }
  }
  attr(output, 'granularity') <- granularity
  output
}


#' Map JSON string to an R data frame.
#'
#' \code{parseRanks} parses the JSON returned by an app rank request made to the
#'  appFigures web API.
#'

parseRanks <- function(jsonText) {
  datr <- fromJSON(jsonText)
  rr <- length(datr$dates)
  dd <- nrow(datr$data)
  delts <- unlist(lapply(datr$data$positions, function(ll) c(NA, diff(ll))))
  if (rr == 0 || is.null(dd))
    return(data.frame())
  data.frame(
    time_stamp = as.POSIXct(rep(datr$dates, dd), format = "%Y-%m-%dT%H:%M:%S"),
    product_id = rep(datr$data$product_id, each = rr),
    rank = unlist(datr$data$positions),
    iso2 = rep(datr$data$country, each = rr),
    store = rep(datr$data$category$store_id, each = rr),
    device = rep(datr$data$category$device_id, each = rr),
    c_id = rep(datr$data$category$id, each = rr),
    c_name = rep(datr$data$category$name, each = rr),
    c_subcat = rep(datr$data$category$subtype, each = rr),
    delta_api = unlist(datr$data$deltas),
    delta = delts,
    stringsAsFactors = F
  )
}


# ---------------------------------------------------------------------


#' Retrieve a cross section of rank data.
#'
#' \code{getRankSnapshot} requests data for the top ranked apps for for a
#' specific hour from the appFigures web API. 
#'
#' @param timestamp Either the string "current" (default) or a string with the
#' format: "yyyy-MM-ddTHH". Only the last 24 hours are available. 
#' 
#' @param country Chatacter. A country iso2 abbreviation. Defaults to 'US'.
#' 
#' @param category Numeric. The numeric category of the requested snapshot.
#' Defaults to \code{25204} (ios top overall).
#' 
#' @param subcategory Character. The sub-category of the requested#' snapshot.
#' Defaults to \code{"free"}.
#' 
#' @param count Numeric. The total number of the ranks to return. See Details.
#' 
#' @param start Smallest rank value the request returns. See Details.
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
#' @param async Logical. Should the given time-stamps run concurrently? Defaults
#' to \code{FALSE}. See Details.
#' 
#' @details The use of the \code{start} argument differs from the official API 
#' documentation. A value such as \code{start = 15} will return a list of
#' products starting with a rank value of 15. If \code{start = 15} and
#' \code{count = 10}, then products data for apps ranked from 15 - 24 will be
#' returned.
#' 
#' As of now, multiple time-stamp requests are the only input
#' variable that can run concurrently.
#' 
#' @return A data frame containing product and category data for 
#' a cross section of top ranked apps.
#' 
#' @seealso Official documentation:
#' \url{http://docs.appfigures.com/api/reference/v2/ranks}.
#' 

getRankSnapshot <- function(timestamp, category = 25204,                            
                            subcategory = c("free", "paid", "topgrossing"),
                            country = "US", count = 400, start = 1, tz = 'utc',
                            curlHandle, verbose = FALSE, orgJSON = FALSE,
                            async = FALSE) {
  
  stopifnot(length(country) == 1,
            length(category) == 1)
  subcategory <- match.arg(subcategory)
  if (missing(timestamp))
    timestamp <- "current"
  if (length(timestamp) > 1 && async == FALSE)
    stop("If requesting more than one snapshot, async must be set to TRUE.")
  if ("current" %in% timestamp) {
    if (length(timestamp) > 1) {
      stop("'current' can not be included in the timestamps supplied to an asynchronous
           request.")
    }
  } else {
    timestamp <- paste(substr(timestamp, 1, 10), "T", substr(timestamp, 12, 13),
                       sep = "")
  }
  uri <- sprintf(paste(BASE_URI, "ranks", "snapshots", "%s", country, category,
                 subcategory, sep = "/"), timestamp)
  parList <- list(count = count, start = start - 1, format = 'json', tz = tz)
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
  if (async) {
    pairz <- mapply(function(k, v) {
      paste(curlEscape(k), curlEscape(v), sep = "=")}, names(parList), parList)
    args = paste(pairz, collapse = "&")
    uris <- mapply(paste, uri, args, sep = "?")
    jsonText <- getURIAsynchronous(uris, .opts = opts)
  } else {
    jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  }
  if (orgJSON) {
    return(jsonText)
  }
  if (any(unlist(lapply(jsonText, Negate(validate))))) {
    stop("appFigures API yielded invalid JSON!")
  }
  output <- do.call(rbind, lapply(jsonText, parseSnapshot))
  rownames(output) <- NULL
  attr(output, 'balanced') <- FALSE
  attr(output, 'granularity') <- 'hourly'
  output
}



#' Map JSON string to an R data frame.
#'
#' \code{parseSnapshot} parses the JSON returned by a snapshot
#' request made to the appFigures web API.
#'

parseSnapshot <- function(jsonText) {
  datr <- fromJSON(jsonText)
  if (!is.null(datr$status) && datr$status == 404)
    return(data.frame())
  st <- datr$page_start + 1
  ed <- datr$page_count + datr$page_start
  data.frame(
    rank = st:ed,
    product_id = datr$entries$id,
    p_name = strtrim(datr$entries$name, 40),
    p_dev = strtrim(datr$entries$developer, 25),
    time_stamp = strptime(datr$timestamp, format = "%Y-%m-%dT%H:%M:%S"),
    store = datr$category$store_id,
    device = datr$category$device_id,
    price = as.numeric(datr$entries$price$price),
    currency = datr$entries$price$currency,
    iso2 = datr$country,
    c_id = datr$category$id,
    c_name = datr$category$name,
    c_subcat = datr$category$subtype,
    released = as.POSIXct(datr$entries$release_date, format = "%Y-%m-%dT%H:%M:%S"),
    added = as.POSIXct(datr$entries$added_date, format = "%Y-%m-%dT%H:%M:%S"),
    updated = as.POSIXct(datr$entries$updated_date, format = "%Y-%m-%dT%H:%M:%S"),
    stringsAsFactors = F
  )
}

