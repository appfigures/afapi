#' Retrieve review reports.
#'
#' \code{getReviews} provides access to detailed review data
#' from the appFigures web API 
#'
#' @param product_ids Numeric. A vector of appFigures-assigned
#' product ids. Defaults to all products in the account.
#' 
#' @param q Character. Only return reviews matching this character
#' string.
#' 
#' @param page Integer. Page number. Defaults to one.
#' 
#' @param count Integer. The number of reviews to return. Any number
#' between 1 and 500 is valid. Defaults to 25.
#' 
#' @param lang Character. Short language code to translate reviews
#' into. See Details.
#' 
#' @param author Character. Name of an author to filter reviews.
#' 
#' @param country One or more country iso codes. Defaults to all
#' countries. See Details.
#' 
#' @param versions Character. One of more version numbers of an app to
#' filter reviews on.
#' 
#' @param stars Integer. Values of one to five to filter reviews on.
#' Defaults to reviews with any number of stars.
#' 
#' @param sort Character. How to sort the resulting reviews. Choose
#' from \code{"date", "country", or "stars"}. Defaults to \code{"date"}.
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
#' Regarding the \code{lang} and \code{country} arguments, a
#' complete list of supported languages can be found using
#' \code{\link{getStoreData}}.
#' 
#' Also, since the results are paginated, the returned data frame
#' has an attached attribute called 'header', which provides data
#' on the total number of reviews available, the current page number,
#' and the total number of pages (given 'count' = xx).d
#' 
#' @return A data frame containing the requested review report.
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/api/reference/v2/reviews}.
#' 

getReviews <- function(product_ids, q = NULL, page = 1, count = 25,
                       lang = NULL, author = NULL, country, version,
                       stars, sort = c("date", "country", "stars"),
                       start_date, end_date, curlHandle,
                       verbose = FALSE, orgJSON = FALSE)  {
  
  uri <- paste(BASE_URI, "reviews", sep = "/")
  sort <- match.arg(sort)
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
  product_ids <- if (!missing(product_ids)) {
    paste(product_ids, collapse = ",")
  }
  country <- if (!missing(country)) {
    paste(country, collapse = ",")
  }
  version <- if (!missing(version)) {
    paste(version, collapse = ",")
  }
  stars <- if (!missing(stars)) {
    paste(stars, collapse = ",")
  }
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
  parList <- c(format = 'json', countries = country, page = page,
               lang = lang, author = author, q = q, versions = version,
               stars = stars, sort = sort, start = start_date,
               end = end_date, products = product_ids, count = count)
  jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  if (orgJSON) {
    return(jsonText)
  }
  if (!validate(jsonText)) {
    stop("appFigures API yielded invalid JSON!")
  }
  output <- parseReviews(jsonText)
  header <- attr(output, "header")
  if (header["total"] == 0) {
    warning("Product id returned no reviews", call. = FALSE)
  }
  output
}



#' Map JSON string to an R data frame.
#'
#' \code{parseReviews} parses the JSON returned by an app
#' review request made to the appFigures web API.
#'

parseReviews <- function(jsonText) {
  dat <- fromJSON(jsonText)
  plang <- sapply(dat$reviews$predicted_langs,
                  function(ll) paste(ll, collapse = ","))

  header <- c(total = dat$total, total_pages = dat$pages,
    this_page = dat$this_page)
  if (header["total"] == 0) {
    out <- data.frame()
    attr(out, "header") <- header
    return(out)
  }
  reviews <- data.frame(
    product_id = as.numeric(dat$reviews$product),
    time_stamp = as.POSIXct(dat$reviews$date,
                            format = "%Y-%m-%dT%H:%M:%S"),
    stars = as.integer(dat$reviews$stars),
    iso2 = dat$reviews$iso,
    version = as.character(dat$reviews$version),
    weight = as.numeric(dat$reviews$weight),
    author = dat$reviews$author,
    review_id = dat$reviews$id,
    predicted_langs = plang,
    org_title = dat$reviews$original_title,
    trans_title = dat$reviews$title,
    org_review = dat$reviews$original_review,
    trans_review = dat$reviews$review,
    stringsAsFactors = F)
  attr(reviews, "header") <- header
  reviews
}


# --------------------------------------------------------------------------


#' Retrieve review counts.
#'
#' \code{getReviewCounts} provides access to detailed review data
#' from the appFigures web API 
#'
#' @param product_ids Numeric. A vector of appFigures-assigned
#' product ids. Defaults to all products in the account.
#' 
#' @param q Character. Only return reviews matching this character
#' string.
#' 
#' @param author Character. Name of an author to filter reviews.
#' 
#' @param version Character. One of more version numbers of an app to
#' filter reviews on.
#' 
#' @param country One or more country iso codes. Defaults to all
#' countries. See Details.
#' 
#' @param stars Integer. Values of one to five to filter reviews on.
#' Defaults to reviews with any number of stars.
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
#' Regarding the \code{country} argument, a complete list of
#' supported languages can be found using \code{\link{getStoreData}}.
#' 
#' @return A a list of six named vectors breaking down the review
#' counts. Review counts are broken down by stars, versions,
#' countries, product ids, languages, and tags.
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/api/reference/v2/reviews}.
#' 
getReviewCounts <- function(product_ids, q = NULL, country, author,
                            version, stars, start_date, end_date,
                            curlHandle, verbose = FALSE,
                            orgJSON = FALSE)  {
  
  uri <- paste(BASE_URI, "reviews", "count", sep = "/")
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
  product_ids <- if (!missing(product_ids)) {
    paste(product_ids, collapse = ",")
  }
  country <- if (!missing(country)) {
    paste(country, collapse = ",")
  }
  author <- if (!missing(author)) {
    paste(author, collapse = ",")
  }
  version <- if (!missing(version)) {
    paste(version, collapse = ",")
  }
  stars <- if (!missing(stars)) {
    paste(stars, collapse = ",")
  }
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
  parList <- c(format = 'json', countries = country,
               author = author, q = q, versions = version,
               stars = stars, products = product_ids,
               start = start_date, end = end_date)
  jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  if (orgJSON) {
    return(jsonText)
  }
  if (!validate(jsonText)) {
    stop("appFigures API yielded invalid JSON!")
  }
  parseReviewCounts(jsonText)
}



#' Map JSON string to a list of named vectors.
#'
#' \code{parseReviewCounts} parses the JSON returned by an app
#' review request made to the appFigures web API.
#'
parseReviewCounts <- function(jsonText) {
  datr <- fromJSON(jsonText)
  lapply(datr, unlist)
}
