#' Retrieve Ad reports.
#'
#' \code{getAdReport} provides access to detailed ad data across
#' all supported ad networks. 
#'
#' @param product_ids Numeric. A vector of appFigures-assigned
#' product ids. The default will return ad reports for all products
#' in the account.
#' 
#' @param end_date Character string or date object. Date of last ranks
#' to be reported. Defaults to today. See Details.
#' 
#' @param start_date Character string or date object. Date of first
#' ranks to be reported. Defaults to the last 31 days. See Details.
#' 
#' @param group_by Character. Choose one or more of:
#' \code{"dates", "products", "store", "countries", or "regions"}
#' 
#' @param country Character. One or more country iso2 abbreviations.
#' Defaults to all countries.
#' 
#' @param networks Character. One or more ad networks. See Details.
#'  
#' @param format Character. Choose between \code{"flat", "csv",
#' or "json"}. Defaults to \code{"flat"}. See Details
#' 
#' @param granularity Character. Should ranks be reported on a
#' \code{"daily", "weekly", "monthly", or "yearly"} basis.
#' See Details.
#' 
#' @param curlHandle Provide an instance of the CURLHandle-class
#' from the RCurl package. The default will create a curl handle
#' specific to the function call.
#' 
#' @param verbose Logical. Should details of the web request
#' print to the console? Defaults to \code{FALSE}. Irrelevant if 
#' \code{format = 'csv'}.
#' 
#' @param orgJSON Logical. Should the JSON string be returned
#' without being converted to R objects? Defaults to \code{FALSE}. If 
#' \code{format = 'csv'}, the data is returned as a character string in
#' csv format.
#' 
#' @details For \code{start_date} and \code{end_date}, if the
#' supplied argument can be interepreted as a date or POSIX
#' object, any hour, minute, or second is ignored. If a string
#' is supplied, it should have the format: 'yyyy-MM-dd'.
#' 
#' The argument \code{granularity} only matters if \code{group_by}
#' contains \code{"dates"}.
#' 
#' Network names to use in the \code{networks} argument can be
#' found by using \code{\link{getStoreData}} with the \code{tables}
#' argument set to \code{"stores"}.
#' 
#' For the \code{format} argument, \code{"csv"} and \code{"flat"}
#' return similar fields. Both values will truncate some of the data
#' returned in the original JSON. At times (i.e. \code{group_by =
#' "products"}),  \code{"csv"} will contain a few more fields
#' than \code{"flat"}. Since different combinations of
#' \code{group_by} arguments will lead to different nested JSON
#' structures, if \code{format} is set to \code{"json"}, the
#' unformatted JSON string will be returned.
#' 
#' @return A data frame containing the requested ad report.
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/api/reference/v2/ads}.
#' 
#' @seealso A list of suppported ad networks can be found here:
#' \url{https://appfigures.com/integrations}.

getAdReport <- function(product_ids, end_date, start_date,
                        group_by, country, networks,
                        format = c("flat", "csv", "json"),
                        granularity = c("daily", "weekly", "monthly", "yearly"),
                        curlHandle, verbose = FALSE, orgJSON = FALSE) {
  
  uri <- paste(BASE_URI, "reports", "sales", sep = "/")
  granularity = match.arg(granularity)
  format <- match.arg(format)
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
    paste(product_ids, collapse = ";")
  }
  country <- if (!missing(country)) {
    paste(country, collapse = ";")
  }
  networks <- if (!missing(networks)) {
    paste(networks, collapse = ",")
  }
  group_by <- if (!missing(group_by)) {
    paste(group_by, collapse = ",")
  }
  parList <- c(products = product_ids, end_date = end_date,
               start_date = start_date, countries = country,
               granularity = granularity, networks = networks,
               format = format, group_by = group_by)
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
  jsonText <- getForm(uri, .opts = opts, .params = parList)
  if (orgJSON || format == "json") {
    return(jsonText) 
  }
  if (format == "csv") {
    conn <- textConnection(jsonText)
    out <- read.csv(file = conn, header = T, stringsAsFactors = F)
    close(conn)
    return(out)
  }
  if (!validate(jsonText)) {
    stop("appFigures API yielded invalid JSON!")
  }
  output <- fromJSON(jsonText)
  output$revenue <- as.numeric(output$revenue)
  output$returns_amount <- as.numeric(output$returns_amount)
  output$educational_revenue <- as.numeric(output$educational_revenue)
  if (all(c("start_date", "end_date") %in% names(output))) {
    output$start_date <- as.Date(output$start_date)
    output$end_date <- as.Date(output$end_date)
  }
  output
}