#' Retrieve external account details.
#'
#' \code{getExternalAccounts} provides details of a user's 
#' external accounts.
#' 
#' @param id Character or numeric external account id. The
#' default \code{NULL} value will return details on all accounts.
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
#' @return A data frame.
#' 
#' @seealso Official documentation: 
#' \url{http://docs.appfigures.com/api/reference/v2/external-accounts}.
#' 

getExternalAccounts <- function(id = NULL, curlHandle,
                                verbose = FALSE, orgJSON = FALSE) {
  
  if (length(id) > 1) {
    stop("There are two options:\n
          \t 1) Search all external accounts (id = NULL)\n
          \t 2) Search a single external account.")
  }
  parList <- c(format = 'json')
  if (is.null(id)) {
    uri <- paste(BASE_URI, "external_accounts", sep = "/")
  } else {
    uri <- paste(BASE_URI, "external_accounts", id, sep = "/")
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
  jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  if (orgJSON) {
    return(jsonText)
  }
  if (!validate(jsonText)) {
    stop("appFigures API yielded invalid JSON!")
  }
  parseExternalAccounts(jsonText, id)
}




#' Map JSON string to an R data frame.
#'
#' \code{parseExternalAccounts} parses the JSON returned by an
#' external account request made to the appFigures web API.
#'

parseExternalAccounts <- function(jsonText, id) {
  datr <- fromJSON(jsonText)
  formData <- function(lst) {
    ext_acct_id <- lst[["id"]]
    account_id <- lst[["account_id"]]
    nickname <- lst[["nickname"]]
    username <- lst[["username"]]
    auto_import <- lst[["auto_import"]]
    store_id <- lst[["store_id"]]
    store <- lst[["store"]]
    metadata <- paste(lst[["metadata"]], collapse = "/")
    mdnames <- names(lst[["metadata"]])
    data.frame(ext_acct_id, account_id, nickname, username,
               auto_import, store_id, store, metadata,
               stringsAsFactors = FALSE)
  }
  if (is.null(id)) {
    return(do.call(rbind, lapply(datr, formData)))
  } else {
    return(formData(datr))
  }
}