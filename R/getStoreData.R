#' Retrieve general metadata used in other API requests.
#'
#' \code{getStoreData} can request data on store categories, country
#' abbreviations, supported languages for review translation, and
#' supported currencies from the appFigures web API. 
#'
#' @param tables Character. One of five choices: "categories", 
#' "countries", "currencies", "languages", "stores". See Details.
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
#' @details For \code{tables} set to \code{"categories", "countries",
#' "currencies", or "languages"}, a single data frame is returned 
#' containing the relevant metadata available across all stores. If
#' \code{tables} is set to \code{"stores"}, a list of five data frames
#' is returned. The first four data frames in the list contains all
#' metadata relevant for a particular store (e.g. apple, google_play,
#' ect.). The last data frame summarizes all the supported ad
#' networks.
#' 
#' @return A data frame containing the appropriate table, or if
#' \code{tables = "stores"}, a list of five data frames.
#' 
#' @seealso Official documentation:
#' \url{http://docs.appfigures.com/api/reference/v2/data}.
#' 

getStoreData <- function(tables = c("categories", "countries",
                         "currencies", "languages", "stores"),
                         curlHandle, verbose = FALSE, orgJSON = FALSE) {
  
  tables <- match.arg(tables)
  uri <- paste(BASE_URI, "data", tables, sep = "/")
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
  jsonText <- getForm(uri, curl = curlHandle)
  if (orgJSON) {
    return(jsonText)
  }
  if (!validate(jsonText)) {
    stop("appFigures API yielded invalid JSON!")
  }
  if (tables == "categories") {
    jsonText <- gsub("null", -999, jsonText)
    datr <- fromJSON(jsonText)
    dat <- lapply(1:length(datr),
                  function(node) {
                    data.frame(category_id = datr[[node]]$id,
                               store = datr[[node]]$store,
                               name = datr[[node]]$name,
                               device = datr[[node]]$device,
                               subcategory = paste(datr[[node]]$subcategories,
                                                     collapse = "/"),
                               active = datr[[node]]$active,
                               parent_id = datr[[node]]$parent_id,
                               stringsAsFactors = F)
                  })
    output <- do.call(rbind, dat)
    return(output[order(output$store, output$category_id),])
  } else if (tables == "countries"){
    datr <- fromJSON(jsonText)
    dat <- lapply(1:length(datr),
                  function(node) {
                    data.frame(iso2 = datr[[node]]$iso,
                               name = datr[[node]]$name,
                               apple_store_no = datr[[node]]$apple_store_no,
                               stringsAsFactors = F)
                  })
    output <- do.call(rbind, dat)
    return(output[order(output$iso2),])
  } else if (tables == "currencies") {
    datr <- fromJSON(jsonText)
    return(datr)
  } else if (tables == "languages") {
    datr <- fromJSON(jsonText)
    dat <- lapply(1:length(datr),
                  function(node) {
                    data.frame(language = datr[[node]]$name,
                               code = datr[[node]]$code,
                               ios2 = datr[[node]]$iso,
                               stringsAsFactors = F)
                  })
    output <- do.call(rbind, dat)
    return(output)
  } else if (tables == "stores") {
    return(parseFullStoreData(jsonText))
  } else {
    return("Something is broken.")
  }
}



#' Map JSON string to an R data frame.
#'
#' \code{parseFullStoreData} parses the JSON returned by a
#' request made to the appFigures web API.
#'

parseFullStoreData <- function(jsonText) {
  datr <- fromJSON(jsonText)
  # store formatting
  format_stores <- function(lst) {
    st_dat <- data.frame(lst[1:5],
                         features = paste(lst[[6]], collapse = "/"),
                         stringsAsFactors = F)
    st_front <- lst$storefronts
    st_ctry <- lst$countries
    st_cats <- lst$categories[, c("id", "store", "device", "parent_id",
                                  "name", "active")]
    subcat <- sapply(lst$categories$subcategories, paste, collapse = "/")
    st_cats <- data.frame(st_cats, subcat, stringsAsFactors = F)
    names(st_cats)[c(1, 5, 7)] <- c("category_id", "c_name", "c_subcat")
    list(store_data = st_dat,
         storefront = st_front,
         store_countries = st_ctry,
         store_categories = st_cats)
  }
  stores <- lapply(datr[1:4], format_stores)
  # ad network formatting
  format_adnetworks <- function(lst) {
    data.frame(t(unlist(lst[1:6])),
               lst$countries,
               stringsAsFactors = F)
  }
  adnetworks <- lapply(datr[5:length(datr)], format_adnetworks)
  adnetworks <- do.call(rbind, adnetworks)
  adnetworks$code <- as.numeric(adnetworks$code)
  adnetworks$apple_store_no <- as.numeric(adnetworks$apple_store_no)
  names(adnetworks)[8] <- "country"
  row.names(adnetworks) <- NULL
  
  list(apple = stores[[1]], google_play = stores[[2]],
       amazon_appstore = stores[[3]], widnows_phone = stores[[4]],
       adnetworks = adnetworks)
}
