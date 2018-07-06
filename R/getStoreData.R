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
#' etc.). The last data frame summarizes all the supported ad
#' networks.
#' 
#' @return A data frame containing the appropriate table, or if
#' \code{tables = "stores"}, a list of five data frames.
#' 
#' @seealso Official documentation:
#' \url{http://docs.appfigures.com/api/reference/v2/data}.
#' 

getStoreData <- function(tables = c("categories", "countries", "currencies",
                         "languages", "stores", "sdks"),
                         curlHandle, verbose = FALSE, orgJSON = FALSE) {
  
  tables <- match.arg(tables)
  uri <- paste(BASE_URI, "data", tables, sep = "/")
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
  } else if (tables == "sdks") {
    return(parseSDKData(jsonText))
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
  idx <- which(names(datr) %in% c("apple", "google_play", "amazon_appstore",
                                  "windows_phone", "windows10"))
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
  stores <- lapply(datr[idx], format_stores)
  # ad network formatting
  format_adnetworks <- function(lst) {
    data.frame(t(unlist(lst[1:6])),
               lst$countries,
               stringsAsFactors = F)
  }
  adnetworks <- lapply(datr[-idx], format_adnetworks)
  bb <- grep("blackberry", names(adnetworks))
  adnetworks <- do.call(rbind, adnetworks[-bb])
  adnetworks$code <- as.numeric(adnetworks$code)
  adnetworks$apple_store_no <- as.numeric(adnetworks$apple_store_no)
  names(adnetworks)[8] <- "country"
  row.names(adnetworks) <- NULL
  
  list(apple = stores[["apple"]], google_play = stores[["google_play"]],
       amazon_appstore = stores[["amazon_appstore"]], windows10 = stores[["windows10"]],
       windows_phone = stores[["windows_phone"]], adnetworks = adnetworks)
}




#' Map JSON string to an R data frame.
#'
#' \code{parseSDKData} parses the JSON returned by a
#' request made to the appFigures web API.
#'

parseSDKData <- function(jsonText) {

  datr <- fromJSON(jsonText)
  
  ext_link <- function(x, y) {
    if (nrow(x) == 0)
      return(data.frame())
    
    x$name <- y
    return(x)
  }
  external_links <- mapply(ext_link, datr$external_links, datr$name)
  external_links <- do.call(rbind, external_links)
  external_links <- external_links[, c("name", "type", "value")]
  
  datr$external_links <- NULL
  datr$description <- NULL
  datr$notes <- NULL
  datr$developer <- NULL
  
  datr$tags <- vapply(datr$tags, function(x) paste(x, collapse=", "),
                      FUN.VALUE=character(1))
  
  datr$release_date <- strptime(datr$release_date, format="%Y-%m-%dT%H:%M:%S",
                                tz="GMT")
  
  datr$started_tracking <- strptime(datr$started_tracking,
                                    format="%Y-%m-%dT%H:%M:%S",
                                    tz="GMT")

  datr$platforms <- vapply(datr$platforms, function(x) paste(x, collapse=", "),
                           FUN.VALUE=character(1))
  
  datr$tracked_platforms <- vapply(datr$tracked_platforms,
                                   function(x) paste(x, collapse=", "),
                                   FUN.VALUE=character(1))

  return(list(SDK_info=datr, External_links=external_links))  
}  
