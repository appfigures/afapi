#' Retrieve products by store id or by appFigures-assigned id.
#'
#' \code{getProducts} requests product specific data from the appFigures API. 
#'
#' @param product_id Either the string value \code{"mine"} or the id of a
#' specific product. Defaults to \code{"mine"}. See Details.
#' 
#' @param store A character string to filter products by store. Defaults to
#' "all" stores.
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
#' @details Products represent a trackable item in the appFigures system.
#' Products can be an app as well as an in-app purchase or book. The user can
#' request data on all their products by setting the id variable to
#' \code{"mine"}. Data for products can be requested by either their store
#' specific ids or by their appFigures-assigned ids. If more than one product_id
#' is supplied, the requests are made concurrently.
#' 
#' @return A data frame containing all publicly available fields.
#' 
#' @seealso Official documentation:
#' \url{http://docs.appfigures.com/products}.
#' 

getProducts <- function(product_id, store = c("all", "apple", "google",
                        "amazon", "windows"), curlHandle,
                        verbose = FALSE, orgJSON = FALSE) {
  
  if (missing(product_id))
    product_id <- "mine"
  store <- match.arg(store)
  if (product_id[1] == "mine") {
    uri <- paste(BASE_URI, "products", "mine", sep = "/")
    parList <- if (store != "all") c(store = store)
  } else {
    if (store == "all" && !missing(product_id)) {
      message("Store is set to `all`. Assuming `product_id` is the
              appFigures-assinged id.")
      uri <- sprintf(paste(BASE_URI, "products", "%s", sep = "/"), product_id)
      parList <- NULL
    } else if(store != "all" && !missing(product_id)) {
      message("Assuming `product_id` is store specific id.")
      uri <- sprintf(paste(BASE_URI, "products", "%s", sep = "/"), product_id)
      parList <- NULL
    } else stop("appFigures or Store `id` must be specified for this function.")
  }
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
  if (length(product_id) > 1) {
    jsonText <- getURIAsynchronous(uri, .opts = opts)
  } else {
    jsonText <- getForm(uri, curl = curlHandle, .params = parList)
  }
  if (orgJSON)
    return(jsonText)
  
  if (any(unlist(lapply(jsonText, Negate(validate)))))
    # To Do: Return all those that DID NOT fail
    stop("appFigures API yielded invalid JSON!")
  
  return(do.call(rbind, lapply(jsonText, parseProducts)))
}



#' Map JSON string to an R data frame.
#'
#' \code{parseProducts} parses the JSON returned by a product
#' request made to the appFigures web API.

parseProducts <- function(jsonText) {
  jsonText <- gsub("null", -999, jsonText)
  datr <- fromJSON(jsonText)
  # Helper function
  vapply2 <- function(vb, cl) {
    tst <- lapply(datr, `[[`, vb)
    vapply(tst, function(x) as(x, cl), vector(cl, 1),
           USE.NAMES = F)
  }
  if (length(datr[[1]]) == 1) {
    out <- data.frame(
      product_id = datr[["id"]],
      p_name = datr[["name"]],
      p_dev = datr[["developer"]],
      vendor_id = datr[["vendor_identifier"]],
      refno = datr[["ref_no"]],
      sku = datr[["sku"]],
      package_name = datr[["package_name"]],
      store_id = datr[["store_id"]],
      store = datr[["store"]],
      storefronts = paste(datr[["storefront"]], collapse = "/"),
      release_date = as.POSIXct(datr[["release_date"]],
                                format = "%Y-%m-%dT%H:%M:%S"),
      added_date = as.POSIXct(datr[["added_date"]],
                              format = "%Y-%m-%dT%H:%M:%S"),
      updated_date = as.POSIXct(datr[["updated_date"]],
                                format = "%Y-%m-%dT%H:%M:%S"),
      version = datr[["version"]],
      type = datr[["type"]],
      devices = paste(datr[["devices"]], collapse = "/"),
      bundle_id = datr[["bundle_identifier"]],
      price = as.numeric(datr[["price"]][["price"]]),
      currency = datr[["price"]][["currency"]],
      icon = datr[["icon"]],
      stringsAsFactors = FALSE, row.names = NULL
    )
  } else {
    prices <- lapply(datr, `[[`, "price")
    out <- data.frame(
      product_id = vapply2("id", "numeric"),
      p_name = vapply2("name", "character"),
      p_dev = vapply2("developer", "character"),
      vendor_id = vapply2("vendor_identifier", "character"),
      refno = vapply2("ref_no", "character"),
      sku = vapply2("sku", "character"),
      package_name = vapply2("package_name", "character"),
      store_id = vapply2("store_id", "integer"),
      store = vapply2("store", "character"),
      release_date = as.POSIXct(vapply2("release_date", "character"),
                                format = "%Y-%m-%dT%H:%M:%S"),
      added_date = as.POSIXct(vapply2("added_date", "character"),
                              format = "%Y-%m-%dT%H:%M:%S"),
      updated_date = as.POSIXct(vapply2("updated_date", "character"),
                                format = "%Y-%m-%dT%H:%M:%S"),
      version = vapply2("version", "character"),
      type = vapply2("type", "character"),
      devices = vapply(lapply(datr, `[[`, "devices"), paste,
                       character(1), collapse = "/"),
      bundle_id = vapply2("bundle_identifier", "character"),
      features = vapply(datr, function(x) paste(x[["features"]],
                                                collapse = "/"),
                        character(1)),
      accessible_feat = vapply(datr,
                  function(x) paste(x[["accessible_features"]], collapse = "/"),
                  character(1)),
      children = vapply(datr,function(x) paste(x[["children"]], collapse = "/"),
                        character(1)),
      parent_id = vapply2("parent_id", "numeric"),
      storefronts = vapply(lapply(datr, `[[`, "storefronts"), paste,
                           character(1), collapse = "/"),
      price = vapply(prices, function(x) as.numeric(x[["price"]]),
                     numeric(1)),
      currency = vapply(prices, function(x) as.character(x[["currency"]]),
                        character(1)),
      icon = vapply2("icon", "character"),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }
  out
}
