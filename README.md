# afapi

afapi is an R interface for the appFigures API. It uses RCurl for html requests and parses the (JSON) responses with jsonlite. Aside from the documentation contained in this package, the [original documentation](http://docs.appfigures.com/) is a great reference for the available routes. A conscious effort has been made to keep the afapi naming patterns as similar as possible to those described in the official documentation.

## Installation

To install the dependencies, use the CRAN repository:

```R
install.packages("RCurl")
install.packages("jsonlite")
```

The development version from github:

```R
# install.packages("devtools")
devtools::install_github("appfigures/appfigures-r")
```

The following examples demonstrate the basics of how to get up and running with afapi.

## HTTP Authentication

Before any requests are made, the package needs to be loaded and the constants for HTTP authentication need to be set.

```R
library(afapi)

# These four variables need to be loaded into the global environment.
USERNAME <- 'your_username_here'
PASSWORD <- 'your_password_here'
API_KEY <- 'your_api_key_here'
BASE_URI <- 'https://api.appfigures.com/v2'
```

## Requesting Metadata

The following bloc of code requests product metadata in various ways.

```R
# Check data usage before making a bunch of calls to the API
usage = getUsage()
usage["api_requests", c("today", "remaining")]

# Get a list of all your products
getProducts("mine")

# You can request product metadata through the appFigures assigned product id:
getProducts(41308333846)

# Or by the vendor id which is store specific:
getProducts(962408539, store = "apple")
getProducts("com.snapchat.android", store = "google")

# Or by searching through products
searchProducts("appFigures")
(aF <- searchProducts("@developer=appFigures"))
```

## Ranks Data

Rank data can be obtained one of two ways. Either you can search the ranks of a particular app using its product id for a (possible) group of time series:

 ```R
 # Search for an app whose developer is 'Snapchat' and is listed in the Google Play Store
 (snapchat <- searchProducts("@developer=Snapchat", filter = "google"))
 
 # Daily rank data for the past 30 days (default) from the United Kingdom
 getRanks(snapchat$product_id, granularity = "daily", country = "GB")
 ```

Or you can search for a cross-section of the top apps for a particular category/subcategory combination. Below is a more involved example. We will obtain a list of the top overall apps for Google Play, the Amazon App Store, and the Apple App Store.

```R
# Bring in data on store categories
cats <- getStoreData("categories")

# Find active categories that have 'Top Overall' in their category name,
# excluding Windows apps
top_cats <- subset(cats, grepl("Top Overall", cats$name) & active & store != "windows_phone")

# Create a curl handle to use for all snapshot requests.
# The default is to use a separate handle for each request.
ch <- getCurlHandle(.opts = list(userpwd = paste(USERNAME, PASSWORD, sep = ":"),
                                 httpheader = c('X-Client-Key' = API_KEY),
                                 httpauth = 1L,
                                 verbose = FALSE,
                                 ssl.verifypeer = FALSE))

# Make request for the ranks in each top overall category.
top_ranks <- lapply(top_cats$category_id,
                    function(x) getRankSnapshot(category = x, subcategory = "free",
                                                curlHandle = ch)
top_ranks
```

## Reviews

Retrieving reviews is simple enough.

```R
# Using the appFigures metadata stored earlier
getReviews(aF$product_id)
```

For an app that has a high volume of reviews, multiple calls to 'getReviews()' will be needed. To aid in pulling in all reviews, the data frame that is returned by a call to 'getReviews()' will have an attribute named 'header'. This attribute contains information on the total number of reviews, the total number of pages, and the current page. Use this information to loop through multiple calls of the function:

```R
# Using the Snapchat metadata stored earlier, search for all reviews in the past seven days:
rev1 <- getReviews(snapchat$product_id, start_date = Sys.Date() - 3, count = 50,
		   page = 1)

# Multiple calls to 'getReview()' should follow, where each call changes the
# 'page' argument (page = page + 1)
npages <- attr(rev1, "header")["total_pages"]
out <- vector("list", npages - 1)
for (np in 2:npages) {
  out[[np]] <- getReviews(snapchat$product_id, start_date = Sys.Date() - 3,
                          count = 50, page = np)
}
do.call(rbind, c(list(rev1), out))

```

## Final Example

If, for any reason, you dislike any of the JSON --> R mappings, most of the get* functions have an argument 'orgJSON', which will return the original JSON string from the appFigures API. Also, 'verbose = TRUE' will provide details about the web requests.

```R
jsonlite::prettify(getUsage(orgJSON = TRUE, verbose = TRUE))
```
