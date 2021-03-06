% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/searchProducts.R
\name{searchProducts}
\alias{searchProducts}
\title{Search products by name or developer.}
\usage{
searchProducts(term, filter = NULL, page = 1, count = 25, curlHandle,
  verbose = FALSE, orgJSON = FALSE)
}
\arguments{
\item{term}{A character string to search for. See Details.}

\item{filter}{\code{NULL} value or a character string. Acceptable
string values are: "ios", "mac", "google", and "amazon". Default
\code{NULL} value returns all results.}

\item{page}{A numeric value indicating which page of results to show.
Defaults to the first page.}

\item{count}{A numeric value indicating the number of results to
show in a page. Defaults to 25.}

\item{curlHandle}{Provide an instance of the CURLHandle-class
from the RCurl package. The default will create a curl handle
specific to the function call.}

\item{verbose}{Logical. Should details of the web request
print to the console? Defaults to \code{FALSE}.}

\item{orgJSON}{Logical. Should the JSON string be returned
without being converted to R objects? Defaults to \code{FALSE}.}
}
\value{
A data frame containing all publicly available fields.
Fields containing meta data will be added later.
}
\description{
\code{searchProducts} searches for a product by name or developer
in the appFigures web API.
}
\details{
For the \code{term} argument, prefix the string
with \code{@name=} or \code{@developer=} if you want to search those fields
specifically. Any spaces included in the \code{term} string
will return an error. Instead, separate multiple terms with '+'.
}
\seealso{
Official documentation:
\url{http://docs.appfigures.com/products#searching_for_products}.
}
