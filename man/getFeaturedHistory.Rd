% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFeatured.R
\name{getFeaturedHistory}
\alias{getFeaturedHistory}
\title{Retrieve featured history reports.}
\usage{
getFeaturedHistory(product_id, feature_id, curlHandle, verbose = FALSE,
  orgJSON = FALSE)
}
\arguments{
\item{product_id}{Numeric. A single appFigures-assigned
product ids.}

\item{feature_id}{Numeric. A single feature id. This can be found
using \code{\link{getFeaturedReport}}.}

\item{curlHandle}{Provide an instance of the CURLHandle-class
from the RCurl package. The default will create a curl handle
specific to the function call.}

\item{verbose}{Logical. Should details of the web request
print to the console? Defaults to \code{FALSE}.}

\item{orgJSON}{Logical. Should the JSON string be returned
without being converted to R objects? Defaults to \code{FALSE}.}
}
\value{
A list of dataframes, one for each product id.
}
\description{
\code{getFeaturedHistory} provides access to detailed featured
data from the appFigures web API
}
\seealso{
Official documentation: 
\url{http://docs.appfigures.com/api/reference/v2/featured}.
}
