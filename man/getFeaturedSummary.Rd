% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFeatured.R
\name{getFeaturedSummary}
\alias{getFeaturedSummary}
\title{Retrieve featured summary reports.}
\usage{
getFeaturedSummary(product_ids, end_date, start_date, curlHandle,
  verbose = FALSE, orgJSON = FALSE)
}
\arguments{
\item{product_ids}{Numeric. A vector of appFigures-assigned
product ids. Defaults to all account products.}

\item{end_date}{Character string or date object. Date of last ranks
to be reported. Defaults to today. See Details.}

\item{start_date}{Character string or date object. Date of first
ranks to be reported. Defaults to the last 31 days. See Details.}

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
\code{getFeaturedSummary} provides access to detailed featured
data from the appFigures web API
}
\details{
For \code{start_date} and \code{end_date}, if the
supplied argument can be interepreted as a date or POSIX
object, any hour, minute, or second is ignored. If a string
is supplied, it should have the format: 'yyyy-MM-dd'.

The list of countries included in the request is included as an
attribute.
}
\seealso{
Official documentation: 
\url{http://docs.appfigures.com/api/reference/v2/featured}.
}
