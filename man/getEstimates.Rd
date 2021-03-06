% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getEstimates.R
\name{getEstimates}
\alias{getEstimates}
\title{Retrieve Estimates by appFigures-assigned id.}
\usage{
getEstimates(product_ids, countries = "US", category, end_date,
  start_date, curlHandle, verbose = FALSE, orgJSON = FALSE)
}
\arguments{
\item{product_ids}{Numeric. A vector of appFigures-assigned product ids.}

\item{end_date}{Character string or date object. Date of last estimate to be
reported. Defaults to today. See Details.}

\item{start_date}{Character string or date object. Date of first estimate to be
reported. Defaults to 31 days prior to the \code{end_date}. See Details.}

\item{curlHandle}{Provide an instance of the CURLHandle-class from the RCurl
package. The default will create a curl handle specific to the function call.}

\item{verbose}{Logical. Should details of the web request print to the
console? Defaults to \code{FALSE}.}

\item{orgJSON}{Logical. Should the JSON string be returned without being 
converted to R objects? Defaults to \code{FALSE}.}

\item{country}{Character. One or more country iso abbreviations that filter
which countries estimates data to include. Defaults to "US".}
}
\value{
A data frame containing all publicly available fields.
}
\description{
\code{getEstimates} requests product specific data from the appFigures API.
}
\details{
Estimates represent a prediction for apps. Both downloads and revenue estimates
are displayed.
}
\seealso{
Official documentation:
\url{http://docs.appfigures.com/}.
}
