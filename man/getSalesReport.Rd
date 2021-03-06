% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSalesReports.R
\name{getSalesReport}
\alias{getSalesReport}
\title{Retrieve sales reports.}
\usage{
getSalesReport(product_ids, end_date, start_date, group_by, country,
  dataset = c("none", "financial"), granularity = c("daily", "weekly",
  "monthly", "yearly"), format = c("flat", "csv", "json"),
  include_inapps = FALSE, curlHandle, verbose = FALSE,
  orgJSON = FALSE)
}
\arguments{
\item{product_ids}{Numeric. A vector of appFigures-assigned
product ids. The default will return sales for all products
in the account.}

\item{end_date}{Character string or date object. Date of last ranks
to be reported. Defaults to today. See Details.}

\item{start_date}{Character string or date object. Date of first
ranks to be reported. Defaults to the last 31 days. See Details.}

\item{group_by}{Character. Choose one or more of:
\code{"dates", "products", "store", "countries", or "regions"}}

\item{country}{Character. One or more country iso abbreviations.
Defaults to all countries.}

\item{dataset}{Character. Choose either \code{"none" or
"financial"}. Defaults to \code{"none"}. This only applies
to Apple products.}

\item{granularity}{Character. How should values be aggregated in time.
Options include \code{"daily", "weekly", "monthly", or "yearly"}.
See Details.}

\item{format}{Character. Choose between \code{"flat", "csv",
or "json"}. Defaults to \code{"flat"}. See Details}

\item{include_inapps}{Logical. If set to \code{TRUE}, any
products given in \code{product_ids} will have their
children automatically selected as well. Defaults to
\code{FALSE}.}

\item{curlHandle}{Provide an instance of the CURLHandle-class
from the RCurl package. The default will create a curl handle
specific to the function call.}

\item{verbose}{Logical. Should details of the web request
print to the console? Defaults to \code{FALSE}.}

\item{orgJSON}{Logical. Should the JSON string be returned
without being converted to R objects? Defaults to \code{FALSE}.}
}
\value{
A data frame containing the requested sales report.
}
\description{
\code{getSalesReport} provides comphrehensive access to sales
reports from the appFigures web API.
}
\details{
For \code{start_date} and \code{end_date}, if the
supplied argument can be interepreted as a date or POSIX
object, any hour, minute, or second is ignored. If a string
is supplied, it should have the format: 'yyyy-MM-dd'.

The argument \code{granularity} only matters if \code{group_by}
contains \code{"dates"}.

For the \code{format} argument, \code{"csv"} and \code{"flat"}
return similar fields. Both values will truncate some of the data
returned in the original JSON. A times (i.e. \code{group_by =
"products"}),  \code{"csv"} will contain a few more fields
than \code{"flat"}. Since different combinations of
\code{group_by} arguments will lead to different nested JSON
structures, if \code{format} is set to \code{"json"}, the
unformatted JSON string will be returned.
}
\seealso{
Official documentation:
\url{http://docs.appfigures.com/api/reference/v2/sales}.
}
