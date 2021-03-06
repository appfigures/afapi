% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getEvents.R
\name{getEvents}
\alias{getEvents}
\title{Retrieve all events.}
\usage{
getEvents(curlHandle, verbose = FALSE, orgJSON = FALSE)
}
\arguments{
\item{curlHandle}{Provide an instance of the CURLHandle-class
from the RCurl package. The default will create a curl handle
specific to the function call.}

\item{verbose}{Logical. Should details of the web request
print to the console? Defaults to \code{FALSE}.}

\item{orgJSON}{Logical. Should the JSON string be returned
without being converted to R objects? Defaults to \code{FALSE}.}
}
\value{
A dataframe containing all events.
}
\description{
\code{getEvents} provides access to all listed events
from the appFigures web API
}
\seealso{
Official documentation: 
\url{http://docs.appfigures.com/api/reference/v2/events}.
}
