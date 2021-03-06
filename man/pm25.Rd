% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PM2.5.R
\name{pm25}
\alias{pm25}
\title{PM2.5}
\usage{
pm25(date_time = "")
}
\arguments{
\item{date_time}{Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS}
}
\value{
A dataframe containing various PM2.5 measures across 5 corners
of Singapore
}
\description{
This functions calls upon the PM2.5 API from data.gov.sg
and returns a data frame of the different measures of PM2.5 across 5
different areas in Singapore. This data provided by the API is updated
hourly from NEA.
}
\details{
Note that this function is different from the `pm25_summary` function,
which returns the PM2.5 measures for a given day.
}
\examples{
pm25()
pm25(date_time = "2019-11-08T17:30:00")
pm25(date_time = "2018-01-04T09:16:17")
}
\keyword{PM2.5}
