% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PM2.5Summary.R
\name{pm25_summary}
\alias{pm25_summary}
\title{PM2.5 Summary}
\usage{
pm25_summary(date = "")
}
\arguments{
\item{date}{Defaults to current (SGD) time. Format: YYYY-MM-DD}
}
\value{
A dataframe containing various PM2.5 measures across 5 corners
of Singapore and time of day. Dependent on the data availible, not all
results range from 0000 to 2300.
}
\description{
This functions calls upon the PM2.5 API from data.gov.sg
and returns a data frame of the different measures of the PM2.5 across 5
different areas in Singapore for each hour. This data provided by the
API is updated hourly from NEA.
}
\details{
Note that this function is different from the `pm25` function,
which returns the PM2.5 measures for a given date and time.
}
\examples{
pm25_summary()
pm25_summary(date = "2019-11-08")
pm25_summary(date = "2018-01-04")
}
\keyword{PM2.5}
