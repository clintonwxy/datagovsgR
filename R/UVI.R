#' UVI(Ultra-violet Index)
#'
#' This functions calls upon the UVI API from data.gov.sg
#' and returns the UVI value for the given data-time.
#' This data provided by the API is updated hourly.
#'
#' Note that there is only information on the UVI between 0700 and 1900.
#' Entering a date-time outside of the range would return the UVI of the
#' closest date time instead.
#'
#'
#' @param date_time Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS
#'
#' @keywords UVI
#'
#' @return A dataframe containing the UVI for the given date-time
#'
#' @export
#' @examples
#' uvi()
#' uvi(date = "2018-10-10T15:00:00")
#' uvi(date = "2019-04-01T09:00:00")

uvi = function(date_time = "") {

  # Creating and pulling URL
  URL = parse_api_date(api = "environment/uv-index",
                       input_date = date_time,
                       summary = FALSE)
  output = httr::GET(URL)

  # Error check
  content.output = parse_api_output(output)

  # Extracting Data Frame
  message("Closest timestamp: ", content.output$items[[1]]$timestamp)

  uvi = as.data.frame(content.output$items[[1]]$index[[1]])
  uvi = uvi[ , c(2, 1)]

  return(uvi)

}

