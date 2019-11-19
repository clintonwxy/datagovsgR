#' UVI(Ultra-violet Index) Summary
#'
#' This functions calls upon the UVI API from data.gov.sg
#' and returns UVI values for the given date.
#' This data provided by the API is updated hourly.
#'
#' Note that there is only information on the UVI between 0700 and 1900.
#' The returned data frame would only contain UVI values between this
#' range.
#'
#' @param date Defaults to current (SGD) time. Format: YYYY-MM-DD
#'
#' @keywords UVI
#'
#' @return A dataframe containing the UVI for the given date
#'
#' @export
#' @examples
#' uvi_summary()
#' uvi_summary(date = "2018-10-10")
#' uvi_summary(date = "2019-04-01")

uvi_summary = function(date = "") {

  # Creating and pulling URL
  URL = parse_api_date(api = "environment/uv-index",
                       input_date = date,
                       summary = TRUE)
  output = httr::GET(URL)

  # Error check
  content.output = parse_api_output(output)

  if (length(content.output$items) == 0) {
    stop("No data returned from API.")
  }

  # Extracting Data Frame
  uvi_summary = dplyr::bind_rows(content.output$item[[length(content.output$item)]]$index)
  uvi_summary = dplyr::arrange(as.data.frame(uvi_summary, stringsAsFactors = FALSE), timestamp)
  uvi_summary = uvi_summary[ , c(2, 1)]

  message("Closest first timestamp: ", uvi_summary[1, 1])

  return(uvi_summary)

}

