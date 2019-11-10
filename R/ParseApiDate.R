#' ParseApiDate
#'
#' Helper function to check if the date was input correctly
#' before returning a url to be called by the GET function.
#' Also returns an error if the API called can only provide data
#' for a specific date-time, and not date only.
#'
#' @param date Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS or YYYY-MM-DD
#' @param summary Returns an error if the user inputs a date only time when not supported by the API.
#'
#' @return A url to be called by the API.

parse_api_date = function(api, input_date = "", summary) {

  if (input_date == "") {

    # No date specified
    return(paste0("https://api.data.gov.sg/v1/", api))

  } else if (stringr::str_detect(input_date, pattern = "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}T[:digit:]{2}:[:digit:]{2}:[:digit:]{2}$") &
             summary == FALSE) {

    # Checks if the date is in the date+time format

    input_date = gsub(":", "%3A", input_date)
    return(paste0("https://api.data.gov.sg/v1/",
                  api,
                  "?date_time=",
                  input_date))

  } else if (stringr::str_detect(input_date, pattern = "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$") &
             summary == TRUE) {

    # Checks if the date is in the date format
    input_date = gsub(":", "%3A", input_date)
    return(paste0("https://api.data.gov.sg/v1/",
                  api,
                  "?date=",
                  input_date))

  } else {

    # Returns an error that the date is not in the correct format
    stop("Check that the date parameter is in the right format.")

  }
}
