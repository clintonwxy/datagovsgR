#' Weather Forecast
#'
#' This functions calls upon the PSI API from data.gov.sg
#' and returns a data frame of the different measures of the PSI across 5
#' different areas in Singapores and the overall measure for the given
#' data-time. This data provided by the API is updated hourly.
#'
#' Note that this function is different from the `PSI_summary` function,
#' which returns the PSI measures for a given day.
#'
#' @param date Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS
#'
#' @keywords PSI
#'
#' @return A dataframe containing various PSI measures across 5 corners
#' of Singapore
#'
#' @export
#' @examples
#' psi()
#' psi(date = "2019-11-08T17:30:00")
#' psi(date = "2018-01-04T09:16:17")

weather_forecast = function(date_time = "", forecast = "2-hour") {

  # Creating and pulling URL
  if (!(forecast %in% c("2-hour", "24-hour", "4-day"))) {
    stop("Forecasts only availible for 2-hour, 24-hour and 4 days.")
  }

  URL = parse_api_date(api = paste0("environment/", forecast, "-weather-forecast"),
                       input_date = date_time,
                       summary = FALSE)
  output = httr::GET(URL)

  # Error check
  content.output = parse_api_output(output)

  # Extracting Data Frame

  if (forecast == "2-hour") {

    message("Closest timestamp: ", content.output$items[[1]]$timestamp)
    message("Forecast valid to: ", content.output$items[[1]]$valid_period$end)

    weather_forecast = dplyr::bind_rows(content.output$items[[1]]$forecasts) %>%
      as.data.frame(stringsAsFactors = FALSE)

    return(weather_forecast)

  } else if (forecast == "24-hour") {

    message("Closest timestamp: ", content.output$items[[1]]$valid_period$start)
    message("Forecast valid to: ", content.output$items[[1]]$valid_period$end)

    weather_forecast = data.frame(general_forecast = content.output$items[[1]]$general$forecast,
                                  relative_humidity_low = content.output$items[[1]]$general$relative_humidity$low,
                                  relative_humidity_high = content.output$items[[1]]$general$relative_humidity$high,
                                  temperature_low = content.output$items[[1]]$general$temperature$low,
                                  temperature_high = content.output$items[[1]]$general$temperature$high,
                                  wind_direction = content.output$items[[1]]$general$wind$direction,
                                  wind_speed_low = content.output$items[[1]]$general$wind$speed$low,
                                  wind_speed_high = content.output$items[[1]]$general$wind$speed$high,
                                  stringsAsFactors = FALSE)

    return(weather_forecast)

  } else {

    message("Closest timestamp: ", content.output$items[[1]]$timestamp)

    weather_forecast = lapply(1:4, function(x) {
      data.frame(date = content.output$items[[1]]$forecasts[[x]]$date,
                 general_forecast = content.output$items[[1]]$forecasts[[x]]$forecast,
                 relative_humidity_low = content.output$items[[1]]$forecasts[[x]]$relative_humidity$low,
                 relative_humidity_high = content.output$items[[1]]$forecasts[[x]]$relative_humidity$high,
                 temperature_low = content.output$items[[1]]$forecasts[[x]]$temperature$low,
                 temperature_high = content.output$items[[1]]$forecasts[[x]]$temperature$high,
                 wind_direction = content.output$items[[1]]$forecasts[[x]]$wind$direction,
                 wind_speed_low = content.output$items[[1]]$forecasts[[x]]$wind$speed$low,
                 wind_speed_high = content.output$items[[1]]$forecasts[[x]]$wind$speed$high,
                 stringsAsFactors = FALSE)
    })

    weather_forecast = dplyr::bind_rows(weather_forecast)

    return(weather_forecast)

  }
}

