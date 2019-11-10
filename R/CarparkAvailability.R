#' Carpark Availability
#'
#' This functions calls upon the carpark availability API from data.gov.sg
#' and processes the returning page.
#'
#' @param date Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS
#'
#' @keywords carpark
#'
#' @return A dataframe containing the carpark id, type, last update, total lots, and current lots.
#'
#' @export
#' @examples
#' dgsg_carpark()
#' dgsg_carpark(date = "2019-06-05T10:10:10")
#' dgsg_carpark(date = "2018-07-01T19:32:56")

carpark_availability = function(date = "") {

  # Creating and pulling URL
  URL = parse_api_date(api = "transport/carpark-availability",
                       input_date = date,
                       summary = FALSE)
  output = GET(URL)

  # Error check
  content.output = parse_api_output(output)

  # Extracting Data Frame
  message("Closest timestamp: ", content.output$items[[1]]$timestamp)

  carpark_availability = data.frame(id = rep(1, length(content.output$items[[1]]$carpark_data)),
                                   type = rep(1, length(content.output$items[[1]]$carpark_data)),
                                   last_update = rep(NA, length(content.output$items[[1]]$carpark_data)),
                                   total_lots = rep(NA, length(content.output$items[[1]]$carpark_data)),
                                   current_lots = rep(NA, length(content.output$items[[1]]$carpark_data)))

  for (i in 1:length(content.output$items[[1]]$carpark_data)) {

    carpark_availability[i, 1] = content.output$items[[1]]$carpark_data[i][[1]]$carpark_number
    carpark_availability[i, 2] = content.output$items[[1]]$carpark_data[i][[1]]$carpark_info[[1]]$lot_type
    carpark_availability[i, 3] = content.output$items[[1]]$carpark_data[i][[1]]$update_datetime
    carpark_availability[i, 4] = as.integer(content.output$items[[1]]$carpark_data[i][[1]]$carpark_info[[1]]$total_lots)
    carpark_availability[i, 5] = as.integer(content.output$items[[1]]$carpark_data[i][[1]]$carpark_info[[1]]$lots_available)

  }

  return(carpark_availability)

}

