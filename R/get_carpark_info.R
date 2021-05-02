#' Carpark Info
#'
#' This functions calls upon the datastore_search API from data.gov.sg
#' to extract HDB carpark information and return it as data.frame
#'
#' @return A dataframe containing the carpark information.
#' Extracted fields include car_park_no, address, x_coord, y_coord, car_park_type,
#' type_of_parking_system, short_term_parking, free_parking,
#' car_park_decks, gantry_height, car_park_basement
#'
#' @export
#'
#' @examples
#' get_carpark_info()
get_carpark_info <- function() {
  offset <- 0
  carpark_info <- data.frame()

  while(TRUE) {
    url <- paste0("https://data.gov.sg/api/action/datastore_search?resource_id=139a3035-e624-4f56-b63f-89ae28d4ae4c&limit=2000", "&offset=", offset)
    output <- httr::GET(url = url)

    # Error check
    content.output <- parse_api_output(output)

    if(length(content.output$result$records) != 0) {
      content_output <- lapply(1:length(content.output$result$records), function(x) {
        data.frame(content.output$result$records[[x]])
      })

      carpark_info <- dplyr::bind_rows(carpark_info, content_output)
      offset <- offset + 2000
    } else {
      break
    }
  }
  # tidy the data.frame
  carpark_info <- carpark_info[c("X_id", "car_park_no", "address", "x_coord", "y_coord", "car_park_type",
                                 "type_of_parking_system", "short_term_parking", "free_parking",
                                 "car_park_decks", "gantry_height", "car_park_basement")]
  carpark_info$x_coord <- as.numeric(carpark_info$x_coord)
  carpark_info$y_coord <- as.numeric(carpark_info$y_coord)
  carpark_info$car_park_decks <- as.numeric(carpark_info$car_park_decks)
  carpark_info$gantry_height <- as.numeric(carpark_info$gantry_height)

  return(carpark_info)
}


