#' ParseApiOutput
#'
#' Helper function to extract the content returned from the API.
#' Returns the status code otherwise.
#'
#' @return The extracted content if not error has occured. Otherwise, the error message is returned.

parse_api_output = function(inputcontent) {

  if (!http_error(inputcontent)) {

    return(content(inputcontent))

  } else if (status_code(inputcontent) >= 400 &
             status_code(inputcontent) < 500) {

    stop("Client error has occured. Check your code.")

  } else if (status_code(inputcontent) >= 500 &
             status_code(inputcontent) < 600) {

    stop("Server Error has occured.")

  }
}
