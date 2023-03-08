#' Historical Raw Data
#'
#' This function retrieves historical raw data from the DataGolf API by sending GET requests to specific API endpoints. It can retrieve a list of event IDs or scoring data for a specific tournament event.
#'
#' @param which A character string indicating which type of data to retrieve. Possible values are "ids" to retrieve a list of event IDs or "scoring" to retrieve scoring data for a specific tournament event. Default is "ids".
#' @param key A character string containing the API key for authentication purposes.
#' @param tour A character string indicating the tour to retrieve data from. Default is "pga".
#' @param site A character string indicating the data provider site. Default is "draftkings".
#' @param event_id A character string indicating the ID of the tournament event to retrieve data for. Default is "403".
#' @param year A character string indicating the year of the tournament event to retrieve data for. Default is "2023".
#' @param file_format A character string indicating the file format to retrieve the data in. Default is "json".
#'
#' @return A data frame containing the retrieved historical raw data.
#'
#' @examples
#' \dontrun{
#' historical_raw_data(which = "ids", key = "your_api_key")
#' historical_raw_data(which = "scoring", key = "your_api_key", tour = "lpga", site = "pga", event_id = "1234", year = "2022")
#'}
#' @export

historical_raw_data <- function(which, key, tour = 'pga',site = 'draftkings',event_id = NULL,
                                year = '2023',file_format = 'json'){
  if(which=='ids'){url <- paste('https://feeds.datagolf.com/historical-raw-data/event-list?file_format=',file_format,'&key=',key,sep = '')}
  else if(which=='scoring'){url <- paste('feeds.datagolf.com/historical-raw-data/rounds?tour=',tour,'&site=',site,'&event_id=',event_id,'&year=',year,'&file_format=',file_format,'&key=',key,sep = '')}

  # Send a GET request to the API endpoint
  response <- httr::GET(url)

  # Convert the response content to a data frame
  data <- jsonlite::fromJSON(httr::content(response, "text"),
                             flatten = TRUE)
  return(data)
}
