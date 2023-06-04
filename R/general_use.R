#' General Use
#'
#' This function retrieves data from the Datagolf API based on the specified input parameters.
#'
#' @param which a character string indicating the type of data to retrieve. Possible options are "player_list", "schedules", and "updates".
#' @param key a character string representing the API key required to access the Datagolf API.
#' @param tour a character string indicating the golf tour for which to retrieve data. This parameter is only used when which is set to "schedules" or "updates". Default is "pga".
#' @param file_format a character string indicating the format in which to retrieve data. This parameter is only used when which is set to "player_list", "schedules", or "updates". Default is "json".
#'
#' @return A data frame containing the requested data.
#'
#' @export

general_use <- function(which, key, tour = 'pga',file_format = 'json'){
  if(which=='player_list'){url <- paste('https://feeds.datagolf.com/get-player-list?file_format=',file_format,'&key=',key,sep='')}
  else if(which=='schedules'){url <- paste('https://feeds.datagolf.com/get-schedule?tour=',tour,'&file_format=',file_format,'&key=',key,sep='')}
  else if(which=='updates'){url <- paste('https://feeds.datagolf.com/field-updates?tour=',tour,'&file_format=',file_format,'&key=',key,sep='')}
  # Send a GET request to the API endpoint
  response <- httr::GET(url)
  # Convert the response content to a data frame
  data <- jsonlite::fromJSON(httr::content(response, "text"),
                             flatten = TRUE)
  return(data)
}
