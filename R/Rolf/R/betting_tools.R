#' Betting Tools
#'
#' This function retrieves betting data from DataGolf's API using GET requests.
#'
#' @param which Type of betting data to retrieve. Can be "outright", "matchup", or "allpairings".
#' @param key API key for authentication.
#' @param tour Tour for which the data is requested. Defaults to "pga".
#' @param market Market for which the data is requested. Defaults to "win".
#' @param odds_format Format for odds. Defaults to "decimal".
#' @param file_format Format for the response file. Defaults to "json".
#' @return Returns a data frame containing the requested betting data.
#'
#' @export

betting_tools <- function(which, key, tour = 'pga',market = 'win',odds_format = 'decimal',file_format = 'json'){
  if(which=='outright'){url <- paste('https://feeds.datagolf.com/betting-tools/outrights?tour=',tour,'&market=',market,'&odds_format=',odds_format,'&file_format=',file_format,'&key=',key,sep = '')}
  else if(which=='matchup'){url <- paste('https://feeds.datagolf.com/betting-tools/matchups?tour=',tour,'&market=',market,'&odds_format=',odds_format,'&file_format=',file_format,'&key=',key,sep = '')}
  else if(which=='allpairings'){url <- paste('https://feeds.datagolf.com/betting-tools/matchups-all-pairings?tour=',tour,'&odds_format=',odds_format,'&file_format=',file_format,'&key=',key,sep = '')}
  # Send a GET request to the API endpoint
  response <- httr::GET(url)

  # Convert the response content to a data frame
  data <- jsonlite::fromJSON(httr::content(response, "text"),
                             flatten = TRUE)
  return(data)
}
