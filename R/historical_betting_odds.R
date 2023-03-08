#' Historical Betting Odds
#'
#' This function retrieves historical betting odds for golf events from the Datagolf API.
#'
#' @param which A character string indicating which type of data to retrieve. Possible values are "ids", "outrights", and "matchups".
#' @param key A character string specifying the API key to use.
#' @param tour A character string indicating the golf tour to retrieve data for. Defaults to "pga".
#' @param site A character string indicating the betting site to retrieve data from. Defaults to "draftkings".
#' @param event_id An optional character string indicating the ID of the golf event to retrieve data for.
#' @param year A character string indicating the year of the golf event to retrieve data for. Defaults to "2023".
#' @param file_format A character string indicating the file format of the API response. Defaults to "json".
#' @param market A character string indicating the type of betting market to retrieve data for. Defaults to "win".
#' @param book A character string indicating the betting book to retrieve data from. Defaults to "betmgm".
#' @param odds_format A character string indicating the format of the betting odds. Defaults to "decimal".
#'
#' @return A data frame containing the historical betting odds for the specified golf events.
#'
#' # Get historical outright betting odds for a specific golf event
#' event_id <- "1234"
#' outright_odds <- historical_betting_odds("outrights", event_id = event_id)
#'
#' # Get historical matchup betting odds for a specific golf event
#' matchup_odds <- historical_betting_odds("matchups", event_id = event_id)
#'
#' @export
historical_betting_odds <- function(which, key, tour = 'pga',site = 'draftkings',event_id = NULL,
                                year = '2023',file_format = 'json',market = 'win',book = 'betmgm',odds_format = 'decimal'){
  if(which=='ids'){url <- paste('https://feeds.datagolf.com/historical-odds/event-list?tour=',tour,'&file_format=',file_format,'&key=',key,sep = '')}
  else if(which=='outrights'){url <- paste('https://feeds.datagolf.com/historical-odds/outrights?tour=',tour,'&event_id=',event_id,'&year=',year,'&market=',market,'&book=',book,'&odds_format=',odds_format,'&file_format=',file_format,'&key=',key,sep = '')}
  else if(which == 'matchups'){url <- paste('https://feeds.datagolf.com/historical-odds/matchups?tour=',tour,'&event_id=',event_id,'&year=',year,'&market=',market,'&book=',book,'&odds_format=',odds_format,'&file_format=',file_format,'&key=',key,sep = '')}
  # Send a GET request to the API endpoint
  response <- httr::GET(url)

  # Convert the response content to a data frame
  data <- jsonlite::fromJSON(httr::content(response, "text"),
                             flatten = TRUE)
  return(data)
}
