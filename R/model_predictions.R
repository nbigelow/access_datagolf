#' Retrieve data from the Data Golf API
#'
#' This function allows the user to retrieve data from the Data Golf API by specifying the API endpoint to query, and any necessary query parameters.
#'
#' @param which A character string indicating which Data Golf API endpoint to query. Must be one of 'dg_rankings', 'pre_predictions', 'pre_predictions_archive', 'skill_decomp', 'skill_ratings', 'detailed_approach_skill', 'fantasy_projection_defaults', 'live_model_predictions', 'live_tournament_stats', or 'live_hole_scoring'.
#' @param key A character string indicating the API key to use for authentication.
#' @param file_format A character string indicating the format in which to return the data. Must be one of 'json' (default), 'xml', 'csv', 'yaml', or 'text'.
#' @param tour A character string indicating the golf tour for which to retrieve data. Only applicable for certain endpoints. Default value is 'pga'.
#' @param odds_format A character string indicating the format in which to return the betting odds. Only applicable for certain endpoints. Must be one of 'percent' (default) or 'decimal'.
#' @param event_id A character string indicating the ID of the golf event for which to retrieve data. Only applicable for certain endpoints.
#' @param year A character string indicating the year of the golf event for which to retrieve data. Only applicable for certain endpoints. Default value is '2023'.
#' @param display A character string indicating how to display the skill ratings. Only applicable for certain endpoints. Must be one of 'value' (default) or 'rank'.
#' @param period A character string indicating the period over which to calculate the detailed approach skill ratings. Only applicable for certain endpoints. Must be one of 'l4', 'l8', 'l12', or 'l24' (default).
#' @param site A character string indicating the daily fantasy sports site for which to retrieve data. Only applicable for certain endpoints. Default value is 'draftkings'.
#' @param slate A character string indicating the slate for which to retrieve data. Only applicable for certain endpoints. Default value is 'main'.
#' @param dead_heat A character string indicating whether to account for dead heat scenarios in the live model predictions. Only applicable for certain endpoints. Must be one of 'yes' or 'no' (default).
#' @param round A character string indicating the round for which to retrieve data. Only applicable for certain endpoints. Must be one of 'event_avg' (default), '1', '2', '3', or '4'.
#'
#' @return A data frame containing the requested data.
#'
#' @export

model_predictions <- function(which,key,file_format='json',tour='pga',odds_format='percent',
                              event_id=NULL,year='2023',display='value',period='l24',site='draftkings',
                              slate='main',dead_heat='no',round='event_avg'){
  if(which=='dg_rankings'){url <- paste('https://feeds.datagolf.com/preds/get-dg-rankings?file_format=',file_format,'&key=',key,sep = '')}
  else if(which=='pre_predictions'){url <- paste('https://feeds.datagolf.com/preds/pre-tournament?tour=',tour,'&add_position=',add_position,'&odds_format=',odds_format,'&file_format=',file_format,'&key=',key,sep='')}
  else if(which=='pre_predictions_archive'){url <- paste('https://feeds.datagolf.com/preds/pre-tournament?event_id=',event_id,'&year=',year,'&odds_format=',odds_format,'&file_format=',file_format,'&key=',key,sep='')}
  else if(which=='skill_decomp'){url <- paste('https://feeds.datagolf.com/preds/player-decompositions?tour=',tour,'&file_format=',file_format,'&key=',key,sep='')}
  else if(which=='skill_ratings'){url <- paste('https://feeds.datagolf.com/preds/skill-ratings?display=',display,'&file_format=',file_format,'&key=',key,sep='')}
  else if(which=='detailed_approach_skill'){url <- paste('https://feeds.datagolf.com/preds/approach-skill?period=',period,'&file_format=',file_format,'&key=',key,sep = '')}
  else if(which=='fantasy_projection_defaults'){url<-paste('https://feeds.datagolf.com/preds/fantasy-projection-defaults?tour=',tour,'&site=',site,'&slate=',slate,'&file_format=',file_format,'&key=',key,sep='')}
  else if(which=='live_model_predictions'){url<-paste('https://feeds.datagolf.com/preds/in-play?tour=',tour,'&dead_heat=',dead_heat,'&odds_format=',odds_format,'&file_format=',file_format,'&key=',key,sep='')}
  else if(which=='live_tournament_stats'){url<-paste('https://feeds.datagolf.com/preds/live-tournament-stats?round=',round,'&display=',display,'&file_format=',file_format,'&key=',key,sep='')}
  else if(which=='live_hole_scoring'){url<-paste('https://feeds.datagolf.com/preds/live-hole-stats?tour=',tour,'&file_format=',file_format,'&key=',key,sep='')}
  # Send a GET request to the API endpoint
  response <- httr::GET(url)

  # Convert the response content to a data frame
  data <- jsonlite::fromJSON(httr::content(response, "text"),
                             flatten = TRUE)
  return(data)
}
