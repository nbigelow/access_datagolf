#' Historical DFS Data
#'
#' This function returns historical DFS data from the Datagolf API as a dataframe.
#'
#' @param which A character string indicating the which of data to retrieve. Possible values are 'ids' and 'points'.
#' @param key A character string representing the user's API key for Datagolf.
#' @param tour A character string indicating the tour to retrieve data for (e.g. 'pga', 'euro').
#' @param site A character string indicating the site to retrieve data for (e.g. 'draftkings', 'fanduel').
#' @param event_id A character string representing the ID of the event to retrieve data for.
#' @param year A character string representing the year of the event to retrieve data for.
#' @param file_format A character string indicating the file format of the response (e.g. 'json', 'csv').
#'
#' @return A data frame containing the requested historical DFS data.
#' @export
#'
#' @examples
#' \dontrun{
#' historical_DFS_data(which = "ids", key = "your_api_key_here")
#' historical_DFS_data(which = "points", key = "your_api_key_here", tour = "euro", site = "draftkings", event_id = "405", year = "2022", file_format = "csv")
#' }
#'
historical_DFS_data <- function(which, key, tour = 'pga',site = 'draftkings',event_id = NULL,
                         year = '2023',file_format = 'json'){
                          if(which=='ids'){url <- paste('https://feeds.datagolf.com/historical-dfs-data/event-list?file_format=',file_format,'&key=',key,sep = '')}
                          else if(which=='points'){url <- paste('https://feeds.datagolf.com/historical-dfs-data/points?tour=',tour,'&site=',site,'&event_id=',event_id,'&year=',year,'&file_format=',file_format,'&key=',key,sep = '')}

                          # Send a GET request to the API endpoint
                          response <- httr::GET(url)

                          # Convert the response content to a data frame
                          data <- jsonlite::fromJSON(httr::content(response, "text"),
                                                     flatten = TRUE)
                          return(data)
                        }
