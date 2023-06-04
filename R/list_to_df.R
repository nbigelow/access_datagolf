#' Convert a list of data frames to a single data frame
#'
#' This function takes a list of data frames and combines them into a single data frame by row binding.
#' The function adds any missing columns to each data frame in the list and makes sure that the columns are in the same order.
#'
#' @param your_list A list of data frames to combine.
#' @return A single data frame obtained by row binding the data frames in the input list.
#' @export
list_to_df <- function(your_list) {
  df <- your_list
  
  # Get the column names of all data frames
  all_colnames <- unique(unlist(lapply(df, colnames)))
  
  # Add missing columns to each data frame
  my_list <- lapply(df, function(df) {
    missing_cols <- setdiff(all_colnames, colnames(df))
    df[missing_cols] <- NA
    return(df[, all_colnames])
  })
  
  # Combine the data frames
  result <- do.call(rbind, my_list)
  
  return(result)
}