#' Update data in an RDS file with new data from Rolf package
#'
#' This function reads in a list object from an RDS file, and updates it with new
#' data from the Rolf package. The updated list is saved back to the same RDS file.
#'
#' @return Nothing is returned, but the updated list is saved back to the same RDS file.
#'
#' @details This function starts by reading in the list object from the RDS file
#' using `readRDS()`, and loading the `Rolf` and `dplyr` packages. It then retrieves
#' new data from the Rolf package using `Rolf::historical_raw_data()` and
#' `Rolf::historical_DFS_data()`. The function updates the list object with new
#' data if the new data is not already in the list, and saves the updated list back
#' to the same RDS file using `saveRDS()`.
#'
#' @import Rolf
#' @import dplyr
#'
#' @examples
#' # Run the function to update the data
#' update_data()
#'
#' @seealso \code{\link{readRDS}}, \code{\link{saveRDS}}
#'
#' @export

update_data <- function() {
  my_list <- readRDS("C:/Users/npbig/OneDrive/Documents/Rolf/my_list.RData")
  library(Rolf)
  library(dplyr)

  key <- 'ba458d459a1d477bc4d44df8a322'
  events <- Rolf::historical_raw_data(which = 'ids', key = key, tour = 'pga')
  events <- subset(events, events$tour == 'pga')
  events <- subset(events, events$date > my_list[[1]]$date)

  for(i in 1:nrow(events)) {
    new_event <- try(Rolf::historical_DFS_data(which = 'points', key = key, event_id = events[i, 3]))
    if (inherits(new_event, "try-error")) {
      print(paste('failed at', i))
    } else {
      u1 <- new_event$dfs_points
      u1$date <- rep(new_event$event_completed)
      my_list[[length(my_list)+1]] <- u1
    }
  }

  my_list <- unique(my_list)
  saveRDS(my_list, "C:/Users/npbig/OneDrive/Documents/Rolf/my_list.RData")
}
