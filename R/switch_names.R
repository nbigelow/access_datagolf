#' Switch names
#'
#' This function takes a vector of names and switches the order of the first and last names.
#' If a name consists of more than two parts (e.g., middle names or initials), the function
#' will handle those cases as well.
#'
#' @param names A character vector of names to be switched.
#'
#' @return A character vector of names with the first and last names switched.
#'
#' @examples
#' switch_names(c("John Doe", "Jane A. Smith", "Robert James Williams"))
#'
#' @export
switch_names <- function(names) {
  # Split names into separate words
  name_parts <- strsplit(names, " ")

  # Reorder name parts as needed
  switched_names <- sapply(name_parts, function(x) {
    if (length(x) == 2) {
      paste(x[2], x[1], sep = ", ")
    } else {
      # Handle cases with middle names or initials
      paste(x[length(x)], paste(x[-length(x)], collapse = " "), sep = ", ")
    }
  })

  # Remove commas from the switched names
  switched_names <- gsub(",", "", switched_names)

  return(switched_names)
}
