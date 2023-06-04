## Switch names functuin 
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
  switched_names<-gsub(',','',switched_names)
  return(switched_names)
}

