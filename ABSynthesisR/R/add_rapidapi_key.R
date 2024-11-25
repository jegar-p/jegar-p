#' Add a RapidAPI key to your .Renviron
#'
#' @description add_rapidapi_key is a helper function that adds your RapidAPI key to your R environment. You will need to provide an API key which can be downloaded from rapidapi.com/hub .
#' @param key The number of visitors that should be simulated
#' @param renviron_path Whether the study should include return visitors
#' @return Updates the .Renviron with the key

#' @export
add_rapidapi_key <- function(key, renviron_path = "~/.Renviron") {
  # Ensure the .Renviron file exists, create it if it doesn't
  if (!file.exists(renviron_path) & !file.exists("~/.Rprofile")) {
    file.create(renviron_path)
  }

  # Read the current content of .Renviron
  renviron_content <- readLines(renviron_path)

  # Construct the new variable line
  new_var <- paste("rapidapi_key=", key, sep = "")

  # Check if the variable already exists
  existing_var_index <- grep("^rapidapi_key=", renviron_content)

  if (length(existing_var_index) > 0) {
    # Replace the existing line if the variable already exists
    renviron_content[existing_var_index] <- new_var
  } else {
    # Append the new variable if it doesn't exist
    renviron_content <- c(renviron_content, new_var)
  }

  # Write the updated content back to .Renviron
  writeLines(renviron_content, renviron_path)

  message("Environment variable added/updated in .Renviron")
}



