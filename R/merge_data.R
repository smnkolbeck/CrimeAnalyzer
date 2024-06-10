#'Merge Data Files
#'
#'Merges All CSV or Excel files Contained in Specified Folder
#'
#' @encoding UTF-8
#'
#' @param folder The folder containing the .csv or .xlsx files
#' @param filetype The file type. Defaults to .csv. Options are ".csv" or ".xlsx".
#' @param columns Which columns from the data files to retain. Defaults to all columns. 
#'
#' @returns A tibble dataframe
#' 
#' @export
#' 
merge_files <- function(folder_path, file_type = ".csv", columns = NULL) {
require(dplyr)
require(readr)
require(purrr)

# Get list of files with the specified file type in the folder
file_list <- list.files(folder_path, pattern = file_type, full.names = TRUE)

# Read each file and bind them row-wise
data <- map_dfr(file_list, ~ {
  if (tolower(file_type) == ".csv") {
    read_csv(.x)
  } else if (tolower(file_type) == ".xlsx") {
    read_excel(.x)
  } else {
    stop("Unsupported file type. Please provide either '.csv' or '.xlsx'.")
  }
})

# Filter columns if specified
if (!is.null(columns)) {
  data <- data %>% select(columns)
}

return(data)
}