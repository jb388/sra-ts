#' @author J. Beem-Miller
#' @description Reads Jena elemental analysis reports (.xls) files
#' @details Function first checks that the target file matches the template, then strips the headers out of the file and returns a data frame with the data values
#' @param template_file Filename, character
#' @return list of data frames
#' @import readxl

library(readxl)

read_jena_elm_results <- function(jena_elm_dir, template_file) {
  if(missing(template_file)) {
    template_file <- "../data/raw/elm_jena_template_2020-10-22/elm_jena_template.xls"
  }
  
  # start input at column header row (skip 1st row)
  template <- data.frame(read_excel(template_file, sheet = "Pivot", skip = 1))
  
  # get pathnames for .xlsx files in jena_ams_dir
  data_files <- list.files(jena_elm_dir, pattern = "\\.xls", full.names = TRUE)
  # remove open .xlsx files
  data_files <- grep(data_files, pattern='\\~\\$', inv=T, value=T)
  
  # check that row 27 column names match the template
  for(i in seq_along(data_files)) {
    for(j in seq_along(names(read_excel(data_files[i], sheet = "Pivot", skip = 1)))) {
      if(names(read_excel(data_files[i], sheet = "Pivot", skip = 1))[j] != names(template)[j])
        cat("Row 2 of ", data_files[i], " does not contain proper column names")
    }
  }
  
  # read in files
  data_ls <- lapply(seq_along(data_files), function(i) {
    df <- data.frame(read_excel(data_files[i], sheet = "Pivot", skip = 1))
    df <- df[c(1:nrow(df)-1), ] # remove summary row at bottom
    return(df)
  })
  
  names(data_ls) <- grep(list.files(jena_elm_dir, pattern = "\\.xls"), pattern='\\~\\$', inv=T, value=T)
  
  return(data_ls)
}
