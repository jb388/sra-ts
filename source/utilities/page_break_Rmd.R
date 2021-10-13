#' @author J. Beem-Miller
#' @description Function for pagebreaks in .Rmd files
#' @details Call in .Rmd text chunck using: `r pagebreak()`

pagebreak <- function() {
  if(knitr::is_latex_output())
    return("\\newpage")
  else
    return('<div style="page-break-before: always;" />')
}