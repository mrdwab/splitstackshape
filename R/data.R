#' Example Dataset with Concatenated Cells
#' 
#' This is a sample dataset to demonstrate the different features of the
#' [concat.split()] family of functions.
#' 
#' @name concat.test
#' @aliases concat.test
#' @docType data
#' @format A `data.frame` in which many columns contain concatenated cells.
#' 
#' @details The `concat.test` dataset contains three columns with concatenated
#' data:
#' 
#' * `"Likes"`: Comma-separated integer values, with no spaces. The full value
#' range for the column would be 1 to 6. Some values are blank (`""`) and others
#' are `NA`.
#' * `"Siblings"`: Comma-separated string values, with spaces before and after
#' the delimiter. Some values end with the delimiter; others don't.
#' * `"Hates"`: Semicolon-separated integer values, with no spaces. The full
#' value range for the column would be 1 to 4. Some values are blank (`""`) and 
#' others are `NA`. If not blank or `NA`, the value ends with a trailing 
#' semicolon.
#' 
#' @keywords datasets
NULL