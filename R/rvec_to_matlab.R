#' @title Convert R vector to matlab cell mat
#'
#' @description This function takes in an R vector then turns it into 
#' a cell list
#' @param x Character vector of values
#' @param matname Object in matlab to be assigned
#' @export
#' @return Character scalar of matlab code
rvec_to_matlabclist = function(x, matname = NULL){
  x = paste0("{'", x, "'};")
  x = paste(x, collapse =  " ")
  x = paste0('[', x, '];')
  if (!is.null(matname)) x = paste0(matname, " = ", x)
  x
}



#' @title Convert R vector to matlab cell mat
#'
#' @description This function takes in an R numeric and returns a
#' status
#' @param x Numeric vector of values
#' @param row Create row vector instead of column vector
#' @param sep separator to use to separate cells.  Will override row
#' argument
#' @param matname Object in matlab to be assigned
#' @export
#' @return Character scalar of matlab code
#' @import stringr
rvec_to_matlab = function(x, row = FALSE,
                          sep = NULL,
                          matname = NULL){
  if (is.null(sep)) {
    sep = ifelse(row, ",", ";")
  }
  x = paste0(x, sep)
  x = paste(x, collapse = " ")
  x = str_trim(x)
  x = gsub(paste0(sep, "$"), "", x)
  x = paste0("[", x, "];")
  if (!is.null(matname)) x = paste0(matname, " = ", x)
  x
}
