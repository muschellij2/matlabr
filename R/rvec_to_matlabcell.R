#' @title Convert R vector to matlab cell
#'
#' @description This function takes in an R vector then turns it into 
#' a cell
#' @param x Vector or list of values.
#' @param sep separator to use to separate values. Defaults to ";".
#' If `x` is list, `sep` applies to the inner cell vectors also.
#' @param matname Object in matlab to be assigned
#' @param transpose Transpose the cell
#' @export
#' @return Character scalar of matlab code. If `x` is a list of 
#' all numeric vectors, returns a matlab cell array of vectors. 
#' If `x` is a list containing any character vectors, 
#' returns a matlab nested cell array.
#' @import stringr
rvec_to_matlabcell = function(x,
                              sep = ";",
                              matname = NULL, 
                              transpose = FALSE){
  if (is.numeric(x)) {
    # do nothing
  } else if (is.list(x)) {
    if (all(sapply(x, is.numeric))) {
      x = sapply(x, rvec_to_matlab, sep = sep)
    } else {
      # Might have unpredicted coercion behavior if there's any other objs in the list
      stopifnot(sapply(x, is.vector))
      x = sapply(x, rvec_to_matlabcell, sep = sep)
    }
    # trim off the eol semicolon bc these are going into a longer call
    x = str_sub(x, end = -2L)
  } else {
    x = paste0("'", x, "'")
  }
  x = paste0(x, sep)
  x = paste(x, collapse = " ")
  x = paste0("{", x, "}", ifelse(transpose, "'", ""), ";")
  if (!is.null(matname)) x = paste0(matname, " = ", x)
  x
}