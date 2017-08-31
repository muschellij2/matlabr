#' @title Convert R vector to matlab cell
#'
#' @description This function takes in an R vector then turns it into 
#' a cell
#' @param x Character vector of values
#' @param sep separator to use to separate values. Defaults to ";"
#' argument 
#' @param matname Object in matlab to be assigned
#' @param transpose Transpose the cell
#' @export
#' @return Character scalar of matlab code
rvec_to_matlabcell = function(x, 
                              sep = ";",
                              matname = NULL, 
                              transpose = FALSE){
  x = paste0("'", x, "'", sep)
  x = paste(x, collapse = " ")
  x = paste0("{", x, "}", ifelse(transpose, "'", ""), sep)
  if (!is.null(matname)) x = paste0(matname, " = ", x)
  x
}