#' Create PATHs to add to MATLAB PATHs
#'
#' @param path path to add
#'
#' @return A character vector
#' @examples 
#' add_path("~/")
#' gen_path("~/")
#' gen_path("~/")
#' @export
add_path = function(path) {
  path = sapply(path, function(x) {
    paste0("addpath('", path, "');")
  })
  path = unname(unlist(path))
  return(path)
}

#' @rdname add_path
#' @export
gen_path = function(path) {
  path = sapply(path, function(x) {
    paste0("genpath('", path, "');")
  })
  path = unname(unlist(path))
  return(path)
}

#' @rdname add_path
#' @export
add_gen_path = function(path) {
  path = gen_path(path)
  path = add_path(path)
  path = unname(path)
  return(path)
}