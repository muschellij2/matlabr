#' Create PATHs to add to MATLAB PATHs
#'
#' @param path vector of paths to add
#'
#' @return A character vector
#' @examples 
#' add_path("~/")
#' gen_path("~/")
#' add_gen_path("~/")
#' You can also use multiple paths at once:
#' add_path(c("~/", "/opt")
#' @export
add_path = function(path) {
  path = sapply(path, function(x) {
    paste0("addpath('", x, "');")
  })
  path = unname(unlist(path))
  return(path)
}

#' @rdname add_path
#' @export
gen_path = function(path) {
  path = sapply(path, function(x) {
    paste0("genpath('", x, "');")
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