#' @title Find matlab path
#'
#' @description This tries to find matlab's path using a system which
#' command, and then, if not found, looks at \code{getOption("matlab.path")}.  If not path is found, it fails.
#' @param try_defaults (logical) If \code{matlab} is not found from 
#' \code{Sys.which}, and \code{matlab.path} not found, then try some 
#' default PATHs for Linux and OS X.  
#' @param desktop Should desktop be active for MATLAB?
#' @param splash Should splash be active for MATLAB?
#' @param display Should display be active for MATLAB?
#' @param wait Should R wait for the command to finish.  Both
#' passed to \code{\link{system}} and adds the \code{-wait} flag.
#' @param single_thread Should the flag \code{-singleCompThread} 
#' be executed to limit MATLAB to a single computational thread?
#' @export
#' @return Character of command for matlab
#' @examples 
#' if (have_matlab()) {
#' get_matlab()
#' }
get_matlab = function(
  try_defaults = TRUE,
  desktop = FALSE,
  splash = FALSE,
  display = FALSE,
  wait = TRUE,
  single_thread = FALSE){
  # find.matlab <- system("which matlab", ignore.stdout=TRUE)
  mat = paste0(
    "matlab", 
    ifelse(
      .Platform$OS.type %in% "windows", 
      ".exe", 
      "")
  )
  find.matlab = as.numeric(Sys.which(mat) == "")
  myfunc = function(x, name) {
    x = as.logical(x)
    ifelse(x, "", paste0("-no", name))
  }
  desktop = myfunc(desktop, "desktop")
  splash = myfunc(splash, "splash")
  display = myfunc(display, "display")
  wait = ifelse(
      .Platform$OS.type %in% "windows", 
      ifelse(wait, "-wait", ""), 
      "")
  
  matcmd <- paste0(mat, " ", 
                   wait, " ",
                   desktop, " ", 
                   splash, " ", 
                   ifelse(single_thread, "-singleCompThread ", ""),
                   display, " -r ")
  
  if (find.matlab != 0) {
    mpath = getOption("matlab.path")
    
    ####################################
    # Trying defaults
    ####################################    
    if (is.null(mpath)) {
      if (try_defaults) {
        this.year = as.numeric(format(Sys.Date(), "%Y"))
        years = seq(this.year + 1, this.year - 5, by = -1)
        mac_ends = c(outer(years, c("a", "b"), paste0))
        def_paths = c(
          "/usr/local/bin",
          "/usr/bin",
          paste0("/Applications/MATLAB_R", mac_ends, ".app/bin"),
          paste0("C:/Program Files/MATLAB/R", mac_ends, "/bin"),
          paste0("D:/Program Files/MATLAB/R", mac_ends, "/bin")
        )
        for (ipath in def_paths) {
          def_path = file.path(ipath, mat)
          if (file.exists(def_path)) {
            warning(paste0("Setting matlab.path to ", ipath))
            options(matlab.path = ipath)
            mpath = ipath
            break;
          } # end def_path
        } # end loop
      } # end try_defaults
    } # end null mpath
    
    stopifnot(!is.null(mpath))
    stopifnot(file.exists(mpath))
    mpath = shQuote(mpath)
    matcmd <- file.path(mpath, matcmd)
  }
  return(matcmd)
}

#' @title Logical check if MATLAB is accessible
#'
#' @description Uses \code{\link{get_matlab}} to check if 
#' MATLAB's path accessible 
#' @export
#' @return Logical \code{TRUE} is MATLAB is accessible, \code{FALSE} if not
#' @examples 
#' have_matlab()
have_matlab = function(){
  x = suppressWarnings(try(get_matlab(), silent = TRUE))
  return(!inherits(x, "try-error"))
}



#' @title Run matlab script
#'
#' @description This function runs a matlab script, and 
#' returns exit statuses
#' @param fname Filename of matlab script (.m file)
#' @param verbose print diagnostic messages
#' @param ... Options passed to \code{\link{system}}
#' @inheritParams get_matlab
#' @export
#' @return Exit status of matlab code
run_matlab_script = function(
  fname, 
  verbose = TRUE, 
  desktop = FALSE,
  splash = FALSE,
  display = FALSE,
  wait = TRUE,
  single_thread = FALSE,
  ...){
  stopifnot(file.exists(fname))
  matcmd = get_matlab(  
    desktop = desktop,
    splash = splash,
    display = display,
    wait = wait,
    single_thread = single_thread)
  cmd = paste0(' "', "try, run('", fname, "'); ",
               "catch err, disp(err.message); ", 
               "exit(1); end; exit(0);", '"')  
  cmd = paste0(matcmd, cmd)
  if (verbose) {
    message("Command run is:")
    message(cmd)
  }
  x <- system(cmd, wait = wait, ...)
  return(x)
}


#' @title Runs matlab code
#'
#' @description This function takes in matlab code, where
#' the last line must end with a ;, and returns the exit
#' status
#' @param code Character vector of code. 
#' @param endlines Logical of whether the semicolon (;) should be
#' pasted to each element of the vector.
#' @param verbose Print out filename to run
#' @param add_clear_all Add \code{clear all;} to the beginning of code
#' @param paths_to_add Character vector of PATHs to add to the 
#' script using \code{\link{add_path}}
#' @param ... Options passed to \code{\link{run_matlab_script}}
#' @export
#' @return Exit status of matlab code 
#' @examples 
#' if (have_matlab()){
#'    run_matlab_code(c("disp('The version of the matlab is:')", "disp(version)"),
#'    paths_to_add = "~/")
#' }
#' \dontrun{ 
#' if (have_matlab()){ 
#'    run_matlab_code("disp(version)")
#'    run_matlab_code("disp(version)", paths_to_add = "~/")
#'    run_matlab_code(c("x = 5", "disp(['The value of x is ', num2str(x)])"))
#' }
#' }
run_matlab_code = function(
  code, endlines = TRUE, verbose = TRUE,
  add_clear_all = FALSE,
  paths_to_add = NULL,
  ...){
  # matcmd = get_matlab()
  code = c(ifelse(add_clear_all, "clear all;", ""), 
           paste0("cd('", getwd(), "');"), code)
  if (!is.null(paths_to_add)) {
    paths_to_add = add_path(paths_to_add)
    code = c(code, paths_to_add)
  }
  sep = ifelse(endlines, ";", " ")
  code = paste0(code, sep = sep, collapse = "\n")
  code = gsub(";;", ";", code)
  #   cmd <- paste(' "try \n')
  #   cmd <- paste(cmd, code)
  #   cmd <- paste(cmd, "\n catch err \n disp(err.message); \n exit(1); \n")
  #   cmd <- paste0(cmd, 'end; \n exit(0);"')
  #   cmd = gsub("\n", ";", cmd)
  #   cmd = paste0(matcmd, cmd)
  cmd = code
  fname = tempfile(fileext = ".m")
  cat(cmd, file = fname)
  if (verbose) {
    message(paste0("Script created: ", fname))
  }
  x = run_matlab_script(fname, verbose = verbose, ...)
  return(x)
}
