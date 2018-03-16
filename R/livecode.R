#' Simulate Live Coding
#'
#' Use the \pkg{xfun} package to simulate live coding.
#'
#' @param n A script number within the root.
#' @param root A root folder.
#' @param script A script name.
#' @export
#' @rdname livecode
#' @examples
#' get_root()
#' options(teach_path = '.bin/scripts/01')
#' get_root()
. <- function(n = NULL, root = teach::get_root(), script = sprintf('%s.R', sprintf("%02d", n))) {
    path <- file.path(root, script)
    x <- readLines(path)
    xfun::rstudio_type(x, pause = function() runif(1, 0, 0.25), mistake = 0.005)
}

#' Simulate Live Coding
#'
#' Use the \pkg{xfun} package to simulate live coding.
#'
#' @param path A path for the root.
#' @export
#' @rdname livecode
get_root <- function(path = getOption('teach_path')){
    if (is.null(path)) path <- 'scripts'
    path
}
