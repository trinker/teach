#' Make Lecture Folders
#'
#' Make boiler plate for lectures.
#'
#' @param n_lectures The number of lectures.  Go higher.
#' @param path The path to the sessions directory.
#' @param other_dirs Other directories to add to the template.
#' @export
#' @rdname make_lectures
#' @examples
#' \dontrun{
#' make_lectures()
#' make_instructor_scripts_dir()
#' make_lesson(
#'     session_number = 1,
#'     instructor_script_control_file = ".bin/control_scripts/01_intro_to_r.R"
#' )
#' }
make_lectures <- function(n_lectures = 12, path = 'sessions', other_dirs = NULL){

    path <- gsub("\\s+", "_", path)
    if (path == Sys.getenv("R_HOME")) stop("path can not be `R_HOME`")

    if (file.exists(path)) {
        message(paste0("\"", path, "\" already exists:\nDo you want to overwrite?\n"))
        ans <- menu(c("Yes", "No"))
        if (ans == "2") {
            stop("project aborted")
        }
        else {
            unlink(path, recursive = TRUE, force = FALSE)
        }
    }
    suppressWarnings(invisible(dir.create(path, recursive = TRUE)))

    .rproj <- c(
        "Version: 1.0", "", "RestoreWorkspace: Default", "SaveWorkspace: Default",
        "AlwaysSaveHistory: Default", "", "EnableCodeIndexing: Yes",
        "UseSpacesForTab: Yes", "NumSpacesForTab: 4", "Encoding: UTF-8",
        "", "RnwWeave: knitr", "LaTeX: pdfLaTeX", "", "BuildType: Package",
        "PackageUseDevtools: Yes", "PackageInstallArgs: --no-multiarch --with-keep.source", ''
    )

    dir.create(path)
    for (i in seq_len(n_lectures)) {
        sess <- file.path(path, stringi::stri_pad_left(i, min(2, nchar(n_lectures)), '0'))
        dir.create(sess)
        for (j in c('resources', 'data', other_dirs)) dir.create(file.path(sess, j))
        cat(.rproj, file = file.path(sess, '.Rproj.'), sep = '\n')
    }

}

#' Make Lecture Folders
#'
#' Make boiler plate for lectures.
#'
#' @param session_number The lecture session number.
#' @param instructor_script_control_file The file R file with # mark up that is
#' used to make student lesson scripts as well as the \code{.(n)} code chunks.
#' @param instructor_script_path Path to where the instructor scripts should be
#' stored.
#' @param teach_path Path to where the teacher files are located for this lecture
#' session.
#' @param student_session_path A path to where student sessions are stored.
#' @export
#' @rdname make_lectures
make_lesson <- function(
    session_number,
    instructor_script_control_file,
    instructor_script_path = '.bin/instructor_scripts',
    teach_path = file.path(instructor_script_path, stringi::stri_pad_left(session_number, 2, '0')),
    student_session_path = 'sessions'
) {

    lines <- readLines(instructor_script_control_file, warn =  FALSE)

    code_chunks_loc <- grep('^# ', lines)
    code_chunks <- split(
        lines[code_chunks_loc],
        cumsum(seq_along(code_chunks_loc) %in% (which(diff(code_chunks_loc) > 1) + 1))
    )

    loc <- dir(file.path(instructor_script_path))[as.integer(dir(file.path(instructor_script_path))) == session_number]
    script_names <- paste0(
        stringi::stri_pad_left(
            seq_along(code_chunks),
            max(2, nchar(seq_along(code_chunks))),
            '0'
        ),
        '.R'
    )

    for (i in seq_along(code_chunks)) {
        cat(
            c(gsub('^#\\s', '', code_chunks[[i]])), '\n', sep ='\n',
            file = file.path(instructor_script_path, loc, 'live_coding_scripts', script_names[i])
        )
    }

    lines2 <- lines
    lines2[grep('^#(!| )', lines)] <- ''
    cat(lines2, sep = '\n', file = file.path(student_session_path, loc, basename(instructor_script_control_file)))
    cat(lines2, sep = '\n', file = file.path(instructor_script_path, loc, basename(instructor_script_control_file)))

    lines[code_chunks_loc] <- unlist(Map(function(x, i){
        c(sprintf('\n.(%s)\n%s', i, x[1]), x[-1])
    }, code_chunks, seq_along(code_chunks)))

    cat(lines, sep = '\n', file = file.path(instructor_script_path, loc, gsub('\\.R$', '_PRINTED_NOTES_.R', basename(instructor_script_control_file))))

    setup <- c(
        'library(teach)', sprintf("options(teach_path = '%s')", file.path('live_coding_scripts')),
        ''
    )
    cat(setup, sep = '\n', file = file.path(instructor_script_path, loc, 'session_set_up.R'))


}






#' Make Lecture Folders
#'
#' Make boiler plate for lectures.
#'
#' @export
#' @rdname make_lectures
make_instructor_scripts_dir <- function(n_lectures = 12, path = ".bin/instructor_scripts"){

    make_lectures(n_lectures, path, other_dirs = c('live_coding_scripts'))
}

#'01_slides', '03_assignments'

