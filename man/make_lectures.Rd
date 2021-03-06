% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_lectures.R
\name{make_lectures}
\alias{make_lectures}
\alias{make_lesson}
\alias{make_instructor_scripts_dir}
\title{Make Lecture Folders}
\usage{
make_lectures(n_lectures = 12, path = "sessions", other_dirs = NULL)

make_lesson(session_number, instructor_script_control_file,
  instructor_script_path = ".bin/instructor_scripts",
  teach_path = file.path(instructor_script_path,
  stringi::stri_pad_left(session_number, 2, "0")),
  student_session_path = "sessions")

make_instructor_scripts_dir(n_lectures = 12,
  path = ".bin/instructor_scripts")
}
\arguments{
\item{n_lectures}{The number of lectures.  Go higher.}

\item{path}{The path to the sessions directory.}

\item{other_dirs}{Other directories to add to the template.}

\item{session_number}{The lecture session number.}

\item{instructor_script_control_file}{The file R file with # mark up that is
used to make student lesson scripts as well as the \code{.(n)} code chunks.}

\item{instructor_script_path}{Path to where the instructor scripts should be
stored.}

\item{teach_path}{Path to where the teacher files are located for this lecture
session.}

\item{student_session_path}{A path to where student sessions are stored.}
}
\description{
Make boiler plate for lectures.

Make boiler plate for lectures.

Make boiler plate for lectures.
}
\examples{
\dontrun{
make_lectures()
make_instructor_scripts_dir()
make_lesson(
    session_number = 1,
    instructor_script_control_file = ".bin/control_scripts/01_intro_to_r.R"
)
}
}
