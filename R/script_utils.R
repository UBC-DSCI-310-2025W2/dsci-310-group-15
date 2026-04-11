#' Locate the project root for command-line scripts
#'
#' Searches upward from likely starting directories until it finds a directory
#' containing both `R/` and `scripts/`.
#'
#' @param start Optional directory to use as the first search location.
#'
#' @return Absolute path to the project root.
find_project_root <- function(start = NULL) {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_dir <- character(0)

  if (length(script_arg) == 1L) {
    script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
    script_dir <- dirname(script_path)
  }

  candidates <- unique(stats::na.omit(c(
    start,
    script_dir,
    getwd(),
    file.path(getwd(), ".."),
    file.path(script_dir, "..")
  )))

  for (candidate in candidates) {
    current <- normalizePath(candidate, winslash = "/", mustWork = FALSE)

    repeat {
      if (
        dir.exists(file.path(current, "R")) &&
          dir.exists(file.path(current, "scripts"))
      ) {
        return(normalizePath(current, winslash = "/", mustWork = TRUE))
      }

      parent <- normalizePath(file.path(current, ".."), winslash = "/", mustWork = FALSE)
      if (identical(parent, current)) {
        break
      }
      current <- parent
    }
  }

  stop("Could not locate project root containing `R/` and `scripts/`.", call. = FALSE)
}

#' Source a file relative to the project root
#'
#' @param project_root Absolute or relative path to the project root.
#' @param ... Path components under `project_root`.
#'
#' @return Invisibly returns the value from [source()].
source_project_file <- function(project_root, ...) {
  source_path <- file.path(project_root, ...)

  if (!file.exists(source_path)) {
    stop(sprintf("Required source file not found: %s", source_path), call. = FALSE)
  }

  source(source_path)
}

#' Load required packages with a clear error for missing dependencies
#'
#' @param packages Character vector of package names to load.
#'
#' @return Invisibly returns loaded package names.
load_required_packages <- function(packages) {
  if (!is.character(packages) || length(packages) == 0L) {
    stop("`packages` must be a non-empty character vector.", call. = FALSE)
  }

  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0L) {
    stop(
      paste(
        "Install missing packages before running:",
        paste(missing_packages, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(lapply(
    packages,
    function(package) suppressPackageStartupMessages(library(package, character.only = TRUE))
  ))
}
