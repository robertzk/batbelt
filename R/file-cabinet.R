#' Creates a folder, with overwrite protection.
#' @param dir character. The name of the directory to create.
#' @param can_overwrite logical. If true, will replace an existing directory.
#' @export
create_folder <- function(dir, can_overwrite = FALSE) {
  if (!file.exists(dir) || isTRUE(can_overwrite)) {
    cat('Creating', dir, 'base directory...\n')
    dir.create(dir)
  } else {
    cat('/', dir, 'already exists. Skipping...\n')
  }
}

#' Creates a file from a value, with overwrite protection.
#' @param name character. The name of the file to create.
#' @param value ANY. The value to put in the file.
#' @param can_overwrite logical. If true, will replace an existing directory.
#' @export
create_file <- function(name, value, can_overwrite = FALSE) {
  if (!file.exists(name) || isTRUE(can_overwrite)) {
    cat('Creating', name, '\n')
    sink(name)
    dput(value)
    sink()
  } else {
    cat('/', dir, 'already exists. Skipping...\n')
  }
}
