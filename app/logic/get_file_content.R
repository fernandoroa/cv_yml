#' @export
get_file_content <- function(file_path) {
  if (file.exists(file_path)) {
    return(paste0(readLines(file_path, warn = FALSE), collapse = "\n"))
  } else {
    return(".yml not loaded")
  }
}
