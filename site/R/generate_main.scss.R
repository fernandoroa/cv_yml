generate_main.scss <- function(files, folder = "styles") {
  scss_files <- grep("^_", files, value = TRUE)
  scss_files <- sub("^_", "", scss_files)

  import_statements <- paste0("@import '", scss_files, "';")

  writeLines(import_statements, file.path(folder, "main.scss"))
}
