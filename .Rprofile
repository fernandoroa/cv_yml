if (file.exists("renv")) {
  source("renv/activate.R")
} else {
  # The `renv` directory is automatically skipped when deploying with rsconnect.
  message("No 'renv' directory found; renv won't be activated.")
}

# Allow absolute module imports (relative to the app root).
options(box.path = getwd())

Sys.info_sysname <- Sys.info()["sysname"]
sep <- ":"
if (Sys.info_sysname == "Windows") {
  pandoc_path <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
  sep <- ";"
} else if (Sys.info_sysname == "Darwin") {
  pandoc_path <- "enter osx pandoc path"
} else {
  pandoc_path <- "/usr/bin"
  # "/usr/lib/rstudio/bin/quarto/bin/tools"
}

if (dir.exists(pandoc_path)){
  Sys.setenv("PATH" = paste0(Sys.getenv("PATH"), sep,
    pandoc_path)
  )
}

if (system.file(package = "rmarkdown") != "") {
  if (!rmarkdown::pandoc_available()) {
    message("configure pandoc in .Rprofile. Are you on VSCode?")
  }
} else {
  message("install rmarkdown")
}
