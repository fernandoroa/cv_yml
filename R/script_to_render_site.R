getwd()
readRenviron(".Renviron")
if (Sys.info()["sysname"] == "Linux") system("rm -rf site/_site")
source("R/modify_exclude_in_site_yml.R")

setwd("site")

config <- yaml::read_yaml("yml/shared_params.yml")
# if config$private is TRUE you should not upload to _site to a public repository

if (!config$private) {
  add_to_exclude("_site.yml", "secret_figures")
} else {
  remove_from_exclude("_site.yml", "secret_figures")
}

{
  rm(list = ls())
  rmarkdown::render_site()
}
setwd("..")
