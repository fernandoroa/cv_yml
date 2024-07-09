getwd()
# setwd(home_folder)
rm(list = ls())

your_name <- "Fernando Roa"
site_title <- paste("CV", your_name)

home_folder <- getwd()

readRenviron(".Renviron")
source("R/modify_exclude_in_site_yml.R")

temp_file_string <- tempfile()
temp_dir <- gsub("file", "folder_", temp_file_string)
if (fs::dir_exists(temp_dir)) {
  fs::dir_delete(temp_dir)
}
dir.create(temp_dir)
list.files(temp_dir)

source_folder <- "site"
list.files(source_folder)

add_name_to_yml(file.path(source_folder, "_site.yml"), key = "navbar.title", site_title)

fs::dir_copy(source_folder, temp_dir)

destination_folder_site <- file.path(temp_dir, "site")
list.files(destination_folder_site)

source_and_final_destination <- "../curriculumpu"
source_and_final_destination <- normalizePath(source_and_final_destination)

source_data <- file.path(source_and_final_destination, "custom")

shared_params_path <- file.path(source_data, "yml/shared_params.yml")

add_name_to_yml(shared_params_path, "name", your_name)

# Copy each file/directory to the destination folder
files_and_dirs_to_copy <- fs::dir_ls(source_data, recurse = TRUE)

# Copy each file/directory to the destination folder
for (item in files_and_dirs_to_copy) {
  relative_path <- fs::path_rel(item, start = source_data)
  target_path <- fs::path(destination_folder_site, relative_path)
  if (fs::dir_exists(item)) {
    fs::dir_create(target_path)
  } else {
    fs::file_copy(item, target_path, overwrite = TRUE)
  }
}

list.files(temp_dir)
list.files(destination_folder_site)

setwd(destination_folder_site)

source("R/generate_main.scss.R")
scss_files <- list.files("styles")
generate_main.scss(scss_files)

require(sass)
sass(
  sass_file("styles/main.scss"),
  output = "css/style.css"
)

shared_params_path <- file.path(destination_folder_site, "yml", "shared_params.yml")
config <- yaml::read_yaml(shared_params_path)
config

getwd()

#                       IMPORTANT!
# if config$private is TRUE you should not upload to _site to a public repository

if (!config$private) {
  add_to_exclude("_site.yml", "secrets")
} else {
  remove_from_exclude("_site.yml", "secrets")
}

rmarkdown::render_site()

output_folder <- file.path(destination_folder_site, "_site")
output_folder |> list.files()

if (fs::dir_exists(file.path(source_and_final_destination, "_site"))) {
  fs::dir_delete(file.path(source_and_final_destination, "_site"))
}

fs::dir_copy(output_folder, source_and_final_destination)

fs::dir_delete(temp_dir)

setwd(home_folder)
getwd()
