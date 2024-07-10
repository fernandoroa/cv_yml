getwd()
rm(list = ls())
# setwd(home_folder)
home_folder <- getwd()
list.files(home_folder)

# readRenviron(".Renviron")

source("R/produce_pdf.R")
source("R/render_single_Rmd.R")
source("R/dictionaries.R")

source_and_final_destination <- "../curriculumpu"
readRenviron(file.path(source_and_final_destination, ".Renviron"))
source_and_final_destination <- normalizePath(source_and_final_destination)

source_data <- file.path(source_and_final_destination, "custom")
config <- yaml::read_yaml(file.path(source_data, "/yml/shared_params.yml"))

config

# $valid_languages
# [1] "english"    "portuguese" "spanish"

# note: the "general" profile is ignored by the function, see produce_pdf.R

# $valid_profiles
# [1] "developer" "academic"  "general"
top_part_of_Rmd_files <- generate_top_part_files(config)
# [1] "txt_part_files/index_top_part.txt"
# [2] "txt_part_files/index_pt_devel_top_part.txt"
# [3] "txt_part_files/index_es_devel_top_part.txt"
# [4] "txt_part_files/index_en_acad_top_part.txt"
# [5] "txt_part_files/index_pt_acad_top_part.txt"
# [6] "txt_part_files/index_es_acad_top_part.txt"

folder_name <- "txt_part_files"
bottom_file <- file.path(folder_name, "index_bottom_Rmd_part.txt")

#
#  optional step produce Rmd into out_Rmd
#

# Rmd_file_list <- concatenate_top_bottom_files(output_folder = "out_Rmd", top_part_of_Rmd_files, bottom_file)
# Rmd_file_list

# [1] "index.Rmd"          "index_pt_devel.Rmd" "index_es_devel.Rmd"
# [4] "index_en_acad.Rmd"  "index_pt_acad.Rmd"  "index_es_acad.Rmd"

# same as above for main folder:
# if everything ok, this outputs files to getwd()

#
# produce .Rmd files in the "site" folder to be used in the _site IMPORTANT!
#

Rmd_file_list <- concatenate_top_bottom_files(output_folder = "site", top_part_of_Rmd_files, bottom_file)
Rmd_file_list

combinations <- make_combi_df(config)
combinations
#     language   profile
# 1    english developer
# 2 portuguese developer
# 3    spanish developer
# ...

name_combi <- transform_data(combinations)
name_combi
# [1] "English-Developer"    "Portuguese-Developer" "Spanish-Developer"
# [4] "English-Academic"     "Portuguese-Academic"  "Spanish-Academic"

# Rmd_file_list <- file.path("site", Rmd_file_list)
names(Rmd_file_list) <- name_combi
Rmd_file_list
#    English-Developer Portuguese-Developer    Spanish-Developer
#          "index.Rmd" "index_pt_devel.Rmd" "index_es_devel.Rmd"
#     English-Academic  Portuguese-Academic     Spanish-Academic
#  "index_en_acad.Rmd"  "index_pt_acad.Rmd"  "index_es_acad.Rmd"

year <- format(Sys.Date(), "%Y")

temp_file_string <- tempfile()
temp_dir <- gsub("file", "folder_", temp_file_string)
dir.create(temp_dir)
list.files(temp_dir)

source_folder <- "site"

fs::dir_copy(source_folder, temp_dir)

# List all files and directories in the source folder
files_to_copy <- fs::dir_ls(source_data)

destination_folder_site <- file.path(temp_dir, "site")
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

list.files(source_data)
list.files(destination_folder_site)
list.files(temp_dir)

package_json_path <- "package.json"
package_names <- get_package_names(package_json_path)

if (!check_npm_packages_installed(package_names)) {
  system("npm install")
} else {
  cat("All npm packages are already installed.\n")
}

setwd(destination_folder_site)

source("R/generate_main.scss.R")
scss_files <- list.files("styles")
generate_main.scss(scss_files)

require(sass)
sass(
  sass_file("styles/main.scss"),
  output = "css/style.css"
)

getwd()

if (!config$private) {
  node_generate_html_and_pdf_files(year, Rmd_file_list,
    location_site.yml = getwd(),
    output_folder = "simple_html_no_toc",
    js_path = file.path(home_folder, "js")
  )
  pdf_folder <- "pdf"
} else {
  node_generate_html_and_pdf_files(year, Rmd_file_list,
    location_site.yml = getwd(),
    output_folder = "simple_html_no_toc_private",
    pdf_folder = "pdf_private", private = config$private,
    js_path = file.path(home_folder, "js")
  )
  pdf_folder <- "pdf_private"
}

output_folder <- file.path(temp_dir, "site", pdf_folder)
output_folder |> list.files()

if (fs::dir_exists(file.path(source_and_final_destination, pdf_folder))) {
  fs::dir_delete(file.path(source_and_final_destination, pdf_folder))
}

fs::dir_copy(output_folder, source_and_final_destination)
setwd(home_folder)
fs::dir_delete(temp_dir)
getwd()
