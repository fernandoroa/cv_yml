getwd()
rm(list = ls())
readRenviron(".Renviron")

source("R/produce_pdf.R")
source("R/render_single_Rmd.R")
source("R/dictionaries.R")

config <- yaml::read_yaml("site/yml/shared_params.yml")
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

Rmd_file_list <- concatenate_top_bottom_files(output_folder = "out_Rmd", top_part_of_Rmd_files, bottom_file)
Rmd_file_list

# [1] "index.Rmd"          "index_pt_devel.Rmd" "index_es_devel.Rmd"
# [4] "index_en_acad.Rmd"  "index_pt_acad.Rmd"  "index_es_acad.Rmd"

# same as above for main folder:
# if everything ok, this outputs files to getwd()

#
# produce .Rmd files in the "site" folder to be used in the _site IMPORTANT!
#

concatenate_top_bottom_files(output_folder = "site", top_part_of_Rmd_files, bottom_file)

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

year <- format(Sys.Date(), "%Y")

Rmd_file_list <- file.path("site", Rmd_file_list)
names(Rmd_file_list) <- name_combi
Rmd_file_list

#    English-Developer Portuguese-Developer    Spanish-Developer
#          "index.Rmd" "index_pt_devel.Rmd" "index_es_devel.Rmd"
#     English-Academic  Portuguese-Academic     Spanish-Academic
#  "index_en_acad.Rmd"  "index_pt_acad.Rmd"  "index_es_acad.Rmd"

if (!config$private) {
  node_generate_html_and_pdf_files(year, Rmd_file_list,
    location_site.yml = "site",
    output_folder = "simple_html_no_toc"
  )
} else {
  node_generate_html_and_pdf_files(year, Rmd_file_list,
    location_site.yml = "site",
    output_folder = "simple_html_no_toc_private",
    pdf_folder = "pdf_private", private = config$private
  )
}
