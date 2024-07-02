# Function to temporarily rename _site.yml and render the RMarkdown file
render_single_Rmd <- function(
    input_file, output_dir, location_site.yml = getwd(),
    private = FALSE,
    private_string = "_pri_documents") {
  site_yml <- file.path(location_site.yml, "_site.yml")
  temp_site_yml <- file.path(location_site.yml, "_site_temp.yml")

  if (file.exists(site_yml)) {
    file.rename(site_yml, temp_site_yml)
  }

  if (private) {
    base_name <- tools::file_path_sans_ext(basename(input_file))
    output_file <- paste0(base_name, private_string, ".html")
    rmarkdown::render(input_file, output_dir = output_dir, output_file = output_file)
  } else {
    rmarkdown::render(input_file, output_dir = output_dir)
  }

  if (file.exists(temp_site_yml)) {
    file.rename(temp_site_yml, site_yml)
  }
}
