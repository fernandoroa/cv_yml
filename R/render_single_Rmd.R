# Function to temporarily rename _site.yml and render the RMarkdown file
render_single_Rmd <- function(input_file, output_dir, location_site.yml = getwd()) {
  site_yml <- file.path(location_site.yml, "_site.yml")
  temp_site_yml <- file.path(location_site.yml, "_site_temp.yml")

  if (file.exists(site_yml)) {
    file.rename(site_yml, temp_site_yml)
  }

  #  rmarkdown::render(input_file, output_dir = output_dir, envir = new.env())
  rmarkdown::render(input_file, output_dir = output_dir)

  if (file.exists(temp_site_yml)) {
    file.rename(temp_site_yml, site_yml)
  }
}
