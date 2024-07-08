# Function to temporarily rename _site.yml and render the RMarkdown file
render_single_Rmd <- function(
    input_file,
    output_dir, location_site.yml = getwd(),
    private = FALSE,
    private_string = "_pri_documents",
    css_file = "css/style.css") {
  site_yml <- file.path(location_site.yml, "_site.yml")
  temp_site_yml <- file.path(location_site.yml, "_site_temp.yml")
  css_file_path <- file.path(location_site.yml, css_file)

  css_backup <- NULL
  if (file.exists(css_file_path)) {
    css_backup <- readLines(css_file_path)
  }

  modify_css_file <- function(css_path) {
    css_content <- readLines(css_path)
    css_content <- gsub(
      "\\.section\\.level1 > \\.section\\.level2:nth-of-type\\(2\\) > \\* \\{",
      "\\.section\\.level1 > \\.section\\.level2:nth-of-type\\(1\\) > \\* \\{",
      css_content
    )
    writeLines(css_content, css_path)
  }

  if (file.exists(css_file_path)) {
    modify_css_file(css_file_path)
  }

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

  if (!is.null(css_backup)) {
    writeLines(css_backup, css_file_path)
  }
}
