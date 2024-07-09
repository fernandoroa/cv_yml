extract_after_second_vowel <- function(input_string) {
  sub("^(([^aeiou]*[aeiou]){2}[[:alnum:]]{1}).*", "\\1", input_string, ignore.case = TRUE)
}

make_combi_df <- function(config) {
  valid_languages <- config$valid_languages
  valid_profiles <- config$valid_profiles
  valid_profiles <- valid_profiles[valid_profiles != "general"]
  combinations <- expand.grid(
    language = valid_languages,
    profile = valid_profiles,
    stringsAsFactors = FALSE
  )
}

generate_top_part_files <- function(config, folder_name = "txt_part_files") {
  combinations <- make_combi_df(config)


  dir.create(folder_name, showWarnings = FALSE)
  file_list <- character(0)
  for (i in 1:nrow(combinations)) {
    language <- combinations$language[i]
    language_2_char <- languages_2codes[combinations$language[i]]

    profile <- combinations$profile[i]
    file_name <- paste0(
      folder_name, "/index_", language_2_char, "_",
      extract_after_second_vowel(profile),
      "_top_part.txt"
    )
    if (i == 1) {
      file_name <- file.path(folder_name, "index_top_part.txt")
    }

    file_content <- paste0(
      "---\n",
      "title: Curriculum Vitae `r format(Sys.Date(), \"%Y\")`\n",
      "params:\n",
      "  language: ", language, "\n",
      "  profile: ", profile, "\n"
    )
    print(file_name)
    file_list <- c(file_list, file_name)
    writeLines(file_content, file_name)
  }
  return(file_list)
}

concatenate_top_bottom_files <- function(output_folder = "out_Rmd", top_part_of_Rmd_files, bottom_file) {
  if (!file.exists(bottom_file)) {
    stop("Bottom file (", bottom_file, ") does not exist.")
  }

  bottom_content <- readLines(bottom_file)

  construct_output_filename <- function(top_file) {
    sub("_top_part\\.txt$", ".Rmd", top_file)
  }
  file_list <- character(0)
  for (top_file in top_part_of_Rmd_files) {
    top_content <- readLines(top_file)

    complete_content <- c(top_content, bottom_content)

    output_file <- construct_output_filename(top_file)

    if (output_folder == "") {
      output_folder <- getwd()
    }
    writeLines(complete_content, file.path(output_folder, basename(output_file)))

    cat("Created", basename(output_file), "\n")
    file_list <- c(file_list, basename(output_file))
  }
  return(file_list)
}

transform_data <- function(df) {
  transformed <- character(nrow(df))

  for (i in 1:nrow(df)) {
    language <- df$language[i]
    profile <- df$profile[i]

    transformed[i] <- paste0(
      toupper(substring(language, 1, 1)), substring(language, 2), "-",
      toupper(substring(profile, 1, 1)), substring(profile, 2)
    )
  }

  return(transformed)
}

node_generate_html_and_pdf_files <- function(
    year, Rmd_file_list,
    location_site.yml = "site",
    output_folder = "simple_html_no_toc",
    pdf_folder = "pdf",
    private = FALSE,
    private_string = "_pri_documents",
    js_path = "js") {
  dir.create(pdf_folder, showWarnings = FALSE)
  for (file in Rmd_file_list) {
    no_ext <- tools::file_path_sans_ext(file) |> basename()
    cv_type <- Rmd_file_list[Rmd_file_list == file] |> names()

    if (private) {
      no_ext <- paste0(no_ext, private_string)
      cv_type <- paste0(cv_type, private_string)
    }

    print(file)
    print(no_ext)
    print(cv_type)

    render_single_Rmd(file,
      output_dir = output_folder,
      location_site.yml = location_site.yml,
      private = private,
      private_string = private_string
    )

    system(paste(
      "node", file.path(js_path, "print.js"),
      file.path(getwd(), paste0(output_folder, "/", no_ext, ".html")),
      file.path(getwd(), paste0(pdf_folder, "/", year, "_CV_Fer_Roa_", cv_type, ".pdf"))
    ))
  }
}

require(jsonlite)

# Function to read package names from package.json
get_package_names <- function(package_json_path) {
  package_data <- fromJSON(package_json_path)
  package_names <- names(package_data$dependencies)
  return(package_names)
}

# Function to check if npm packages are installed
check_npm_packages_installed <- function(package_names) {
  if (!dir.exists("node_modules")) {
    return(FALSE)
  }

  all_installed <- all(sapply(package_names, function(pkg) dir.exists(file.path("node_modules", pkg))))
  return(all_installed)
}
