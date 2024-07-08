require(bib2df) |> invisible()

process_bib_folder <- function(bib_folder = "bib") {
  list_of_bib_files <- list.files(pattern = ".bib", path = file.path(dirname(getwd()), bib_folder))
  bib_file_paths <- file.path("..", bib_folder, list_of_bib_files)

  if (length(bib_file_paths) > 0) {
    filename <- "all.bib"
    a_temp_dir <- tempdir()
    tempfile <- file.path(a_temp_dir, filename)
    outFile <- file(tempfile, "w")
    for (i in bib_file_paths) {
      x <- readLines(i)
      writeLines(x, outFile)
    }
    close(outFile)
  }

  tryCatch(tibble_from_bib <<- bib2df(tempfile), error = function(e) {
    cat("all.bib not found")
    return(NULL)
  })

  list_data <- df_to_list_rowwise(tibble_from_bib, latex_to_unicode)
  yml_path <- file.path(a_temp_dir, "articles.yml")
  write_yaml(list_data, yml_path)
  return(yml_path)
}


latex_to_unicode <- list(
  "\\{\\\\`\\{a\\}\\}" = "à",
  "\\{\\\\'\\{a\\}\\}" = "á",
  "\\{\\\\\\^\\{a\\}\\}" = "â",
  "\\{\\\\~\\{a\\}\\}" = "ã",
  "\\{\\\\\"\\{a\\}\\}" = "ä",
  "\\{\\\\`\\{e\\}\\}" = "è",
  "\\{\\\\'\\{e\\}\\}" = "é",
  "\\{\\\\\\^\\{e\\}\\}" = "ê",
  "\\{\\\\\"\\{e\\}\\}" = "ë",
  "\\{\\\\`\\{i\\}\\}" = "ì",
  "\\{\\\\'\\{i\\}\\}" = "í",
  "\\{\\\\\\^\\{i\\}\\}" = "î",
  "\\{\\\\\"\\{i\\}\\}" = "ï",
  "\\{\\\\`\\{o\\}\\}" = "ò",
  "\\{\\\\'\\{o\\}\\}" = "ó",
  "\\{\\\\\\^\\{o\\}\\}" = "ô",
  "\\{\\\\~\\{o\\}\\}" = "õ",
  "\\{\\\\\"\\{o\\}\\}" = "ö",
  "\\{\\\\`\\{u\\}\\}" = "ù",
  "\\{\\\\'\\{u\\}\\}" = "ú",
  "\\{\\\\\\^\\{u\\}\\}" = "û",
  "\\{\\\\\"\\{u\\}\\}" = "ü",
  "\\{\\\\~\\{n\\}\\}" = "ñ"
)
replace_latex_accents <- function(text, mapping) {
  for (latex_command in names(mapping)) {
    pattern <- latex_command
    replacement <- mapping[[latex_command]]
    text <- gsub(pattern, replacement, text, perl = TRUE)
  }
  return(text)
}

df_to_list_rowwise <- function(df, latex_mapping) {
  list_data <- list()
  df <- df |> select(-ABSTRACT)

  for (i in 1:nrow(df)) {
    row <- df[i, ]

    row_list <- list()

    for (colname in names(row)) {
      value <- row[[colname]]

      if (!is.na(value)) {
        if (is.list(value)) {
          value <- paste(unlist(value), collapse = "; ")
        }
        value <- sub("^\\{(.*?)\\}$", "\\1", value)

        if (colname == "PAGES") {
          value <- gsub("--", "-", value)
        }
        row_list[[tolower(colname)]] <- list(english = replace_latex_accents(value, latex_mapping))
      }
    }
    row_list <- c(id = row$BIBTEXKEY, row_list)
    list_data[[i]] <- row_list
  }
  return(list_data)
}
