replace_first_html <- function(input_string, replacement) {
  pattern <- "([^\"]+\\.html)"
  output_string <- sub(pattern, replacement, input_string)

  return(output_string)
}

generateCategoryTitles <- function(codes, academic, developer) {
  output <- "const categoryTitles = {\n"

  for (code in codes) {
    devel_translated <- sub("([[:alpha:]])", "\\U\\1", developer[names(codes)[codes == code]], perl = T)
    acad_translated <- sub("([[:alpha:]])", "\\U\\1", academic[names(codes)[codes == code]], perl = T)
    output <- paste0(
      output, "  ", code, ": {\n",
      "    ", quote(developer), ": \"", devel_translated, "\",\n",
      "    ", quote(academic), ": \"", acad_translated, "\",\n",
      "  },\n"
    )
  }

  output <- paste0(output, "};\n")

  return(output)
}

order_by_numeric_part <- function(strings) {
  numeric_parts <- as.numeric(gsub(".*_(\\d+)$", "\\1", strings))

  ordered_strings <- strings[order(numeric_parts)]

  return(ordered_strings)
}

lint_js <- function(html_file) {
  if (!require("xml2")) {
    install.packages("xml2")
    library(xml2)
  }

  html_content <- readLines(html_file, warn = FALSE)
  doc <- read_html(paste(html_content, collapse = "\n"))
  script_node <- xml_find_first(doc, "//script")
  if (!is.null(script_node)) {
    script_content <- xml_text(script_node)
    temp_js_file <- tempfile(fileext = ".js")
    writeLines(script_content, temp_js_file)
    system(paste("node_modules/.bin/prettier --write", temp_js_file), intern = TRUE)
    add_leading_spaces(temp_js_file)
    modified_script_content <- readLines(temp_js_file, warn = FALSE)
    writeLines(c("<script>", modified_script_content, "</script>"), html_file)
    # Clean up the temporary file
    unlink(temp_js_file)
  }
}

add_leading_spaces <- function(input_file, output_file = NULL) {
  lines <- readLines(input_file, warn = FALSE)

  modified_lines <- sapply(lines, function(line) {
    if (line == "") {
      line
    } else {
      paste0("  ", line)
    }
  })

  if (is.null(output_file)) {
    writeLines(modified_lines, input_file)
  } else {
    writeLines(modified_lines, output_file)
  }
}

lint_html <- function(input_file, start_marker, end_marker) {
  lines <- readLines(input_file)

  start_pos <- grep(start_marker, lines)
  end_pos <- grep(end_marker, lines)

  line_shift <- 0

  for (idx in seq_along(start_pos)) {
    current_start_pos <- start_pos[idx] + line_shift
    current_end_pos <- end_pos[idx] + line_shift

    content <- lines[(current_start_pos + 1):(current_end_pos - 1)]

    temp_file <- tempfile(fileext = ".html")
    writeLines(content, temp_file)

    system(paste("node_modules/.bin/prettier --write", temp_file))

    modified_content <- readLines(temp_file)

    modified_content <- sapply(modified_content, function(line) {
      if (line == "") {
        line
      } else {
        paste0(paste0(rep(" ", 8), collapse = ""), line)
      }
    })

    original_length <- current_end_pos - current_start_pos - 1
    modified_length <- length(modified_content)
    line_shift <- line_shift + (modified_length - original_length)

    lines <- c(
      lines[1:current_start_pos],
      modified_content,
      lines[current_end_pos:length(lines)]
    )
  }

  writeLines(lines, input_file)
}
