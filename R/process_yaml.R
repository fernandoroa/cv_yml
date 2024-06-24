read_yml_and_check_levels <- function(yaml_file) {
  yml_list <- classify_yaml_structure(yaml_file)

  original_attributes <- attributes(yml_list)

  if (attr(yml_list, "yaml_type") == "one_level") {
    yml_list <- replace_system_vars_in_sublist(yml_list)
  } else if (attr(yml_list, "yaml_type") == "multi_level") {
    has_year <- map_lgl(yml_list, ~ "year" %in% names(.x))
    if (any(has_year)) {
      sublists_with_year <- yml_list[has_year]
      sublists_without_year <- yml_list[!has_year]
      sorted_indices <- order(
        map_int(sublists_with_year, ~ extract_first_year(.x$year[[1]])),
        decreasing = TRUE
      )
      sorted_sublists_with_year <- sublists_with_year[sorted_indices]
      yml_list <- c(sorted_sublists_with_year, sublists_without_year)
    }
    yml_list <- map(yml_list, replace_system_vars_in_sublist)
    yml_list <- modify_nested_list(yml_list)
  }
  attributes(yml_list) <- original_attributes
  yml_list
}

extract_first_year <- function(year_str) {
  as.integer(str_extract(year_str, "^\\d{4}"))
}

classify_yaml_structure <- function(yaml_file) {
  data <- yaml::yaml.load_file(yaml_file)

  if (!is.list(data)) {
    return(NULL)
  }

  # Check if 'data' is a one-level structure
  is_one_level <- all(map_lgl(data, ~ is.list(.) && !any(names(.) == "id")))

  # Check if 'data' is a multi-level structure (- id: ...)
  is_multi_level <- all(map_lgl(data, ~ is.list(.) && "id" %in% names(.)))

  if (is_one_level) {
    attr(data, "yaml_type") <- "one_level"
  } else if (is_multi_level) {
    attr(data, "yaml_type") <- "multi_level"
  } else {
    attr(data, "yaml_type") <- "Unknown structure"
  }

  return(data)
}

# Function to apply changes to nested list and return the modified list
modify_nested_list <- function(nested_list) {
  modified_list <- map(nested_list, function(lst) {
    param_keys <- find_params_keys(lst)
    result_list <- lst
    for (idx_key in seq_along(param_keys)) {
      result_list <- modify(result_list, add_key_to_sublist,
        key = names(param_keys[idx_key]),
        value = param_keys[[idx_key]]
      )
    }
    return(result_list)
  })
  return(modified_list)
}

find_params_keys <- function(lst) {
  params_keys <- names(lst)[grepl("^params", names(lst))]
  params_list <- lst[params_keys]
  return(params_list)
}

# Recursive function to propagate 'params' keys to sublists
add_key_to_sublist <- function(lst, key, value) {
  if (is.list(lst) && is.null(lst[[key]])) {
    lst[[key]] <- value
  }
  return(lst)
}

replace_system_vars_in_sublist <- function(sublist) {
  sublist_names <- names(sublist)

  for (name in sublist_names) {
    if (is.list(sublist[[name]]) && "system_var" %in% names(sublist[[name]]) && params_private) {
      system_var_value <- Sys.getenv(sublist[[name]]$system_var, unset = NA)
      sublist[[name]]["value"] <- system_var_value
      sublist[[name]]$system_var <- NULL
    } else if (is.list(sublist[[name]]) && "system_var" %in% names(sublist[[name]]) && !params_private) {
      sublist[[name]]$system_var <- NULL
    }
  }
  return(sublist)
}

process_all_chapters <- function(chapters) {
  for (chapter_entry in chapters) {
    if (is.character(chapter_entry)) {
      chapter <- chapter_entry
      use_chapter <- TRUE
      yaml_file <- file.path("../data", paste0(chapter, ".yml"))
    } else {
      chapter <- names(chapter_entry)
      chapter_details <- chapter_entry[[1]]
      # Assume use is true if not specified
      use_chapter <- if (is.null(chapter_details$use)) TRUE else chapter_details$use

      if (is.null(chapter_details$bib)) {
        yaml_file <- file.path("data", paste0(chapter, ".yml"))
      } else if (chapter_details$bib) {
        yaml_file <- process_bib_folder()
      }
    }

    if (use_chapter) {
      data_list_from_yaml <- read_yml_and_check_levels(yaml_file)

      config_links <- yaml::read_yaml(file.path("../config", paste0(chapter, ".yml")))
      chapter_content <- ""

      if (attr(data_list_from_yaml, "yaml_type") == "one_level") {
        chapter_content <- map(config_links, \(x) render_from_list(x, list_from_yaml = data_list_from_yaml)) |> invisible()
      } else if (attr(data_list_from_yaml, "yaml_type") == "multi_level") {
        chapter_content <- map(data_list_from_yaml, \(y) {
          map(config_links, \(x) render_from_list(x, list_from_yaml = y)) |> invisible()
        }) |>
          invisible()
      }
      chapter_content_concat <- chapter_content |>
        unlist() |>
        paste0(collapse = "")

      if (chapter_content_concat != "") {
        section_title <- get_chapter_title(data_list_from_yaml, chapter)
        cat(paste0("<br>   \n  \n# ", section_title))
        cat(chapter_content_concat)
      }
    }
  }
}
