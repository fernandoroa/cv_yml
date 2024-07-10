require(purrr)
require(dplyr)
require(yaml)
require(shiny)
require(stringr)

search_names <- function(names_vector, key_name, list_with_sublists) {
  find_match <- function(name) {
    if (name %in% names(list_with_sublists)) {
      sublist <- list_with_sublists[[name]]
      if (!is.null(sublist[[key_name]]) && sublist[[key_name]] != "") {
        return(sublist[[key_name]])
      }
      for (value in sublist) {
        if (value != "") {
          return(value)
        }
      }
    }
    return(name)
  }
  map_chr(names_vector, find_match)
}

remove_childs <- function(left) {
  left <- lapply(left, function(item) {
    if (is.list(item)) {
      names(item)[1]
    } else {
      item
    }
  }) |> unlist()
}

get_keys_with_childs <- function(left) {
  fields_with_subkeys_list <- lapply(left, function(item) {
    if (is.list(item) && !is.null(names(item))) {
      return(item)
    }
  })
  fields_with_subkeys_list <- Filter(Negate(is.null), fields_with_subkeys_list) |> list_flatten()
}

separator_breaks_line_bool <- function(separator) {
  case_when(
    grepl("br", separator) ~ TRUE,
    grepl("\n", separator) ~ TRUE,
    .default = FALSE
  )
}

render_html_tag <- function(html_tag, type = "open", bullet = FALSE, enclose = TRUE, section = FALSE) {
  case_when(
    !section & enclose & type == "open" & !bullet ~ map(html_tag, \(x) paste0("<", x, ">")) |> paste(collapse = ""),
    !section & enclose & type == "close" & !bullet ~ map(rev(html_tag), \(x) paste0("</", x, ">")) |> paste(collapse = ""),
    !section & enclose & type == "open" & bullet ~ paste0("<li>"),
    !section & enclose & type == "close" & bullet ~ paste0("</li>"),
    .default = ""
  )
}

render_class <- function(class = "", type = "open") {
  case_when(
    class != "" & type == "open" ~ paste0("<div class=\"", class, "\">"),
    class != "" & type == "close" ~ "</div>",
    .default = ""
  )
}

replace_placeholders <- function(list_of_refs, string) {
  for (item_id in names(list_of_refs)) {
    placeholder <- paste0("<", item_id, ">")
    replacement <- paste0('<span class="ref-sec" data-target="', list_of_refs[[item_id]], '"></span>')
    string <- gsub(placeholder, replacement, string, fixed = TRUE)
  }
  return(string)
}

div_with_class_open <- function(class = "flex-container") {
  paste0('<div class="', class, '">')
}

print_singleton <- function(item, idx, id, separator, bullet, css_class,
                            html_tag, is_link, section, bold, current_field,
                            global_use_field_names, fail_accumulator, field_names,
                            params_private, enclose, enclose_one_time, params_language) {
  if (is.null(item$image)) {
    item$image <- FALSE
  }
  if (is.null(item$use_field_names)) {
    item$use_field_names <- global_use_field_names
  }
  if (is.null(item$class)) {
    item$class <- css_class
  }
  class_param <- item$class

  field_name_matched <- ""
  if (item$use_field_names) {
    field_or_searched_name <- search_names(current_field, params_language, field_names)
    field_name_matched <- paste0(field_or_searched_name, ": ")
  } else if (!is.null(item$custom_field_name)) {
    field_name_matched <- item$custom_field_name
  }

  if (any(grepl("link_id", names(item)))) {
    item_id_names <- grep("link_id", names(item), value = TRUE)
    list_of_refs <- item[item_id_names]
    item[["value"]] <- replace_placeholders(list_of_refs, item[["value"]])
  }

  if (!is.null(item[["use"]])) {
    if (!is.logical(item[["use"]])) {
      expr <- sub("expr ", "", item[["use"]])
      if (!eval(str2lang(expr))) {
        return("")
      }
    } else {
      if (!item[["use"]]) {
        return("")
      }
    }
  }
  id_value <- ""
  if (!is.na(id)) {
    id_value <- paste0("<a id=\"", tryCatch(id, error = function(e) ""), "\"></a>")
  }

  paste_inner <- paste0(
    ifelse(bold, "__", ""),
    render_class(class_param),
    render_html_tag(html_tag, bullet = bullet, enclose = enclose, section = section),
    field_name_matched,
    ifelse(is_link, "<", ""),
    ifelse(item$image,
      paste0("![](", knitr::include_graphics(item[["value"]], dpi = 800), ")"),
      item["value"]
    ),
    ifelse(is_link, ">", ""),
    render_html_tag(html_tag, "close", bullet, enclose = enclose, section = section),
    render_class(class_param, "close"),
    ifelse(bold, "__", "")
  )

  if (length(fail_accumulator) == 0) fail_accumulator <- FALSE

  paste0(
    ifelse(idx == 1 & !enclose_one_time, "  \n", ""),
    ifelse(idx != 1, separator, ""),
    ifelse(idx == 1 & section, "\n## ", ""),
    paste_inner,
    ifelse(idx == 1 & section, id_value, ""),
    ifelse(idx == 1 & section, "  \n  ", "")
  )
}

print_to_right <- function(item, idx, section, separator, enclose = FALSE, current_field,
                           global_use_field_names, left_side_accumulator, left_failed,
                           field_names, params_private, div_class_for_set, params_language) {
  if (is.null(item$image)) {
    item$image <- FALSE
  }
  class_param <- ""
  if (!is.null(item$class)) {
    class_param <- item$class
  }

  class <- "go-right"
  if (grepl("flex-container", div_class_for_set) && section) {
    class <- "go-right-flex-section"
  } else if (grepl("flex-container", div_class_for_set)) {
    class <- ""
  }

  if (is.null(item$use_field_names)) {
    item$use_field_names <- global_use_field_names
  }
  field_name_matched <- ""
  if (item$use_field_names) {
    field_or_searched_name <- search_names(current_field, params_language, field_names)
    field_name_matched <- paste0(field_or_searched_name, ": ")
  } else if (!is.null(item$custom_field_name)) {
    field_name_matched <- item$custom_field_name
  }

  if (!is.null(item[["use"]])) {
    if (!is.logical(item[["use"]])) {
      expr <- sub("expr ", "", item[["use"]])
      if (!eval(str2lang(expr))) {
        return("")
      }
    } else {
      if (!item[["use"]]) {
        return("")
      }
    }
  }

  paste0(
    ifelse(idx == 1 & all(isTRUE(left_failed)), "  \n", ""),
    ifelse(left_side_accumulator[[length(left_side_accumulator)]] == "", "<br>", ""),
    ifelse(!enclose, paste0("<span class=\"", class, " ", class_param, "\">", "")),
    ifelse(idx != 1, separator, ""),
    field_name_matched,
    ifelse(item$image,
      paste0("![](", knitr::include_graphics(item[["value"]]), ")"),
      item["value"]
    ),
    ifelse(!enclose, "</span>", "")
  )
}

sort_string_items <- function(input_string) {
  input_string |>
    str_split_1(",\\s*") |>
    sort() |>
    str_c(collapse = ", ")
}

render_link <- function(link_name, url) {
  paste0("[", link_name, "](", url, "){target=\"_blank\"}")
}

mask_single_link <- function(url, use_server_name = FALSE, use_last_part = FALSE,
                             use_string = FALSE, link_name = "link", use_field = FALSE,
                             after_comma = "", list_from_yaml_filtered = NULL, params_language, params_valid_languages) {
  if (use_server_name) {
    link_name <- get_server_name(url)
  } else if (use_last_part) {
    link_name <- get_last_part(url)
  } else if (use_string) {
    link_name <- get_custom_string(after_comma)
  } else if (use_field) {
    link_name <- get_field_value(after_comma, list_from_yaml_filtered, params_language, params_valid_languages)
  }
  render_link(link_name, url)
}

get_field_value <- function(after_comma, list_from_yaml_filtered, params_language, params_valid_languages) {
  field <- sub("^\\s*use_field:\\s*", "", after_comma)
  field_item <- use_next_language_when_missing(list_from_yaml_filtered[[field]], params_language, params_valid_languages)
  link_name <- field_item[["value"]]
}

get_custom_string <- function(after_comma) {
  custom_string <- sub("^\\s*use_string:\\s*", "", after_comma)
}

get_server_name <- function(url) {
  url_no_protocol <- sub("^https?://", "", url)

  server_name <- sub("^[^.]+\\.([^.]+\\.[^.]+).*", "\\1", url_no_protocol)
  server_name <- sub("/.*", "", server_name)

  return(server_name)
}

get_last_part <- function(url) {
  url <- sub("/$", "", url)
  last_part <- sub(".*/([^/]+)$", "\\1", url)

  return(last_part)
}

multi_extract_server_and_last_part <- function(urls) {
  url_list <- str_split(urls, ",\\s*")[[1]]

  results <- c()

  for (url in url_list) {
    server <- get_server_name(url)
    server <- gsub("\\.com", "", server)
    server <- gsub("r-project.org", "CRAN", server)

    last_part <- get_last_part(url)
    last_part <- gsub("package=", "", last_part)

    link_name <- paste(server, last_part)
    rendered_link <- render_link(link_name, url)

    results <- c(results, rendered_link)
  }

  return(paste(results, collapse = ", "))
}

get_after_comma <- function(x) {
  if (str_detect(x, ",")) {
    str_trim(str_extract(x, "(?<=,).*"))
  } else {
    ""
  }
}

modify_item_with_params <- function(item, current_field, fields_with_subkeys_list, params_location,
                                    params_profile, params_allow_proof_documents, list_from_yaml_filtered,
                                    params_language, params_valid_languages) {
  if (current_field %in% (fields_with_subkeys_list |> names())) {
    if (current_field == "proof" && !params_allow_proof_documents) {
      item[["value"]] <- NULL
      return(item)
    }
    subkeys_of_field <- fields_with_subkeys_list[[current_field]] |> names()
    for (subkey in subkeys_of_field) {
      subkey_value <- fields_with_subkeys_list[[current_field]][[subkey]]
      if (subkey_value == "sort") {
        item[["value"]] <- sort_string_items(item[["value"]])
      }
      if (grepl("mask_link", subkey_value)) {
        after_comma <- get_after_comma(subkey_value)
        if (after_comma == "use_server_name") {
          item[["value"]] <- mask_single_link(item[["value"]], use_server_name = TRUE)
        } else if (after_comma == "use_last_part") {
          item[["value"]] <- mask_single_link(item[["value"]], use_last_part = TRUE)
        } else if (after_comma == "use_server_and_last_part") {
          item[["value"]] <- multi_extract_server_and_last_part(item[["value"]])
        } else if (grepl("use_string", after_comma)) {
          item[["value"]] <- mask_single_link(item[["value"]], use_string = TRUE, after_comma = after_comma)
        } else if (grepl("use_field", after_comma)) {
          item[["value"]] <- mask_single_link(item[["value"]],
            use_field = TRUE,
            after_comma = after_comma, list_from_yaml_filtered = list_from_yaml_filtered,
            params_language = params_language, params_valid_languages = params_valid_languages
          )
        } else {
          item[["value"]] <- mask_single_link(item[["value"]])
        }
      }
      if (subkey_value == "make_badge" && current_field == "doi") {
        item[["value"]] <- badge_to_doi(item[["value"]])
      }
      if (subkey == "params_profile") {
        if (subkey_value != params_profile && params_profile != "general") {
          item[["value"]] <- NULL
        }
      }
      if (subkey == "params_location") {
        if (subkey_value != params_location && params_location != "general") {
          item[["value"]] <- NULL
        }
      }
      if (subkey == "params_use") {
        if (!subkey_value) {
          item[["value"]] <- NULL
        }
      }
      item[[subkey]] <- subkey_value
    }
  }
  item
}

get_default_values <- function(combined_parameters, parameters) {
  parsed_parameters <- imap(combined_parameters, ~ {
    sublist_value <- parameters[[.y]]
    if (length(sublist_value) > 1) {
      return(sublist_value)
    } else if (is.null(sublist_value) || is.na(sublist_value)) {
      return(.x)
    } else {
      return(sublist_value)
    }
  })
  parsed_parameters
}

filter_list_from_yaml <- function(list_from_yaml, params) {
  list_from_yaml_filtered <- list_from_yaml[params]
  list_from_yaml_filtered <- Filter(Negate(is.null), list_from_yaml_filtered)
}

overwrite_separator_with_spaces <- function(spaces) {
  separator <- paste0(c(rep("&nbsp;", spaces)), collapse = "")
}


check_for_meaningful_values <- function(input_list) {
  has_valid_key <- input_list %>%
    map_lgl(~ any(!names(.x) %in% c("params_profile", "params_location") & !is.na(.x))) |>
    any()

  return(has_valid_key)
}

filter_list_using_params <- function(
    list_to_filter_with_params,
    params_language,
    params_valid_languages,
    params_location,
    params_profile) {
  has_name_with_value_or_name_is_missing <- function(lst, name, values) {
    name %in% names(lst) && lst[[name]] %in% values || is.null(lst[[name]])
  }
  for (idx in seq_along(list_to_filter_with_params)) {
    item <- list_to_filter_with_params[[idx]]
    if (length(item) == 0) next

    item <- use_next_language_when_missing(item, params_language, params_valid_languages)

    profile_check <- has_name_with_value_or_name_is_missing(item, "params_profile", c("general", params_profile))
    location_check <- has_name_with_value_or_name_is_missing(item, "params_location", c("general", params_location))
    use_check <- has_name_with_value_or_name_is_missing(item, "params_use", TRUE)

    if (!(profile_check && location_check && use_check)) {
      list_to_filter_with_params[[idx]] <- NA
    }
  }

  remove_nas <- function(x) {
    if (is.list(x)) {
      x <- lapply(x, remove_nas)
      x <- Filter(Negate(is.null), x)
    } else if (is.na(x)) {
      x <- NULL
    }
    return(x)
  }

  list_to_filter_with_params <- remove_nas(list_to_filter_with_params)
  list_to_filter_with_params <- Filter(Negate(is.na), list_to_filter_with_params)
  list_to_filter_with_params <- Filter(Negate(is.null), list_to_filter_with_params)
  return(list_to_filter_with_params)
}

use_next_language_when_missing <- function(item, params_language, params_valid_languages) {
  if (params_language %in% names(item)) {
    item["value"] <- item[params_language]
  } else if (!"value" %in% names(item)) {
    if (length(intersect(params_valid_languages, names(item))) > 0) {
      item["value"] <- item[params_valid_languages][1]
    }
  }
  item
}

badge_to_doi <- function(doi) {
  doi_link <- paste0("https://doi.org/", doi)

  transformed_link <- sprintf(
    "[![](https://img.shields.io/badge/doi-%s-green.svg)](%s){target=\"_blank\"}",
    gsub("-", "--", doi),
    doi_link
  )

  return(transformed_link)
}
