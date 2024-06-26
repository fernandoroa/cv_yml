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

render_from_list <- function(parameters, list_from_yaml) {
  default_parameters <- list(
    fields_to_render = NULL,
    fields_right = NULL,
    separator = "<br>",
    is_link = FALSE,
    use_field_names = TRUE,
    html_tag = "span",
    indent = FALSE,
    css_class = "indented-lines-after-first",
    bullet = FALSE,
    bold = FALSE,
    section = FALSE,
    spaces = 0,
    use = TRUE
  )

  combined_parameters <- modifyList(default_parameters, parameters)

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

  remove_childs <- function(fields_to_render) {
    fields_to_render <- lapply(fields_to_render, function(item) {
      if (is.list(item)) {
        names(item)[1]
      } else {
        item
      }
    }) |> unlist()
  }

  get_keys_with_childs <- function(fields_to_render) {
    fields_with_subkeys_list <- lapply(fields_to_render, function(item) {
      if (is.list(item) && !is.null(names(item))) {
        return(item)
      }
    })
    fields_with_subkeys_list <- Filter(Negate(is.null), fields_with_subkeys_list) |> list_flatten()
  }

  if (is.list(parsed_parameters$fields_to_render)) {
    fields_with_subkeys_list <- get_keys_with_childs(parsed_parameters$fields_to_render)
    parsed_parameters$fields_to_render <- remove_childs(parsed_parameters$fields_to_render)
  } else {
    fields_with_subkeys_list <- list()
  }

  if (is.list(parsed_parameters$fields_right)) {
    fields_with_subkeys_list_right <- get_keys_with_childs(parsed_parameters$fields_right)
    parsed_parameters$fields_right <- remove_childs(parsed_parameters$fields_right)
  } else {
    fields_with_subkeys_list_right <- list()
  }

  list_from_yaml_filtered <- list_from_yaml[parsed_parameters$fields_to_render]
  list_from_yaml_filtered <- Filter(Negate(is.null), list_from_yaml_filtered)

  id <- NA
  if ("id" %in% names(list_from_yaml)) {
    id <- list_from_yaml[["id"]]
  }

  list_from_yaml_filtered_right <- list_from_yaml[parsed_parameters$fields_right]
  list_from_yaml_filtered_right <- Filter(Negate(is.null), list_from_yaml_filtered_right)

  separator_breaks_line_bool <- function(separator) {
    case_when(
      grepl("br", separator) ~ TRUE,
      grepl("\n", separator) ~ TRUE,
      .default = FALSE
    )
  }

  indent_one_time <- FALSE

  if (parsed_parameters$spaces > 0) {
    parsed_parameters$separator <- paste0(c(rep("&nbsp;", parsed_parameters$spaces)), collapse = "")
  }

  if (parsed_parameters$indent &&
    parsed_parameters$html_tag == "span" &&
    !separator_breaks_line_bool(parsed_parameters$separator) &&
    !parsed_parameters$use_field_names) {
    indent_one_time <- TRUE
    parsed_parameters$indent <- FALSE
  }

  parsed_parameters__use <- parsed_parameters$use
  if (!is.logical(parsed_parameters$use)) {
    parsed_parameters__use <- parsed_parameters$use
    parsed_parameters$use <- TRUE
  }

  enclose_one_time <- FALSE
  enclose <- TRUE
  if (length(parsed_parameters$html_tag) > 1) {
    enclose_one_time <- TRUE
    enclose <- FALSE
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

  render_indent_class <- function(indent, class = "", type = "open", section = FALSE) {
    case_when(
      !section & indent & type == "open" ~ paste0("<div class=\"", class, "\">"),
      !section & indent & type == "close" ~ "</div>",
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

  print_singleton <- function(separator, bullet, indent, css_class,
                              html_tag, is_link, section, bold, current_field,
                              global_use_field_names, first_failed) {
    if (is.null(item$image)) {
      item$image <- FALSE
    }
    if (is.null(item$use_field_names)) {
      item$use_field_names <- global_use_field_names
    }
    title_matched <- ""
    if (item$use_field_names) {
      title_or_searched_name <- search_names(current_field, params_language, field_names)
      title_matched <- paste0(title_or_searched_name, ": ")
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

    paste0(
      ifelse(idx == 2 & isTRUE(first_failed), "  \n", ""),
      ifelse(idx == 1 & !enclose_one_time, "  \n", ""),
      ifelse(idx != 1, separator, ""),
      ifelse(idx == 1 & bullet, "<ul>", ""),
      ifelse(idx == 1 & section, "  \n## ", ""),
      ifelse(bold, "__", ""),
      render_indent_class(indent, css_class, section = section),
      render_html_tag(html_tag, bullet = bullet, enclose = enclose, section = section),
      title_matched,
      ifelse(is_link, "<", ""),
      ifelse(item$image,
        paste0("![](", knitr::include_graphics(item[["value"]], dpi = 800), ")"),
        item["value"]
      ),
      ifelse(is_link, ">", ""),
      render_html_tag(html_tag, "close", bullet, enclose = enclose, section = section),
      render_indent_class(indent, type = "close", section = section),
      ifelse(bold, "__", ""),
      ifelse(idx == 1 & section, id_value, "")
    )
  }

  print_to_right <- function(separator, enclose = FALSE, current_field,
                             global_use_field_names, left_side_accumulator, left_failed) {
    if (is.null(item$image)) {
      item$image <- FALSE
    }
    class_param <- ""
    if (!is.null(item$class)) {
      class_param <- item$class
    }
    class <- "go-right"
    if (is.null(item$use_field_names)) {
      item$use_field_names <- global_use_field_names
    }

    title_matched <- ""
    if (item$use_field_names) {
      title_or_searched_name <- search_names(current_field, params_language, field_names)
      title_matched <- paste0(title_or_searched_name, ": ")
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
      ifelse(idx == 1 & isTRUE(left_failed), "  \n", ""),
      ifelse(left_side_accumulator[[length(left_side_accumulator)]] == "", "<br>", ""),
      ifelse(!enclose, paste0("<span class=\"", class, " ", class_param, "\">", "")),
      ifelse(idx != 1, separator, ""),
      title_matched,
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

  mask_single_link <- function(input_string, use_server_name = FALSE, use_last_part = FALSE, link_name = "journal link") {
    if (use_server_name) {
      link_name <- get_server_name(input_string)
    } else if (use_last_part) {
      link_name <- get_last_part(input_string)
    }
    paste0("[", link_name, "](", input_string, ")")
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

      result <- paste(server, last_part)
      result <- paste0("[", result, "](", url, ")")
      results <- c(results, result)
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

  modify_item_with_params <- function(item, current_field, fields_with_subkeys_list) {
    if (current_field %in% (fields_with_subkeys_list |> names())) {
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
          } else {
            item[["value"]] <- mask_single_link(item[["value"]])
          }
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

  if ((length(list_from_yaml_filtered_right) + length(list_from_yaml_filtered) == 0) || !parsed_parameters$use) {
    return(NULL)
  }

  string_to_cat <- ""
  if (length(list_from_yaml_filtered_right) > 0 || length(list_from_yaml_filtered) > 0) {
    list_from_yaml_filtered <- filter_list_using_params(list_from_yaml_filtered)
    list_from_yaml_filtered_right <- filter_list_using_params(list_from_yaml_filtered_right)

    string_to_cat <- c(
      string_to_cat,
      render_indent_class(indent = indent_one_time, parsed_parameters$css_class, section = parsed_parameters$section)
    )
    string_to_cat <- c(
      string_to_cat,
      render_html_tag(parsed_parameters$html_tag,
        bullet = parsed_parameters$bullet,
        enclose = enclose_one_time,
        section = parsed_parameters$section
      )
    )
    left_side_accumulator <- ""
    fail_accumulator <- logical()
    for (idx in seq_along(list_from_yaml_filtered)) {
      current_field <- list_from_yaml_filtered[idx] |> names()

      item <- list_from_yaml_filtered[[idx]]
      if (length(item) == 0) next

      item <- use_next_language_when_missing(item)

      if (is.null(item[["value"]]) && idx == 1 && parsed_parameters$section) {
        fail_accumulator <- c(fail_accumulator, TRUE)
      }
      if (is.null(item[["value"]])) next

      item <- modify_item_with_params(item, current_field, fields_with_subkeys_list)

      if (!isTruthy(item[["value"]]) && idx == 1 && parsed_parameters$section) {
        fail_accumulator <- c(fail_accumulator, TRUE)
      }
      if (!isTruthy(item[["value"]])) next

      if (is.character(parsed_parameters__use)) {
        expr <- sub("expr ", "", parsed_parameters__use)
        if (!eval(str2lang(expr)) && idx == 1 && parsed_parameters$section) {
          fail_accumulator <- c(fail_accumulator, TRUE)
        }
        if (!eval(str2lang(expr))) {
          next
        }
      }
      left_side_accumulator <- c(
        left_side_accumulator,
        print_singleton(
          parsed_parameters$separator, parsed_parameters$bullet,
          parsed_parameters$indent, parsed_parameters$css_class,
          parsed_parameters$html_tag, parsed_parameters$is_link,
          parsed_parameters$section, parsed_parameters$bold,
          current_field, parsed_parameters$use_field_names,
          fail_accumulator
        )
      )
    }
    left_side_failed <- FALSE
    if (!is.null(parsed_parameters$whole)) {
      if (parsed_parameters$whole) {
        len_rendered <- length(left_side_accumulator[left_side_accumulator != ""])
        initial_len <- length(list_from_yaml_filtered)
        if (len_rendered < initial_len || len_rendered == 0) {
          left_side_accumulator <- ""
          left_side_failed <- TRUE
        }
      }
    }

    string_to_cat <- c(string_to_cat, left_side_accumulator)

    enclose_right <- FALSE
    if (length(list_from_yaml_filtered_right) > 1) {
      enclose_right <- TRUE
    }
    if (enclose_right) {
      string_to_cat <- c(
        string_to_cat,
        "<span class=\"go-right\">"
      )
    }
    right_side_accumulator <- ""
    for (idx in seq_along(list_from_yaml_filtered_right)) {
      current_field <- list_from_yaml_filtered_right[idx] |> names()

      item <- list_from_yaml_filtered_right[[idx]]
      if (length(item) == 0) next

      item <- use_next_language_when_missing(item)

      if (is.null(item[["value"]])) next

      item <- modify_item_with_params(item, current_field, fields_with_subkeys_list_right)

      if (!isTruthy(item[["value"]])) next

      if (is.character(parsed_parameters__use)) {
        expr <- sub("expr ", "", parsed_parameters__use)
        if (!eval(str2lang(expr))) {
          next
        }
      }
      right_side_accumulator <- c(
        right_side_accumulator,
        print_to_right(
          parsed_parameters$separator, enclose_right,
          current_field, parsed_parameters$use_field_names,
          left_side_accumulator, fail_accumulator
        )
      )
    }

    if (!is.null(parsed_parameters$whole)) {
      if (parsed_parameters$whole) {
        len_rendered <- length(right_side_accumulator[right_side_accumulator != ""])
        initial_len <- length(list_from_yaml_filtered)
        if (len_rendered < initial_len || left_side_failed || len_rendered == 0) {
          right_side_accumulator <- ""
        }
      }
    }

    string_to_cat <- c(string_to_cat, right_side_accumulator)

    if (enclose_right) {
      string_to_cat <- c(
        string_to_cat,
        "</span>"
      )
    }

    string_to_cat <- c(
      string_to_cat,
      render_html_tag(parsed_parameters$html_tag,
        type = "close",
        bullet = parsed_parameters$bullet,
        enclose = enclose_one_time,
        section = parsed_parameters$section
      )
    )
    string_to_cat <- c(
      string_to_cat,
      ifelse(parsed_parameters$bullet, "</ul>", "")
    )
    string_to_cat <- c(
      string_to_cat,
      render_indent_class(indent = indent_one_time, type = "close", section = parsed_parameters$section)
    )
    string_to_cat <- string_to_cat |>
      unlist() |>
      paste0(collapse = "")

    return(string_to_cat)
  }
}

get_chapter_title <- function(list_from_yml, chapter_name) {
  render_bool <- check_for_meaningful_values(list_from_yml)
  section_title <- ""
  if (render_bool) {
    section_title <- search_names(chapter_name, params_language, chapters_names)
  }
}

check_for_meaningful_values <- function(input_list) {
  has_valid_key <- input_list %>%
    map_lgl(~ any(!names(.x) %in% c("params_profile", "params_location") & !is.na(.x))) |>
    any()

  return(has_valid_key)
}

filter_list_using_params <- function(list_to_filter_with_params) {
  has_name_with_value_or_name_is_missing <- function(lst, name, values) {
    name %in% names(lst) && lst[[name]] %in% values || is.null(lst[[name]])
  }
  for (idx in seq_along(list_to_filter_with_params)) {
    item <- list_to_filter_with_params[[idx]]
    if (length(item) == 0) next

    item <- use_next_language_when_missing(item)

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

use_next_language_when_missing <- function(item) {
  if (params_language %in% names(item)) {
    item["value"] <- item[params_language]
  } else if (!"value" %in% names(item)) {
    if (length(intersect(params_valid_languages, names(item))) > 0) {
      item["value"] <- item[params_valid_languages][1]
    }
  }
  item
}
