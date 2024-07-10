require(purrr)
require(dplyr)
require(yaml)
require(shiny)
require(stringr)

render_from_list <- function(
    parameters, list_from_yaml, params_language, field_names,
    params_private, params_valid_languages, params_location,
    params_profile, params_allow_proof_documents) {
  default_parameters <- list(
    left = NULL,
    right = NULL,
    separator = "<br>",
    is_link = FALSE,
    use_field_names = TRUE,
    html_tag = "span",
    div_class_for_set = "",
    right_align = "float",
    class = "",
    bullet = FALSE,
    bold = FALSE,
    section = FALSE,
    spaces = 0,
    use = TRUE
  )

  combined_parameters <- modifyList(default_parameters, parameters)

  parsed_parameters <- get_default_values(combined_parameters, parameters)

  if (is.list(parsed_parameters$left)) {
    left_fields_with_subkeys_list <- get_keys_with_childs(parsed_parameters$left)
    parsed_parameters$left <- remove_childs(parsed_parameters$left)
  } else {
    left_fields_with_subkeys_list <- list()
  }

  if (is.list(parsed_parameters$right)) {
    right_fields_with_subkeys_list <- get_keys_with_childs(parsed_parameters$right)
    parsed_parameters$right <- remove_childs(parsed_parameters$right)
  } else {
    right_fields_with_subkeys_list <- list()
  }

  id <- NA
  if ("id" %in% names(list_from_yaml)) {
    id <- list_from_yaml[["id"]]
  }

  left_list_from_yaml_filtered <- filter_list_from_yaml(list_from_yaml, parsed_parameters$left)

  right_list_from_yaml_filtered <- filter_list_from_yaml(list_from_yaml, parsed_parameters$right)

  if (parsed_parameters$spaces > 0) {
    parsed_parameters$separator <- overwrite_separator_with_spaces(parsed_parameters$spaces)
  }

  parsed_parameters__use <- parsed_parameters$use
  if (!is.logical(parsed_parameters$use)) {
    parsed_parameters__use <- parsed_parameters$use
    parsed_parameters$use <- TRUE
  }

  if (parsed_parameters$right_align == "flex") {
    parsed_parameters$div_class_for_set <- paste(parsed_parameters$div_class_for_set, "flex-container")
  }

  enclose_one_time <- FALSE

  enclose <- TRUE

  if (length(parsed_parameters$html_tag) > 1) {
    enclose_one_time <- TRUE
    enclose <- FALSE
  }

  if (((length(right_list_from_yaml_filtered) + length(left_list_from_yaml_filtered)) == 0) || !parsed_parameters$use) {
    return(NULL)
  } else {
    string_to_cat <- ""

    if (length(right_list_from_yaml_filtered) > 0 || length(left_list_from_yaml_filtered) > 0) {
      left_list_from_yaml_filtered <- filter_list_using_params(
        left_list_from_yaml_filtered,
        params_language, params_valid_languages,
        params_location, params_profile
      )
      right_list_from_yaml_filtered <- filter_list_using_params(
        right_list_from_yaml_filtered,
        params_language, params_valid_languages,
        params_location, params_profile
      )

      left_side_accumulator <- ""
      fail_accumulator <- logical()

      for (idx in seq_along(left_list_from_yaml_filtered)) {
        current_field <- left_list_from_yaml_filtered[idx] |> names()

        item <- left_list_from_yaml_filtered[[idx]]
        if (length(item) == 0) next

        item <- use_next_language_when_missing(item, params_language, params_valid_languages)

        if (is.null(item[["value"]]) && idx == 1 && parsed_parameters$section) {
          fail_accumulator <- c(fail_accumulator, TRUE)
        }
        if (is.null(item[["value"]])) next

        item <- modify_item_with_params(
          item, current_field, left_fields_with_subkeys_list,
          params_location, params_profile, params_allow_proof_documents, list_from_yaml,
          params_language, params_valid_languages
        )

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
            item, idx, id,
            parsed_parameters$separator, parsed_parameters$bullet,
            parsed_parameters$class,
            parsed_parameters$html_tag, parsed_parameters$is_link,
            parsed_parameters$section, parsed_parameters$bold,
            current_field, parsed_parameters$use_field_names,
            fail_accumulator, field_names, params_private, enclose, enclose_one_time,
            params_language
          )
        )
      }

      left_side_failed <- FALSE

      if (!is.null(parsed_parameters$whole)) {
        if (parsed_parameters$whole) {
          len_rendered <- length(left_side_accumulator[left_side_accumulator != ""])
          initial_len <- length(left_list_from_yaml_filtered)
          if (len_rendered < initial_len || len_rendered == 0) {
            left_side_accumulator <- ""
            left_side_failed <- TRUE
          }
        }
      }

      enclose_right <- FALSE

      if (length(right_list_from_yaml_filtered) > 1) {
        enclose_right <- TRUE
      }

      right_side_accumulator <- ""
      for (idx in seq_along(right_list_from_yaml_filtered)) {
        current_field <- right_list_from_yaml_filtered[idx] |> names()

        item <- right_list_from_yaml_filtered[[idx]]
        if (length(item) == 0) next

        item <- use_next_language_when_missing(item, params_language, params_valid_languages)

        if (is.null(item[["value"]])) next

        item <- modify_item_with_params(
          item, current_field, right_fields_with_subkeys_list,
          params_location, params_profile, params_allow_proof_documents, right_list_from_yaml_filtered,
          params_language, params_valid_languages
        )

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
            item, idx, parsed_parameters$section,
            parsed_parameters$separator, enclose_right,
            current_field, parsed_parameters$use_field_names,
            left_side_accumulator, fail_accumulator,
            field_names, params_private, parsed_parameters$div_class_for_set,
            params_language
          )
        )
      }

      if (!is.null(parsed_parameters$whole)) {
        if (parsed_parameters$whole) {
          len_rendered <- length(right_side_accumulator[right_side_accumulator != ""])
          initial_len <- length(left_list_from_yaml_filtered)
          if (len_rendered < initial_len || left_side_failed || len_rendered == 0) {
            right_side_accumulator <- ""
          }
        }
      }

      string_to_cat <- c(
        string_to_cat,
        render_html_tag(parsed_parameters$html_tag,
          bullet = parsed_parameters$bullet,
          enclose = enclose_one_time,
          section = parsed_parameters$section
        )
      )

      string_to_cat <- c(
        string_to_cat,
        ifelse(parsed_parameters$bullet, "<ul>", "")
      )

      left_side_accumulator_empty_test <- gsub("^ +| +$", "", left_side_accumulator)

      right_side_accumulator_empty_test <- gsub("^ +| +$", "", right_side_accumulator)

      if (all(left_side_accumulator_empty_test == "") && all(right_side_accumulator_empty_test == "")) {
        parsed_parameters$div_class_for_set <- ""
      }

      string_to_cat <- c(
        string_to_cat,
        ifelse(parsed_parameters$div_class_for_set != "", div_with_class_open(parsed_parameters$div_class_for_set), "")
      )

      string_to_cat <- c(string_to_cat, left_side_accumulator)

      if (enclose_right && parsed_parameters$div_class_for_set != "") {
        string_to_cat <- c(
          string_to_cat,
          "<span class=\"go-right\">"
        )
      } else if (enclose_right && parsed_parameters$div_class_for_set == "flex-container") {
        string_to_cat <- c(
          string_to_cat,
          "<span class=\"go-right-flex-section\">"
        )
      }

      string_to_cat <- c(string_to_cat, right_side_accumulator)

      if (enclose_right) {
        string_to_cat <- c(
          string_to_cat,
          "</span>"
        )
      }

      if (parsed_parameters$div_class_for_set != "") {
        string_to_cat <- c(
          string_to_cat,
          "</div>"
        )
      }

      string_to_cat <- c(
        string_to_cat,
        ifelse(parsed_parameters$bullet, "</ul>", "")
      )

      string_to_cat <- c(
        string_to_cat,
        render_html_tag(parsed_parameters$html_tag,
          type = "close",
          bullet = parsed_parameters$bullet,
          enclose = enclose_one_time,
          section = parsed_parameters$section
        )
      )

      string_to_cat <- string_to_cat |>
        unlist() |>
        paste0(collapse = "")
    }

    return(string_to_cat)
  }
}

get_chapter_title <- function(list_from_yml, chapter_name, chapters_names, params_language) {
  render_bool <- check_for_meaningful_values(list_from_yml)
  section_title <- ""
  if (render_bool) {
    section_title <- search_names(chapter_name, params_language, chapters_names)
  }
}
