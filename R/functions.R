library(purrr)
library(dplyr)
library(yaml)
library(shiny)
addFields <- function(leftFields, rightField = NA, xCasted, xFielddata, spaces, separator = "", ifNAsep = FALSE,
                      xtitle, Section = F, Bold = F, xtitleRight = F, FontSizeRightSmall = F) {
  for (j in 1:length(leftFields)) {
    if (nrow(xFielddata[which(xFielddata$field %in% leftFields[j]), ]) == 0) {
      xFielddata[(nrow(xFielddata) + 1), ] <- leftFields[j]
    }
  }

  xCasted[, setdiff(leftFields, colnames(xCasted))] <- as.character(NA)

  if (!is.na(rightField)) {
    for (j in 1:length(rightField)) {
      if (nrow(xFielddata[which(xFielddata$field %in% rightField[j]), ]) == 0) {
        xFielddata[(nrow(xFielddata) + 1), ] <- rightField[j]
      }
    }
    xCasted[, setdiff(rightField, colnames(xCasted))] <- as.character(NA)
  }
  for (j in 1:length(leftFields)) {
    assign(paste0("t", j), which(xFielddata$field %in% leftFields[j]))
  }
  vec2 <- ls(pattern = "^t")
  vec2 <- unlist(lapply(vec2, function(x) eval(parse(text = x))))

  for (k in 1) {
    t2 <- which(xFielddata$field %in% rightField)
    emspacer <- ifelse(is.na(xCasted[, which(colnames(xCasted) %in% xFielddata[vec2[k], 1])][i]), "<br>", "")
    cat(
      ifelse(
        !is.na(xCasted[, which(colnames(xCasted) %in% xFielddata[vec2[k], 1])][i]),
        paste0(
          "   \n",
          ifelse(Section, "  \n## ", "")
          , ifelse(Bold, "__", ""),
          ifelse(xtitle,
            paste0(xFielddata[vec2[k], languagePos], ": "),
            ""
          ),
          paste(xCasted[, which(colnames(xCasted) %in% xFielddata[vec2[k], 1])][i]),
          ifelse(Bold, "__", "")
        ),
        ifelse(ifNAsep, paste0("<br>"), "")
      )
    )
  }
  if (length(vec2) > 1) {
    for (k in 2:length(vec2)) {
      cat(
        ifelse(!is.na(xCasted[, which(colnames(xCasted) %in% xFielddata[vec2[k], 1])][i]),
          paste0(
            separator,
            paste0(c(rep("&nbsp;", spaces)), collapse = ""),
            ifelse(xtitle,
              paste0(xFielddata[vec2[k], languagePos], ": "),
              ""
            ),
            paste(xCasted[, which(colnames(xCasted) %in% xFielddata[vec2[k], 1])][i])
          ),
          ifelse(ifNAsep, paste0("<br>"), "")
        )
      )
    }
  }
  if (length(t2 > 0)) {
    cat(
      ifelse(!is.na(xCasted[, which(colnames(xCasted) %in% xFielddata[t2, 1])][i]),
        paste0(
          emspacer, ifelse(FontSizeRightSmall, '<font size="2">', ""),
          paste(
            "[",
            ifelse(xtitleRight,
              paste0(xFielddata[t2, languagePos], ": "),
              ""
            ),
            xCasted[, which(colnames(xCasted) %in% xFielddata[t2, 1])][i],
            "]{style=\"float:right; font-family:Open Sans\" }"
          ),
          emspacer, ifelse(FontSizeRightSmall, "</font>", "")
        ), ""
      )
    )
  }
}

#
#  example use:
#  CommonTitleOnly("signature",fieldIts)
#

CommonTitleOnly <- function(leftFields, xFielddata) {
  for (j in 1:length(leftFields)) {
    if (nrow(xFielddata[which(xFielddata$field %in% leftFields[j]), ]) == 0) {
      xFielddata[(nrow(xFielddata) + 1), ] <- leftFields[j]
    } # if
  }
  vars <- sapply(leftFields, function(x) grep(x, xFielddata$field))
  for (t in vars) {
    cat(
      ifelse(!is.na(xFielddata[t, languagePos]),
        paste0("  \n", xFielddata[t, languagePos], ": "), ""
      )
    )
  }
}

printLogo <- function(field = "logo", xCasted) {
  xCasted[, setdiff(field, colnames(xCasted))] <- NA
  for (j in 1:length(field)) {
    if (file.exists(xCasted[, which(colnames(xCasted) %in% field[j])][i])) {
      cat(paste0("<img src=", xCasted[, which(colnames(xCasted) %in% field[j])][i], " width=\"7%\" style=\"float:right\">"))
    }
  }
}

readDataTypes <- function(typeofdata) {
  tryCatch(
    {
      listofDataItr <- lapply(listofAllItsr, function(x) x[attr(x, "dataType") == typeofdata])
      listofDataItr <- Filter(function(u) ncol(u) != 0, listofDataItr)
      if (nrow(listofDataItr[[1]]) > 0) {
        cat(paste0("<br>   \n  \n# ", SectionIts[which(SectionIts$dataType %in% typeofdata), languagePos]))
        dcastedDatalist <- lapply(listofDataItr, function(x) {
          reshape2::dcast(reshape2::melt(x[, c("field", language)],
            id.var = "field"
          ), variable ~ factor(field, levels = unique(field)))
        })
        DataCasted <- plyr::rbind.fill(dcastedDatalist)

        if ("year" %in% colnames(DataCasted)) {
          DataCasted <- DataCasted[order(DataCasted$year, decreasing = T), ]
        }
        DataCasted <- as.data.frame(DataCasted)
        DataCasted[DataCasted == "-"] <- NA
        return(DataCasted)
      } else {
        return(data.frame())
      }
    },
    error = function(e) {
      return(data.frame())
    }
  )
}

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

check_for_meaningful_values <- function(input_list) {
  has_valid_key <- input_list %>%
    map_lgl(~ any(!names(.x) %in% c("params_profile", "params_location") & !is.na(.x))) |>
    any()

  return(has_valid_key)
}

# Function to replace "system_var" with its value from environment variables
replace_system_vars <- function(sublist) {
  if (!is.null(sublist$system_var)) {
    sublist$value <- Sys.getenv(sublist$system_var, unset = NA)
    sublist$system_var <- NULL
  }
  return(sublist)
}

render_singletons_yml <- function(parameters, list_from_yaml) {
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
    image = FALSE
  )

  parsed_parameters <- imap(default_parameters, ~ {
    sublist_value <- parameters[[.y]]
    if (length(sublist_value) > 1) {
      return(sublist_value)
    } else if (is.null(sublist_value) || is.na(sublist_value)) {
      return(.x)
    } else {
      return(sublist_value)
    }
  })

  list_from_yaml_filtered <- list_from_yaml[parsed_parameters$fields_to_render]
  list_from_yaml_filtered_right <- list_from_yaml[parsed_parameters$fields_right]

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

  enclose_one_time <- FALSE
  enclose <- TRUE
  if (length(parsed_parameters$html_tag) > 1) {
    enclose_one_time <- TRUE
    enclose <- FALSE
  }

  has_name_with_value <- function(lst, name, values) {
    name %in% names(lst) && lst[[name]] %in% values
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

  print_singleton <- function(separator, bullet, indent, css_class, html_tag, is_link, section, bold, image) {
    cat(
      paste0(
        ifelse(idx == 1 & !enclose_one_time, "  \n", ""),
        ifelse(idx != 1, separator, ""),
        ifelse(idx == 1 & bullet, "<ul>", ""),
        ifelse(idx == 1 & section, "  \n## ", ""),
        ifelse(bold, "__", ""),
        render_indent_class(indent, css_class, section = section),
        render_html_tag(html_tag, bullet = bullet, enclose = enclose, section = section),
        title_matched,
        ifelse(is_link, "<", ""),
        ifelse(image, paste0("![](", knitr::include_graphics(item[["value"]], dpi = 800), ")"), item["value"]),
        ifelse(is_link, ">", ""),
        render_html_tag(html_tag, "close", bullet, enclose = enclose, section = section),
        render_indent_class(indent, type = "close", section = section),
        ifelse(bold, "__", ""),
        ifelse(idx == 1 & section, paste0("<a id=\"", tryCatch(item["value"], error = function(e) ""), "\"></a>"), "")
      )
    )
  }

  print_to_right <- function(separator, enclose = FALSE) {
    cat(
      paste0(
        ifelse(!enclose, "<span class=\"go-right\">", ""),
        ifelse(idx != 1, separator, ""),
        title_matched,
        item["value"],
        ifelse(!enclose, "</span>", "")
      )
    )
  }

  cat(
    render_indent_class(indent = indent_one_time, parsed_parameters$css_class, section = parsed_parameters$section)
  )
  cat(
    render_html_tag(parsed_parameters$html_tag,
      bullet = parsed_parameters$bullet,
      enclose = enclose_one_time,
      section = parsed_parameters$section
    )
  )

  for (idx in seq_along(list_from_yaml_filtered)) {
    title_matched <- ""
    if (parsed_parameters$use_field_names) {
      title_or_searched_name <- search_names(list_from_yaml_filtered[idx] |> names(), language, field_names)
      title_matched <- paste0(title_or_searched_name, ": ")
    }

    item <- list_from_yaml_filtered[[idx]]

    if (language %in% names(item)) {
      item["value"] <- item[language]
    }

    if (!isTruthy(item["value"])) next

    profile_check <- has_name_with_value(item, "params_profile", c("general", params_profile))
    location_check <- has_name_with_value(item, "params_location", c("general", params_location))

    if ((profile_check && !"params_location" %in% names(item)) ||
      (location_check && !"params_profile" %in% names(item)) ||
      (profile_check && location_check) ||
      (!"params_profile" %in% names(item) && !"params_location" %in% names(item))) {
      print_singleton(
        parsed_parameters$separator, parsed_parameters$bullet,
        parsed_parameters$indent, parsed_parameters$css_class,
        parsed_parameters$html_tag, parsed_parameters$is_link,
        parsed_parameters$section, parsed_parameters$bold,
        parsed_parameters$image
      )
    } else {
      next
    }
  }

  enclose_right <- FALSE
  if (length(list_from_yaml_filtered_right) > 1) {
    enclose_right <- TRUE
  }
  if (enclose_right) {
    cat("<span class=\"go-right\">")
  }
  for (idx in seq_along(list_from_yaml_filtered_right)) {
    title_matched <- ""
    if (parsed_parameters$use_field_names) {
      title_or_searched_name <- search_names(list_from_yaml_filtered_right[idx] |> names(), language, field_names)
      title_matched <- paste0(title_or_searched_name, ": ")
    }

    item <- list_from_yaml_filtered_right[[idx]]

    if (language %in% names(item)) {
      item["value"] <- item[language]
    }
    if (!isTruthy(item["value"])) next

    profile_check <- has_name_with_value(item, "params_profile", c("general", params_profile))
    location_check <- has_name_with_value(item, "params_location", c("general", params_location))

    if ((profile_check && !"params_location" %in% names(item)) ||
      (location_check && !"params_profile" %in% names(item)) ||
      (profile_check && location_check) ||
      (!"params_profile" %in% names(item) && !"params_location" %in% names(item))) {
      print_to_right(parsed_parameters$separator, enclose_right)
    } else {
      next
    }
  }
  if (enclose_right) {
    cat("</span>")
  }

  cat(
    render_html_tag(parsed_parameters$html_tag,
      type = "close",
      bullet = parsed_parameters$bullet, enclose = enclose_one_time,
      section = parsed_parameters$section
    )
  )
  cat(
    ifelse(parsed_parameters$bullet, "</ul>", "")
  )
  cat(
    render_indent_class(indent = indent_one_time, type = "close", section = parsed_parameters$section)
  )
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

read_yml_and_check_levels <- function(yaml_file) {
  yml_list <- classify_yaml_structure(yaml_file)

  if (attr(yml_list, "yaml_type") == "one_level") {
    yml_list <- map(yml_list, replace_system_vars)
    attr(yml_list, "yaml_type") <- "one_level"
  } else if (attr(yml_list, "yaml_type") == "multi_level") {
    has_year <- map_lgl(yml_list, ~ "year" %in% names(.x))
    if (any(has_year)) {
      sublists_with_year <- yml_list[has_year]
      sublists_without_year <- yml_list[!has_year]
      sorted_indices <- order(map_int(sublists_with_year, ~ .x$year[[1]]), decreasing = TRUE)
      sorted_sublists_with_year <- sublists_with_year[sorted_indices]
      sorted_list <- c(sorted_sublists_with_year, sublists_without_year)
      return(sorted_list)
    }
  }

  yml_list
}

render_title <- function(list_from_yml, chapter_name) {
  render_bool <- check_for_meaningful_values(list_from_yml)

  if (render_bool) {
    section_title <- search_names(chapter_name, language, chapters_names)
    cat(paste0("<br>   \n  \n# ", section_title))
  }
}
