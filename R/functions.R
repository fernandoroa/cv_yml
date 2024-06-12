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

render_singletons <- function(
    list_from_yaml, fields_to_render, field_titles,
    separator = "<br>", profile = "general", is_link = FALSE, location = "general") {
  list_from_yaml_filtered <- list_from_yaml[fields_to_render]

  has_name_with_value <- function(lst, name, values) {
    name %in% names(lst) && lst[[name]] %in% values
  }

  print_singleton <- function() {
    cat(
      paste0(
        ifelse(idx == 1, "   \n", separator),
        "<span>",
        title_matched, ": ",
        ifelse(is_link, "<", ""),
        item["value"],
        ifelse(is_link, ">", ""),
        "</span>"
      )
    )
  }

  for (idx in seq_along(list_from_yaml_filtered)) {
    title_matched <- search_names(list_from_yaml_filtered[idx] |> names(), language, field_titles)

    item <- list_from_yaml_filtered[[idx]]
    if (!isTruthy(item["value"])) next

    profile_check <- has_name_with_value(item, "profile", c("general", profile))
    location_check <- has_name_with_value(item, "location", c("general", location))

    if ((profile_check && !"location" %in% names(item)) ||
      (location_check && !"profile" %in% names(item)) ||
      (profile_check && location_check) ||
      (!"profile" %in% names(item) && !"location" %in% names(item))) {
      print_singleton()
    }
  }
}

check_for_meaningful_values <- function(input_list) {
  has_valid_key <- input_list %>%
    map_lgl(~ any(!names(.x) %in% c("profile", "location") & !is.na(.x))) |>
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
