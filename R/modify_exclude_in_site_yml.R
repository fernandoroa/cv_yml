require(yaml)

set_nested_value <- function(list, keys, value) {
  if (length(keys) == 1) {
    list[[keys]] <- value
  } else {
    if (!keys[[1]] %in% names(list)) {
      list[[keys[[1]]]] <- list()
    }
    list[[keys[[1]]]] <- set_nested_value(list[[keys[[1]]]], keys[-1], value)
  }
  return(list)
}

add_name_to_yml <- function(yml_path, key, name) {
  yml_content <- read_yaml(yml_path)
  keys <- unlist(strsplit(key, "\\."))

  yml_content <- set_nested_value(yml_content, keys, name)

  write_yaml(yml_content, yml_path)
}


add_to_exclude <- function(yml_path, folder_name) {
  yml_content <- read_yaml(yml_path)

  if (!"exclude" %in% names(yml_content)) {
    yml_content$exclude <- list()
  }

  if (!(folder_name %in% yml_content$exclude)) {
    yml_content$exclude <- c(yml_content$exclude, folder_name)
  }

  write_yaml(yml_content, yml_path)
}

remove_from_exclude <- function(yml_path, folder_name) {
  yml_content <- read_yaml(yml_path)

  if ("exclude" %in% names(yml_content)) {
    yml_content$exclude <- yml_content$exclude[yml_content$exclude != folder_name]
  }

  write_yaml(yml_content, yml_path)
}
