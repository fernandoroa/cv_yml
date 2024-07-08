require(yaml)

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
