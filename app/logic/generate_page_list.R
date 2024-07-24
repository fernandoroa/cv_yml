#' @export
generate_page_list <- function(pages, icon = "box") {
  lapply(seq_along(pages), function(i) {
    list(
      text = pages[i],
      tabName = pages[i],
      icon = icon,
      content = paste("This is page", i, "content")
    )
  })
}
