#' @export
generate_page_list <- function(pages) {
  lapply(seq_along(pages), function(i) {
    list(
      text = pages[i],
      tabName = pages[i],
      icon = "box",
      content = paste("This is page", i, "content")
    )
  })
}
