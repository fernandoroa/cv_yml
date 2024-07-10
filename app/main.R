box::use(
  shiny[bootstrapPage, div, moduleServer, hr, NS, renderUI, tags, uiOutput, tagList, span, img, icon],
  shinydashboard[...],
  shinydashboardPlus[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    dashboardHeader(
      title = tagList(
        span(class = "logo-lg", "CV_yml"),
        img(src = "static/logo.png", width = "25")
      ),
      fixed = TRUE
    ),
    dashboardSidebar(
      id = "mysidebar",
      sidebarMenu(
        menuItem(
          text = "page",
          tabName = "boxes",
          badgeColor = "green",
          icon = icon("briefcase")
        ),
        menuItem(
          text = "page",
          tabName = "buttons",
          badgeColor = "green",
          icon = icon("cubes")
        ),
        menuItem(
          text = "page",
          tabName = "boxelements",
          badgeColor = "green",
          icon = icon("table-cells")
        ),
        menuItem(
          text = "page",
          tabName = "extraelements",
          badgeColor = "green",
          icon = icon("circle-plus")
        )
      ),
      hr()
    ),
    dashboardBody(
      tags$head(
        tags$script(
          "$(function() {
            $('.sidebar-toggle').on('click', function() {
              $('.skinSelector-widget').toggle();
            });
          });
          "
        )
      ),
      tabItems()
    ),
    title = "CV_yml"
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
