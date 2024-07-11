box::use(
  shiny[bootstrapPage, div, moduleServer, hr, NS, renderUI, tags, uiOutput, tagList, span, img, icon, reactive, observe, HTML],
  shinydashboard[dashboardBody, sidebarMenuOutput, renderMenu, sidebarMenu, tabItem, tabItems, menuItem],
  shinydashboardPlus[dashboardPage, dashboardHeader, dashboardSidebar, box]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    title = "CV_yml",
    dashboardHeader(
      title = tagList(
        span(class = "logo-lg", "CV_yml"),
        img(src = "static/logo.png", width = "25")
      ),
      fixed = TRUE
    ),
    dashboardSidebar(
      id = "sidebar_id",
      sidebarMenuOutput(ns("dynamic_menu")),
      hr()
    ),
    dashboardBody(
      uiOutput(ns("tabItms"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pages <- reactive({
      list(
        list(text = "page 1", tabName = "page1", icon = "briefcase", content = "This is page 1 content"),
        list(text = "page 2", tabName = "page2", icon = "cubes", content = "This is page 2 content"),
        list(text = "page 3", tabName = "page3", icon = "box", content = "This is page 3 content"),
        list(text = "page 4", tabName = "page4", icon = "cubes", content = "This is page 4 content")
      )
    })

    # Generate the dynamic menu items
    output$dynamic_menu <- renderMenu({
      sidebarMenu(
        lapply(pages(), function(page) {
          menuItem(
            text = page$text,
            tabName = page$tabName,
            icon = icon(page$icon)
          )
        })
      )
    })

    # Generate the dynamic tab items
    output$tabItms <- renderUI({
      itemsDyn <- lapply(pages(), function(name) {
        tabItem(tabName = name$tabName, uiOutput(ns(name$tabName)))
      })

      items <- c(
        itemsDyn
      )
      do.call(tabItems, items)
    })

    observe({
      lapply(pages(), function(name) {
        output[[name$tabName]] <- renderUI({
          box("Results Box", uiOutput(ns(paste0("plot_", name$text))))
        })

        output[[paste0("plot_", name$text)]] <- renderUI({
          HTML(name$content)
        })
      })
    })
  })
}
