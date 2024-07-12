box::use(
  shiny[bootstrapPage, div, moduleServer, hr, NS, renderUI, tags, uiOutput, tagList, span, img, icon, reactive, observe, HTML],
  shinydashboard[dashboardBody, sidebarMenuOutput, renderMenu, sidebarMenu, tabItem, tabItems, menuItem],
  shinydashboardPlus[dashboardPage, dashboardHeader, dashboardSidebar, box],
  tools[file_path_sans_ext],
  purrr[map_vec]
)

box::use(
  logic / generate_page_list[...],
  modules / yml_ace
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

    config_path <- "../curriculumpu/custom/yml/config"
    yml_files_chapters_c_vec <- list.files(config_path)

    rv_temp_folder_session <- reactive({
      config_path
    })

    chapter_names_c_vec <- map_vec(yml_files_chapters_c_vec, \(x) file_path_sans_ext(x) |> basename())

    pages <- reactive({
      page_list <- generate_page_list(chapter_names_c_vec)
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
      itemsDyn <- lapply(pages(), function(page) {
        tabItem(tabName = page$tabName, uiOutput(ns(page$tabName)))
      })

      items <- c(
        itemsDyn
      )
      do.call(tabItems, items)
    })

    observe({
      lapply(pages(), function(page) {
        output[[page$tabName]] <- renderUI({
          box("config", yml_ace$ui(ns(paste0("yml_ace_ns", page$text))))
        })

        yml_ace$server(paste0("yml_ace_ns", page$text), rv_temp_folder_session, chapter_name = page$tabName)
      })
    })
  })
}
