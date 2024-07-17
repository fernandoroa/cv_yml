box::use(
  shiny[moduleServer, hr, NS, renderUI, tags, uiOutput, tagList, span, img, icon, reactive, observe, HTML, tabPanel, tabsetPanel],
  shinydashboard[dashboardBody, sidebarMenuOutput, renderMenu, sidebarMenu, tabItem, tabItems, menuItem],
  shinydashboardPlus[dashboardPage, dashboardHeader, dashboardSidebar],
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
      fixed = FALSE
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

    data_path <- "../curriculumpu/custom/yml/data"
    yml_files_chapters_data_c_vec <- list.files(data_path)

    rv_temp_folder_session <- reactive({
      config_path
    })

    rv_temp_folder_session_data <- reactive({
      data_path
    })

    chapter_names_c_vec <- map_vec(yml_files_chapters_c_vec, \(x) file_path_sans_ext(x) |> basename())

    chapters_chr_vec <- reactive({
      page_list <- generate_page_list(chapter_names_c_vec)
    })

    # Generate the dynamic menu items
    output$dynamic_menu <- renderMenu({
      sidebarMenu(
        lapply(chapters_chr_vec(), function(chapter) {
          menuItem(
            text = chapter$text,
            tabName = chapter$tabName,
            icon = icon(chapter$icon)
          )
        })
      )
    })

    # Generate the dynamic tab items
    output$tabItms <- renderUI({
      itemsDyn <- lapply(chapters_chr_vec(), function(chapter) {
        tabItem(tabName = chapter$tabName, uiOutput(ns(chapter$tabName)))
      })

      items <- c(
        itemsDyn
      )
      do.call(tabItems, items)
    })

    observe({
      lapply(chapters_chr_vec(), function(chapter) {
        output[[chapter$tabName]] <- renderUI({
          tabsetPanel(
            tabPanel(
              "Config",
              yml_ace$ui(ns(paste0("yml_ace_config", chapter$text)))
            ),
            tabPanel(
              "Data",
              yml_ace$ui(ns(paste0("yml_ace_data", chapter$text)))
            )
          )
        })

        yml_ace$server(paste0("yml_ace_config", chapter$text), rv_temp_folder_session, chapter_name = chapter$tabName)
        yml_ace$server(paste0("yml_ace_data", chapter$text), rv_temp_folder_session_data, chapter_name = chapter$tabName)
      })
    })
  })
}
