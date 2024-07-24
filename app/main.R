box::use(
  shiny[moduleServer, hr, NS, renderUI, tags, uiOutput, tagList, span, img, icon, reactive, observe, HTML, tabPanel, tabsetPanel],
  shinydashboard[dashboardBody, sidebarMenuOutput, renderMenu, sidebarMenu, tabItem, tabItems, menuItem, menuSubItem],
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

    main_files_path <- "../curriculumpu/custom/yml"
    yml_files_main_c_vec <- list.files(main_files_path)

    dictionaries_path <- "../curriculumpu/custom/yml/dictionaries"
    yml_files_dictionaries_c_vec <- list.files(dictionaries_path)

    config_path <- "../curriculumpu/custom/yml/config"
    yml_files_chapters_config_c_vec <- list.files(config_path)

    data_path <- "../curriculumpu/custom/yml/data"
    yml_files_chapters_data_c_vec <- list.files(data_path)

    rv_temp_folder_session_config <- reactive({
      config_path
    })

    rv_temp_folder_session_data <- reactive({
      data_path
    })

    rv_temp_folder_session_dictionaries <- reactive({
      dictionaries_path
    })

    chapter_names_c_vec <- map_vec(yml_files_chapters_config_c_vec, \(x) file_path_sans_ext(x) |> basename())
    dictionary_names_c_vec <- map_vec(yml_files_dictionaries_c_vec, \(x) file_path_sans_ext(x) |> basename())

    chapters_chr_vec <- reactive({
      page_list <- generate_page_list(chapter_names_c_vec)
    })

    dictionaries_chr_vec <- reactive({
      page_list <- generate_page_list(dictionary_names_c_vec, icon = "file-lines")
    })

    # Generate the dynamic menu items
    output$dynamic_menu <- renderMenu({
      sidebarMenu(
        menuItem("Dictionaries",
          tabname = "dictionaries", icon = icon("bars"),
          startExpanded = TRUE,
          lapply(dictionaries_chr_vec(), function(dictionary) {
            menuSubItem(
              text = dictionary$text,
              tabName = dictionary$tabName,
              icon = icon(dictionary$icon)
            )
          })
        ),
        menuItem("Chapters",
          tabname = "chapters", icon = icon("bars"),
          startExpanded = TRUE,
          lapply(chapters_chr_vec(), function(chapter) {
            menuSubItem(
              text = chapter$text,
              tabName = chapter$tabName,
              icon = icon(chapter$icon)
            )
          })
        )
      )
    })

    # Generate the dynamic tab items
    output$tabItms <- renderUI({
      dictionary_tabItems <- lapply(dictionaries_chr_vec(), function(dictionary) {
        tabItem(tabName = dictionary$tabName, uiOutput(ns(dictionary$tabName)))
      })

      chapter_tabItems <- lapply(chapters_chr_vec(), function(chapter) {
        tabItem(tabName = chapter$tabName, uiOutput(ns(chapter$tabName)))
      })

      items <- c(
        dictionary_tabItems,
        chapter_tabItems
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

        yml_ace$server(paste0("yml_ace_config", chapter$text), rv_temp_folder_session_config, filename_no_ext = chapter$tabName)
        yml_ace$server(paste0("yml_ace_data", chapter$text), rv_temp_folder_session_data, filename_no_ext = chapter$tabName)
      })

      lapply(dictionaries_chr_vec(), function(dictionary) {
        output[[dictionary$tabName]] <- renderUI({
          tabsetPanel(
            tabPanel(
              "File",
              yml_ace$ui(ns(paste0("yml_ace_dictionary", dictionary$text)))
            )
          )
        })

        yml_ace$server(paste0("yml_ace_dictionary", dictionary$text),
          rv_temp_folder_session_dictionaries,
          filename_no_ext = dictionary$tabName
        )
      })
    })
  })
}
