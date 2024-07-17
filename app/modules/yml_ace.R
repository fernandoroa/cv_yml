box::use(
  shiny[...],
  shinyAce[aceEditor, updateAceEditor]
)

box::use(
  .. / logic / get_file_content[...],
)

ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ace_ui"))
}

server <- function(id, temp_folder_session, chapter_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$ace_ui <- renderUI({
      tagList(
        column(
          7,
          aceEditor(
            outputId = ns("ace"),
            selectionId = "selection",
            mode = "yaml",
            placeholder = ".yml not loaded",
            value = get_file_content(file.path(temp_folder_session(), paste0(chapter_name, ".yml"))),
            autoScrollEditorIntoView = TRUE,
            minLines = 20,
            maxLines = 60
          )
        )
      )
    })



    observe({
      updateAceEditor(session, "ace",
        value = get_file_content(file.path(temp_folder_session(), paste0(chapter_name, ".yml")))
      )
    })

    outputOptions(output, "ace_ui", suspendWhenHidden = FALSE)
  })
}
