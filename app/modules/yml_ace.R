box::use(
  shiny[...],
  shinyAce[aceEditor, updateAceEditor]
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
            mode = "yml",
            placeholder = ".yml not loaded",
            value = paste0(readLines(file.path(temp_folder_session(), paste0(chapter_name, ".yml")), warn = FALSE)),
            autoScrollEditorIntoView = TRUE,
            minLines = 20,
            maxLines = 60
          )
        )
      )
    })

    observe({
      updateAceEditor(session, "ace",
        value = paste0(readLines(file.path(temp_folder_session(), paste0(chapter_name, ".yml")), warn = FALSE),
          collapse = "\n"
        )
      )
    })

    outputOptions(output, "ace_ui", suspendWhenHidden = FALSE)
  })
}
