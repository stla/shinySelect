library(shiny)
library(shinySelect)
library(bslib)


choices <- list(
  Greek = list("alpha", "beta", "gamma"),
  French = list("A", "B", "G")
)

shinySelect:::process_choices_selected(choices, list("alpha", "G"))

choices2 <- list(
  Greek = list("delta", "beta", "gamma"),
  French = list("D", "B", "G")
)

ui <- fluidPage(
  theme = bs_theme(version = 4),
  titlePanel("KaTeX example"),
  actionButton("update", "Update"),
  selectControlInput(
    "select",
    label = tags$h1("Make a choice", style="color: red;"),
    choices = choices,
    selected = list("alpha"),
    multiple = FALSE,
    animated = TRUE
  ),
  br(),
  verbatimTextOutput("textOutput")
)

server <- function(input, output, session) {
  output[["textOutput"]] <- renderPrint({
    sprintf("You selected: %s.", input[["select"]])
  })
  observeEvent(input[["update"]], {
    updateSelectControlInput(
      session, "select", 
      choices = NULL,
      selected = list("B")
    )
  })
}

shinyApp(ui, server)
