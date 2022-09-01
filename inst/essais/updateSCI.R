library(shiny)
library(shinySelect)
library(bslib)


choices <- HTMLchoices(
  values = list("alpha", "beta", "gamma"),
  labels = list(katex("\\alpha"), katex("\\beta"), katex("\\gamma"))
)

ui <- fluidPage(
  theme = bs_theme(version = 4),
  titlePanel("KaTeX example"),
  actionButton("update", "Update"),
  selectControlInput(
    "select",
    label = tags$h1("Make a choice", style="color: red;"),
    choices = choices,
    selected = "alpha",
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
    updateSelectControlInput(session, "select", selected = c("alpha", "beta"))
  })
}

shinyApp(ui, server)
