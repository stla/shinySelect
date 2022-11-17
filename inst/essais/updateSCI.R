library(shiny)
library(shinySelect)
library(bslib)


choices <- HTMLchoices(
  values = list("alpha", "beta", "gamma"),
  labels = list(katex("\\alpha"), katex("\\beta"), katex("\\gamma"))
)

choices2 <- HTMLchoices(
  values = list("delta", "beta", "gamma"),
  labels = list(katex("\\delta"), katex("\\beta"), katex("\\gamma"))
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
    multiple = TRUE,
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
      selected = "gamma"
    )
  })
}

shinyApp(ui, server)
