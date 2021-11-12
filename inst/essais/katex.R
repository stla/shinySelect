library(shiny)
library(shinySelect)
library(bslib)
library(fontawesome)


choices <- HTMLchoices(
  values = list("alpha", "beta", "gammma"),
  labels = list(katex("\\alpha"), katex("\\beta"), katex("\\gamma"))
)


ui <- fluidPage(
  theme = bs_theme(version = 4),
  # tags$head(
  #   tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/katex@0.15.1/dist/katex.min.css", integrity="sha384-R4558gYOUz8mP9YWpZJjofhk+zx0AS11p36HnD2ZKj/6JR5z27gSSULCNHIRReVs", crossorigin="anonymous"),
  #   tags$script(defer="", src="https://cdn.jsdelivr.net/npm/katex@0.15.1/dist/katex.min.js", integrity="sha384-z1fJDqw8ZApjGO3/unPWUPsIymfsJmyrDVWC8Tv/a1HeOtGmkwNd/7xUS0Xcnvsx", crossorigin="anonymous")
  # ),
  titlePanel("reactR Input Example"),
  selectControlInput(
    "inputid", label = tags$h1("Make a choice", style="color: red;"),
    choices = choices,
    selected = "alpha",
    multiple = FALSE,
    animated = TRUE
  ),
  verbatimTextOutput("textOutput")
)

server <- function(input, output, session) {
  observe({
    print(input$inputid)
  })
  output$textOutput <- renderPrint({
    sprintf("You selected: %s", input$inputid)
  })
}

shinyApp(ui, server)
