library(shiny)
library(shinySelect)
library(bslib)

choices <- HTMLchoices(
  values = list(
    "Euler equality",
    "Gaussian integral",
    "Knopp equality"
  ),
  labels = list(
    katex("\\frac{\\pi}{2} = 1 + \\frac{1}{3} + \\frac{1\\times 2}{3\\times 5} + \\cdots + \\frac{n!}{3\\times 5 \\times 7 \\times \\cdots \\times (2n+1)} + \\cdots"),
    katex("\\int_{-\\infty}^{+\\infty}e^{-x^2}dx=\\sqrt{\\pi}"),
    katex("\\frac{\\pi^2}{16}=\\sum_{k=0}^\\infty\\frac{{(-1)}^k}{k+1}\\left(1+\\frac{1}{3}+\\cdots+\\frac{1}{2k+1}\\right)")
  )
)

ui <- fluidPage(
  theme = bs_theme(version = 4),
  titlePanel("KaTeX example"),
  selectControlInput(
    "select",
    label = tags$h2("Choose a formula", style="color: firebrick;"),
    choices = choices,
    selected = "Euler equality",
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
}

shinyApp(ui, server)
