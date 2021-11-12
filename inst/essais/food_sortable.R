library(shiny)
library(shinySelect)
library(bslib)
library(fontawesome)


food <- HTMLchoices(
  labels = list(
    tags$span(fa_i("hamburger"), "Hamburger"),
    tags$span(fa_i("pizza-slice"), "Pizza"),
    tags$span(fa_i("fish"), "Fish")
  ),
  values = list("hamburger", "pizza", "fish")
)

styles <- list(
  borderBottom = "2px solid orange",
  #color = list(selected = "lime", otherwise = "pink"),
  backgroundColor = list(selected = "cyan", focused = "lemonchiffon", otherwise = "seashell")
)

ui <- fluidPage(
  theme = bs_theme(version = 4),
  titlePanel("reactR Input Example"),
  selectControlInput(
    "inputid", label = tags$h1("Make a choice", style="color: red;"),
    optionsStyles = styles,
    choices = food,
    selected = "hamburger",
    multiple = TRUE,
    sortable = TRUE,
    animated = TRUE,
    ignoreCaseOnFilter = FALSE
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
