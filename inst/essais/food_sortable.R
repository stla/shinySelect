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
  backgroundColor = list(
    selected = "cyan",
    focused = "lemonchiffon",
    otherwise = "seashell"
  )
)

ui <- fluidPage(
  theme = bs_theme(version = 4),
  titlePanel("Sortable example"),
  selectControlInput(
    "select",
    label = tags$h2("What do you want to eat?", style="color: darkred;"),
    optionsStyles = styles,
    choices = food,
    selected = "hamburger",
    multiple = TRUE,
    sortable = TRUE,
    animated = TRUE,
    ignoreCaseOnFilter = FALSE
  ),
  br(),
  verbatimTextOutput("textOutput")
)

server <- function(input, output, session) {
  output[["textOutput"]] <- renderPrint({
    sprintf("You selected: %s.", toString(input[["select"]]))
  })
}

shinyApp(ui, server)
