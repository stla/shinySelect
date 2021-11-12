library(shiny)
library(shinySelect)
library(bslib)
library(fontawesome)

flavours <- list(
  Chocolate = "chocolate",
  Vanilla = "vanilla",
  Strawberry = "strawberry"
)
# flavours <- HTMLchoices(
#   values = c("choco", "van", "straw"),
#   names = list(tags$span("hello"), tags$span("hi", style="color:red"), tags$h2("BIG", fa_i("drum")))
# )
states <- list(`East Coast` = list("NY", "NJ", "CT"),
               `West Coast` = list("WA", "OR", "CA"),
               `Midwest` = list("MN", "WI", "IA"))
# states <- HTMLgroupedChoices(
#   values = list(list("NY", "NJ", "CT"), list("WA", "OR", "CA"), list("MN", "WI", "IA")),
#   names = list(tags$span("hello"), tags$span("hi", style="color:red"), tags$h2("BIG", fa_i("drum")))
# )
states <- HTMLgroupedChoices(
  groups = lapply(list("East Coast", "West Coast", "Midwest"), tags$h2),
  labels = list(
    lapply(list("NY", "NJ", "CT"), function(x) tags$span(x, style="color:red")),
    lapply(list("WA", "OR", "CA"), function(x) tags$span(x, style="color:green")),
    lapply(list("MN", "WI", "IA"), function(x) tags$span(x, style="color:blue"))
  ),
  values = list(
    list("NY", "NJ", "CT"),
    list("WA", "OR", "CA"),
    list("MN", "WI", "IA")
  )
)

food <- HTMLchoices(
  labels = list(
    tags$span(fa_i("hamburger"), "Hamburger"),
    tags$span(fa_i("pizza-slice"), "Pizza"),
    tags$span(fa_i("fish"), "Fish")
  ),
  values = list("hamburger", "pizza", "fish")
)

styles <- list(
  borderBottom = "5px solid orange",
  color = list(selected = "lime", otherwise = "pink"),
  backgroundColor = list(selected = "cyan", otherwise = "seashell")
)

ui <- fluidPage(
  theme = bs_theme(version = 4),
  titlePanel("reactR Input Example"),
  selectControlInput(
    "inputid", label = tags$h1("Make a choice", style="color: red;"),
    styles = styles,
    choices = food,
    selected = "hamburger",
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
