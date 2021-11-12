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
# states <- list(`East Coast` = list("NY", "NJ", "CT"),
#                `West Coast` = list("WA", "OR", "CA"),
#                `Midwest` = list("MN", "WI", "IA"))
# states <- HTMLgroupedChoices(
#   values = list(list("NY", "NJ", "CT"), list("WA", "OR", "CA"), list("MN", "WI", "IA")),
#   names = list(tags$span("hello"), tags$span("hi", style="color:red"), tags$h2("BIG", fa_i("drum")))
# )
katex <- HTMLchoices(
  values = list("alpha", "beta", "gammma"),
  names = list("\\alpha", "\\\\beta", "\\\\\\gamma")
)

styles <- list(
  borderBottom = "5px solid orange",
  color = list(selected = "lime", otherwise = "pink"),
  backgroundColor = list(selected = "cyan", focused = "yellow", otherwise = "seashell")
)

ui <- fluidPage(
  theme = bs_theme(version = 4),
  tags$head(
    tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/katex@0.15.1/dist/katex.min.css", integrity="sha384-R4558gYOUz8mP9YWpZJjofhk+zx0AS11p36HnD2ZKj/6JR5z27gSSULCNHIRReVs", crossorigin="anonymous"),
    tags$script(defer="", src="https://cdn.jsdelivr.net/npm/katex@0.15.1/dist/katex.min.js", integrity="sha384-z1fJDqw8ZApjGO3/unPWUPsIymfsJmyrDVWC8Tv/a1HeOtGmkwNd/7xUS0Xcnvsx", crossorigin="anonymous")
  ),
  titlePanel("reactR Input Example"),
  selectControlInput(
    "inputid", label = tags$h1("Make a choice", style="color: red;"),
    styles = styles,
    choices = katex,
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
