library(shiny)
library(shinySelect)
library(bslib)
library(fontawesome)

data(Countries, package = "jsTreeR")
continents <- unique(Countries[["continentName"]])
L <- lapply(continents, function(continent){
  indices <- Countries[["continentName"]] == continent
  countries <- Countries[["countryName"]][indices]
  pop <- Countries[["population"]][indices]
  mapply(function(x, y){tags$span(x, title=y)}, countries, pop,
         SIMPLIFY = FALSE, USE.NAMES = FALSE)
})
countries <- lapply(continents, function(continent){
  indices <- Countries[["continentName"]] == continent
  Countries[["countryName"]][indices]
})
countries <- HTMLgroupedChoices(
  groups = lapply(continents, function(nm) tags$h2(nm, style="color: blue;")),
  labels = L,
  values = countries
)

styles <- list(
  backgroundColor = list(selected = "cyan", focused = "orange", otherwise = "seashell")
)

ui <- fluidPage(
  theme = bs_theme(version = 4),
  titlePanel("reactR Input Example"),
    wellPanel(
      style="width:100%",
      selectControlInput(
        "inputid", label = tags$h1("Make a choice", style="color: red;"),
#        styles = styles,
        choices = countries,
        selected = "Tonga",
        multiple = TRUE,
        animated = TRUE,
        ignoreCaseOnFilter = FALSE
      ),
      verbatimTextOutput("textOutput")
    )
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
