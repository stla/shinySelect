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
  mapply(function(x, y){tags$span(x, `data-toggle`="tooltip", title=y)}, countries, pop,
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

CSS <- '
.tooltip {
  pointer-events: none;
}
.tooltip > .tooltip-inner {
  pointer-events: none;
  background-color: #73AD21;
  color: #FFFFFF;
  border: 1px solid green;
  padding: 5px;
  font-size: 15px;
  text-align: justify;
  margin-left: 10px;
  max-width: 1000px;
}
.tooltip > .arrow::before {
  border-top-color: #73AD21;
}
'

ui <- fluidPage(
  theme = bs_theme(version = 4),
  tags$head(
    tags$style(HTML(CSS))
  ),
  titlePanel("reactR Input Example"),
  sidebarLayout(
    sidebarPanel(
      selectControlInput(
        "inputid", label = tags$h1("Make a choice", style="color: red;"),
        containerClass = NULL,
        #        styles = styles,
        choices = countries,
        selected = "Tonga",
        multiple = TRUE,
        animated = TRUE,
        ignoreCaseOnFilter = FALSE
      ),
      verbatimTextOutput("textOutput")
    ),
    mainPanel()
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
