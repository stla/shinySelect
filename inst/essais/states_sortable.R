library(shiny)
library(shinySelect)
library(bslib)
library(fontawesome)

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


styles <- list(
  borderBottom = "5px solid orange",
  color = list(selected = "lime", otherwise = "pink"),
  backgroundColor = list(selected = "cyan", otherwise = "seashell")
)
controlStyles = list(
  marginTop = "0"
)
multiValueStyles = list(
  backgroundColor = "orange"
)
multiValueLabelStyles = list(
  fontStyle = "italic"
)
multiValueRemoveStyles = list(
  color = "hotpink",
  ":hover" = list(
    backgroundColor = "navy",
    color = "white"
  )
)

# CSS <- '
# div[class$="-menu"][id^="react-select"] {position: static}
# div[class$="-control"] + div {position: static}'

ui <- fluidPage(
  theme = bs_theme(version = 4),
  # tags$head(
  #   tags$style(HTML(CSS))
  # ),
  titlePanel("reactR Input Example"),
  selectControlInput(
    "inputid", label = tags$h1("Make a choice", style="color: red;"),
    containerClass = NULL,
    optionsStyles = styles,
    controlStyles = controlStyles,
    multiValueStyles = multiValueStyles,
    multiValueLabelStyles = multiValueLabelStyles,
    multiValueRemoveStyles = multiValueRemoveStyles,
    choices = states,
    selected = list("NY", "CT"),
    multiple = TRUE,
    sortable = TRUE,
    animated = TRUE,
    ignoreCaseOnFilter = FALSE
  ),
  verbatimTextOutput("textOutput"),
  actionButton("toggle", "Toggle menu")
)

server <- function(input, output, session) {
  observe({
    print(input$inputid)
  })
  output$textOutput <- renderPrint({
    sprintf("You selected: %s", input$inputid)
  })
  observeEvent(input[["toggle"]], {
    toggleMenu(session, "inputid")
  })

}

shinyApp(ui, server)
