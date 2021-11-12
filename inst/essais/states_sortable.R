library(shiny)
library(shinySelect)
library(bslib)
library(fontawesome)

states <- HTMLgroupedChoices(
  groups = lapply(list("East Coast", "West Coast", "Midwest"), function(x){
    tags$h2(x, style="text-decoration: underline")
  }),
  labels = list(
    lapply(list("NY", "NJ", "CT"), function(x){
      tags$span(HTML("&bull;"), x, style="color: red")
    }),
    lapply(list("WA", "OR", "CA"), function(x){
      tags$span(HTML("&bull;"), x, style="color: green")
    }),
    lapply(list("MN", "WI", "IA"), function(x){
      tags$span(HTML("&bull;"), x, style="color: blue")
    })
  ),
  values = list(
    list("NY", "NJ", "CT"),
    list("WA", "OR", "CA"),
    list("MN", "WI", "IA")
  )
)


styles <- list(
  borderBottom = "2px dotted orange",
  backgroundColor = list(
    selected = "cyan",
    focused = "lemonchiffon",
    otherwise = "seashell"
  )
)
controlStyles = list(
  marginTop = "0",
  boxShadow = toString(c(
    "rgba(50, 50, 93, 0.25) 0px 50px 100px -20px",
    "rgba(0, 0, 0, 0.3) 0px 30px 60px -30px",
    "rgba(10, 37, 64, 0.35) 0px -2px 6px 0px inset;"
  ))
)
multiValueStyles = list(
  backgroundColor = "lavenderblush"
)
multiValueLabelStyles = list(
  fontStyle = "italic",
  fontWeight = "bold"
)
multiValueRemoveStyles = list(
  color = "hotpink",
  ":hover" = list(
    backgroundColor = "navy",
    color = "white"
  )
)

CSS <- '
div[class$="-group"][id^="react-select"][id$="-heading"] {
  background-color: #671227
}'

ui <- fluidPage(
  theme = bs_theme(version = 4),
  tags$head(
    tags$style(HTML(CSS))
  ),
  titlePanel("reactR Input Example"),
  selectControlInput(
    "inputid", label = tags$h1("Make a choice", style="color: red;"),
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
  br(),
  verbatimTextOutput("textOutput"),
  actionButton("toggle", "Toggle menu")
)

server <- function(input, output, session) {
  observe({
    print(input$inputid)
  })
  output$textOutput <- renderPrint({
    sprintf("You selected: %s", toString(input$inputid))
  })
  observeEvent(input[["toggle"]], {
    toggleMenu(session, "inputid")
  })

}

shinyApp(ui, server)
