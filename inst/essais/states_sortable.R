library(shiny)
library(shinySelect)

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
  marginRight = "50px",
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
  background: #0F2027;  /* fallback for old browsers */
  background: -webkit-linear-gradient(to right, #2C5364, #203A43, #0F2027);
  background: linear-gradient(to right, #2C5364, #203A43, #0F2027);
}'

ui <- fluidPage(
  tags$head(
    tags$style(HTML(CSS))
  ),
  titlePanel("Custom styles example"),
  splitLayout(
    selectControlInput(
      "select",
      label = tags$h1("Choose some states", style="color: red;"),
      containerClass = NULL,
      optionsStyles = styles,
      controlStyles = controlStyles,
      multiValueStyles = multiValueStyles,
      multiValueLabelStyles = multiValueLabelStyles,
      multiValueRemoveStyles = multiValueRemoveStyles,
      choices = states,
      selected = "CT",
      multiple = FALSE,
      sortable = TRUE,
      animated = TRUE
    ),
    tagList(
      verbatimTextOutput("textOutput"),
      br(),
      actionButton("toggle", "Toggle menu", class = "btn-primary"),
      actionButton("update", "Update"),
      HTML(
        '<div class="alert alert-warning alert-dismissible fade show" role="alert">
  <strong>Holy guacamole!</strong> You should check in on some of those fields below.
  <button type="button" class="close" data-dismiss="alert" aria-label="Close">
    <span aria-hidden="true">&times;</span>
  </button>
</div>'
      )
    )
  )
)

server <- function(input, output, session) {
  output[["textOutput"]] <- renderPrint({
    sprintf("You selected: %s.", toString(input[["select"]]))
  })
  observeEvent(input[["toggle"]], {
    toggleMenu(session, "select")
  })
  observeEvent(input[["update"]], {
    updateSelectControlInput(
      session, "select",
      label = "hello", choices = states, selected = c("WI", "IA"))
  })
}

shinyApp(ui, server)
