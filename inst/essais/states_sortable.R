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
      selected = list("NY", "CT"),
      multiple = TRUE,
      sortable = TRUE,
      animated = TRUE
    ),
    tagList(
      verbatimTextOutput("textOutput"),
      br(),
      actionButton("toggle", "Toggle menu", class = "btn-primary"),
      actionButton("update", "Update")
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
    #session$sendCustomMessage("update_select", list(sortable = FALSE))
    #config = jsonlite::fromJSON('{"shinyId":"select","containerClass":null,"label":{"__html":"%3Ch1%20style=%22color:%20red;%22%3EChoose%20some%20XXXXX%3C/h1%3E"},"optionsStyles":{"borderBottom":"2px dotted orange","backgroundColor":{"selected":"cyan","focused":"lemonchiffon","otherwise":"seashell"}},"controlStyles":{"marginTop":"0","marginRight":"50px","boxShadow":"rgba(50, 50, 93, 0.25) 0px 50px 100px -20px, rgba(0, 0, 0, 0.3) 0px 30px 60px -30px, rgba(10, 37, 64, 0.35) 0px -2px 6px 0px inset;"},"multiValueStyles":{"backgroundColor":"lavenderblush"},"multiValueLabelStyles":{"fontStyle":"italic","fontWeight":"bold"},"multiValueRemoveStyles":{"color":"hotpink",":hover":{"backgroundColor":"navy","color":"white"}},"grouped":true,"isMulti":true,"sortable":true,"animated":true,"options":[{"label":"1","options":[{"value":"NY","label":"NY"},{"value":"NJ","label":"NJ"},{"value":"CT","label":"CT"}]},{"label":"2","options":[{"value":"WA","label":"WA"},{"value":"OR","label":"OR"},{"value":"CA","label":"CA"}]},{"label":"3","options":[{"value":"MN","label":"MN"},{"value":"WI","label":"WI"},{"value":"IA","label":"IA"}]}],"htmlGroups":["%3Ch2%20style=%22text-decoration:%20underline%22%3EEast%20Coast%3C/h2%3E","%3Ch2%20style=%22text-decoration:%20underline%22%3EWest%20Coast%3C/h2%3E","%3Ch2%20style=%22text-decoration:%20underline%22%3EMidwest%3C/h2%3E"],"htmlLabels":[["%3Cspan%20style=%22color:%20red%22%3E%0A%20%20&amp;bull;%0A%20%20NY%0A%3C/span%3E","%3Cspan%20style=%22color:%20red%22%3E%0A%20%20&amp;bull;%0A%20%20NJ%0A%3C/span%3E","%3Cspan%20style=%22color:%20red%22%3E%0A%20%20&amp;bull;%0A%20%20CT%0A%3C/span%3E"],["%3Cspan%20style=%22color:%20green%22%3E%0A%20%20&amp;bull;%0A%20%20WA%0A%3C/span%3E","%3Cspan%20style=%22color:%20green%22%3E%0A%20%20&amp;bull;%0A%20%20OR%0A%3C/span%3E","%3Cspan%20style=%22color:%20green%22%3E%0A%20%20&amp;bull;%0A%20%20CA%0A%3C/span%3E"],["%3Cspan%20style=%22color:%20blue%22%3E%0A%20%20&amp;bull;%0A%20%20MN%0A%3C/span%3E","%3Cspan%20style=%22color:%20blue%22%3E%0A%20%20&amp;bull;%0A%20%20WI%0A%3C/span%3E","%3Cspan%20style=%22color:%20blue%22%3E%0A%20%20&amp;bull;%0A%20%20IA%0A%3C/span%3E"]],"selected":[{"group":0,"selected":[0,2]}],"displayGroupSizes":true,"closeMenuOnSelect":false,"filterConfig":{"ignoreCase":true,"ignoreAccents":true}}', simplifyVector = FALSE)
    updateSelectControlInput(
      session, "select",
      label = "hello", choices = states, selected = c("WI", "IA"))
  })
}

shinyApp(ui, server)
