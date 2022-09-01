areGroupedOptions <- function(choices){
  any(vapply(choices, length, integer(1L)) > 1L)
}

makeSingleOptions <- function(choices){
  choices <- as.list(choices)
  Names <- names(choices)
  if(is.null(Names)){
    Names <- rep("", length(choices))
  }
  emptyNames <- which(Names == "")
  Names[emptyNames] <- unlist(choices[emptyNames])
  names(choices) <- Names
  lapply(Names, function(nm){
    list(value = as.character(choices[[nm]]), label = nm)
  })
}

makeGroupedOptions <- function(choices){
  lapply(names(choices), function(nm){
    list(label = nm, options = makeSingleOptions(choices[[nm]]))
  })
}

#' @importFrom stats na.exclude na.omit
#' @noRd
getSelectedIndex <- function(options, grouped, selected){
  if(!grouped){
    values <- vapply(options, `[[`, character(1L), "value")
    if(length(selected) == 1L){
      i <- match(selected, values)
    }else{
      i <- vapply(selected, function(s){match(s, values)}, integer(1L))
    }
    return(list(selected = i - 1L))
  }else{
    options <- lapply(options, `[[`, "options")
    groups <- lapply(seq_along(options), function(i){
      vapply(options[[i]],`[[`,character(1L), "value")
    })
    l <- length(selected)
    if(l == 1L){
      nas <- vapply(groups, function(values){
        match(selected, values)
      }, integer(1L))
      notNA <- na.exclude(nas)
      i <- match(notNA, nas)
      return(list(list(group = i-1L, selected = notNA-1L)))
    }else{
      nas <- vapply(groups, function(values){
        match(selected, values)
      }, integer(l))
      notNA <- apply(nas, 2L, function(x){
        x <- na.omit(x)
        length(x) != 0L && any(is.integer(x))
      })
      selectedGroups <- which(notNA)
      return(lapply(selectedGroups, function(j){
        list(group = j-1L, selected = na.exclude(nas[, j])[] - 1L)
      }))
    }
  }
}

isHTML <- function(x){
  inherits(x, "html") || inherits(x, "shiny.tag")
}

toHTML <- function(x){
  list("__html" = URLencode(as.character(x)))
}

isMarkedHTML <- function(x){
  is.list(x) && identical(names(x), "__html")
}

#' @title KaTeX code
#' @description Create an object to be decoded by KaTeX.
#'
#' @param x string, some KaTeX code (this is similar to LaTeX)
#'
#' @return A list containing the url-encoding of \code{x}.
#' @export
#'
#' @examples library(shinySelect)
#' choices <- HTMLchoices(
#'   values = list("alpha", "beta", "gamma"),
#'   labels = list(katex("\\alpha"), katex("\\beta"), katex("\\gamma"))
#' )
katex <- function(x){
  list("__katex" = URLencode(as.character(x)))
}

isKaTeX <- function(x){
  is.list(x) && identical(names(x), "__katex")
}


#' @title Choices with groups and HTML
#' @description Create an object for grouped choices resorting to HTML.
#'
#' @param groups list of HTML elements which can be created
#'   with the \code{\link[htmltools:HTML]{HTML}} function or \code{shiny.tag}
#'   objects, the headings
#' @param labels list of lists, one list for each group, made of HTML elements
#' @param values list of lists of character strings, each label must have a
#'   value
#'
#' @return An object to be passed on to the \code{choices} argument of the
#'   \code{\link{selectControlInput}} function.
#' @export
#' @importFrom utils URLencode
#'
#' @examples library(shinySelect)
#' library(shiny)
#' states <- HTMLgroupedChoices(
#'   groups = lapply(list("East Coast", "West Coast", "Midwest"), function(x){
#'     tags$h2(x, style="text-decoration: underline")
#'   }),
#'   labels = list(
#'     lapply(list("NY", "NJ", "CT"), function(x){
#'       tags$span(HTML("&bull;"), x, style="color: red")
#'     }),
#'     lapply(list("WA", "OR", "CA"), function(x){
#'       tags$span(HTML("&bull;"), x, style="color: green")
#'     }),
#'     lapply(list("MN", "WI", "IA"), function(x){
#'       tags$span(HTML("&bull;"), x, style="color: blue")
#'     })
#'   ),
#'   values = list(
#'     list("NY", "NJ", "CT"),
#'     list("WA", "OR", "CA"),
#'     list("MN", "WI", "IA")
#'   )
#' )
HTMLgroupedChoices <- function(groups, labels, values){
  groups <- vapply(groups, function(nm){
    URLencode(as.character(nm))
  }, character(1L))
  newlabels <- lapply(labels, function(labelist){
    vapply(labelist, function(label){
      if(isKaTeX(label)){
        NA_character_
      }else{
        URLencode(as.character(label))
      }
    }, character(1L))
  })
  for(i in seq_along(newlabels)){
    for(j in seq_along(newlabels[[i]])){
      if(is.na(newlabels[[i]][[j]])){
        newlabels[[i]][[j]] <- labels[[i]][[j]]
      }
    }
  }
  names(values) <- as.character(seq_along(values))
  #names(labels) <- as.character(seq_along(labels))
  out <- values
  attr(out, "htmlgroups") <- groups
  attr(out, "htmllabels") <- newlabels
  out
}

#' @title Choices with HTML
#' @description Create an object for choices resorting to HTML.
#'
#' @param labels the labels of the select control, can be HTML elements created
#'   with the \code{\link[htmltools:HTML]{HTML}} function or \code{shiny.tag}
#'   objects such as \code{tags$span(style = "color:lime;", "label")}
#' @param values the values associated to the labels, they must be character
#'   strings, given in a vector or in a list
#'
#' @return An object (the \code{values} object with some attributes) to be
#'   passed on to the \code{choices} argument of the
#'   \code{\link{selectControlInput}} function.
#' @export
#'
#' @seealso \code{\link{HTMLgroupedChoices}} for choices with groups.
#'
#' @examples library(shinySelect)
#' library(fontawesome)
#' library(shiny)
#' food <- HTMLchoices(
#'   labels = list(
#'     tags$span(fa_i("hamburger"), "Hamburger"),
#'     tags$span(fa_i("pizza-slice"), "Pizza"),
#'     tags$span(fa_i("fish"), "Fish")
#'   ),
#'   values = list("hamburger", "pizza", "fish")
#' )
HTMLchoices <- function(labels, values){
  out <- HTMLgroupedChoices(list("X"), list(labels), list(values))
  a <- attr(out, "htmllabels")
  out <- out[[1L]]
  attr(out, "htmllabels") <- a[[1L]]
  out
}

`%OR%` <- function(x, y){
  if(is.null(x) || length(x) == 0) y else x
}

isBoolean <- function(x){
  is.logical(x) && (length(x) == 1L) && (!is.na(x))
}

isString <- function(x){
  is.character(x) && (length(x) == 1L) && (!is.na(x))
}

isEmpty <- function(x){
  tryCatch({
    length(x) == 0L
  }, error = function(e) FALSE)
}

isNamedList <- function(x){
  is.list(x) && !is.null(names(x)) && all(names(x) != "")
}

#' @title Toggle a select control widget
#' @description Toggle (open/close) a select control widget.
#'
#' @param session the Shiny \code{session} object
#' @param inputId the input id of the select control
#'
#' @return No value; called for side effect.
#' @export
#'
#' @examples # See the last example of 'selectControlInput'.
toggleMenu <- function(session, inputId){
  session$sendCustomMessage(paste0("toggleMenu_", inputId), TRUE)
}

#' @importFrom htmltools htmlDependency
#' @noRd
KaTeX_html_dependency <- function(){
  htmlDependency(
    name = "katex",
    version = "0.15.1",
    src = "www/KaTeX",
    package = "shinySelect",
    script = list(
      src = "katex.min.js",
      integrity = "sha384-z1fJDqw8ZApjGO3/unPWUPsIymfsJmyrDVWC8Tv/a1HeOtGmkwNd/7xUS0Xcnvsx",
      crossorigin = "anonymous",
      defer = ""
    ),
    stylesheet = "katex.min.css"
  )
}

process_label <- function(label){
  if(inherits(label, "shiny.tag")){
    label <- HTML(as.character(label))
  }
  if(inherits(label, "html")){
    label <- list("__html" = URLencode(label))
  }
  if(!isMarkedHTML(label) && !is.null(label) && !isString(label)){
    stop(
      "The `label` argument must be `NULL`, a character string, or a HTML element."
    )
  }
  label
}

process_choices_selected <- function(choices, selected){
  if(!is.null(selected)){
    check <- tryCatch({
      all(vapply(selected, isString, logical(1L)))
    }, error = function(e) FALSE)
    if(!check){
      stop(
        "The `selected` argument must be a vector or a list of character values."
      )
    }
  }
  groupedOptions <- areGroupedOptions(choices)
  options <- if(groupedOptions){
    makeGroupedOptions(choices)
  }else{
    makeSingleOptions(choices)
  }
  if(!is.null(selected)){
    nselected <- length(selected)
    # if(!multiple && nselected > 1L){
    #   stop(
    #     "You cannot provide more than one value in the `selected` argument if ",
    #     "you don't set the `multiple` argument to `TRUE`."
    #   )
    # }
    selected <- getSelectedIndex(options, groupedOptions, selected)
    if(!groupedOptions && anyNA(selected[["selected"]])){
      if(nselected == 1L){
        stop(
          "The value you provided in the `selected` argument cannot be found."
        )
      }else{
        stop(
          "We cannot find all the values you provided in the `selected` argument."
        )
      }
    }
    if(groupedOptions){
      nfound <-
        sum(vapply(selected, function(x) length(x[["selected"]]), integer(1L)))
      if(nfound < nselected){
        if(nselected == 1L){
          stop(
            "The value you provided in the `selected` argument cannot be found."
          )
        }else{
          stop(
            "We cannot find all the values you provided in the `selected` argument."
          )
        }
      }
      values <- c()
      for(i in seq_along(selected)){
        group <- options[[selected[[i]][["group"]] + 1L]][["options"]]
        indices <- selected[[i]][["selected"]] + 1L
        values <- c(
          values,
          vapply(group[indices], `[[`, character(1L), "value")
        )
      }
    }else{
      indices <- selected[["selected"]] + 1L
      values <- vapply(options[indices], `[[`, character(1L), "value")
    }
  }
  list(
    grouped = groupedOptions,
    options = options,
    htmlGroups = attr(choices, "htmlgroups"),
    htmlLabels = attr(choices, "htmllabels"),
    selected = selected
  )
}

# processParameters <- function(
#   label, choices, selected, multiple,
#   sortable, optionsStyles, controlStyles,
#   multiValueStyles, multiValueLabelStyles,
#   multiValueRemoveStyles,
#   containerClass, animated,
#   displayGroupSizes, closeMenuOnSelect,
#   ignoreCaseOnFilter, ignoreAccentsOnFilter
# ){
#   if(sortable && !multiple){
#     warning(
#       "Setting `sortable` has no effect if `multiple = FALSE`."
#     )
#     sortable <- FALSE
#   }
#   emptyNamedList <- `names<-`(list(), character(0L))
#   list(
#     containerClass = containerClass,
#     label = label,
#     optionsStyles = optionsStyles %OR% emptyNamedList,
#     controlStyles = controlStyles %OR% emptyNamedList,
#     multiValueStyles = multiValueStyles %OR% emptyNamedList,
#     multiValueLabelStyles = multiValueLabelStyles %OR% emptyNamedList,
#     multiValueRemoveStyles = multiValueRemoveStyles %OR% emptyNamedList,
#     grouped = groupedOptions,
#     isMulti = multiple,
#     sortable = sortable,
#     animated = animated,
#     options = options,
#     htmlGroups = attr(choices, "htmlgroups"),
#     htmlLabels = attr(choices, "htmllabels"),
#     selected = selected,
#     displayGroupSizes = displayGroupSizes,
#     closeMenuOnSelect = closeMenuOnSelect,
#     filterConfig = list(
#       ignoreCase    = ignoreCaseOnFilter,
#       ignoreAccents = ignoreAccentsOnFilter
#     )
#   )
# }



#' @title Select control widget
#' @description Create a select control widget to be included in a Shiny UI.
#'
#' @param inputId the input slot that will be used to access the value
#' @param label a label for the widget, can be a HTML element; \code{NULL}
#'   for no label
#' @param choices a list of single choices or grouped choices; to use HTML, see
#'   the functions \code{\link{HTMLchoices}} and \code{\link{HTMLgroupedChoices}}
#' @param selected the initially selected value; can be \code{NULL} and can be
#'   a vector or a list of values if \code{multiple = TRUE}
#' @param multiple Boolean, whether the selection of multiple items is allowed
#' @param sortable Boolean, whether the multiple selections are sortable
#' @param optionsStyles styles for the options, given as a list
#' @param controlStyles styles for the control bar, given as a list
#' @param multiValueStyles styles for the item boxes when
#'   \code{multiple = TRUE}, such as the background color
#' @param multiValueLabelStyles styles for the item labels when
#'   \code{multiple = TRUE}, such as the font style
#' @param multiValueRemoveStyles styles for the box containing the cross used
#'   to remove an item
#' @param containerClass CSS class(es) for the container; the default value
#'   assumes you use the 'bslib' package with
#'   \code{\link[bslib:bs_theme]{bs_theme(version = 4)}}
#' @param animated Boolean; this has an effect only when \code{multiple = TRUE}:
#'   the removal of the items is animated
#' @param displayGroupSizes only for grouped choices, whether to display the
#'   number of elements of each group
#' @param closeMenuOnSelect Boolean, whether to close the menu when doing a
#'   selection
#' @param ignoreCaseOnFilter Boolean, whether to ignore the case when searching
#'   an option
#' @param ignoreAccentsOnFilter Boolean, whether to ignore the accents when
#'   searching an option
#'
#' @return An input element that can be included in a Shiny UI definition.
#' @export
#' @importFrom reactR createReactShinyInput
#' @importFrom htmltools htmlDependency tags HTML
#' @importFrom utils URLencode
#' @importFrom fontawesome fa_html_dependency
#'
#' @examples # an example using KaTeX ####
#' library(shiny)
#' library(shinySelect)
#' library(bslib)
#'
#' choices <- HTMLchoices(
#'   values = list("alpha", "beta", "gamma"),
#'   labels = list(katex("\\alpha"), katex("\\beta"), katex("\\gamma"))
#' )
#'
#' ui <- fluidPage(
#'   theme = bs_theme(version = 4),
#'   titlePanel("KaTeX example"),
#'   selectControlInput(
#'     "select",
#'     label = tags$h1("Make a choice", style="color: red;"),
#'     choices = choices,
#'     selected = "alpha",
#'     multiple = FALSE
#'   ),
#'   br(),
#'   verbatimTextOutput("textOutput")
#' )
#'
#' server <- function(input, output, session) {
#'   output[["textOutput"]] <- renderPrint({
#'     sprintf("You selected: %s.", input[["select"]])
#'   })
#' }
#'
#' if(interactive()){
#'   shinyApp(ui, server)
#' }
#'
#' # An example of `sortable = TRUE`, with fontawesome icons ####
#' library(shiny)
#' library(shinySelect)
#' library(bslib)
#' library(fontawesome)
#'
#' food <- HTMLchoices(
#'   labels = list(
#'     tags$span(fa_i("hamburger"), "Hamburger"),
#'     tags$span(fa_i("pizza-slice"), "Pizza"),
#'     tags$span(fa_i("fish"), "Fish")
#'   ),
#'   values = list("hamburger", "pizza", "fish")
#' )
#'
#' styles <- list(
#'   borderBottom = "2px solid orange",
#'   backgroundColor = list(
#'     selected = "cyan",
#'     focused = "lemonchiffon",
#'     otherwise = "seashell"
#'   )
#' )
#'
#' ui <- fluidPage(
#'   theme = bs_theme(version = 4),
#'   titlePanel("Sortable example"),
#'   selectControlInput(
#'     "select",
#'     label = tags$h1("Make a choice", style="color: red;"),
#'     optionsStyles = styles,
#'     choices = food,
#'     selected = "hamburger",
#'     multiple = TRUE,
#'     sortable = TRUE,
#'     animated = TRUE
#'   ),
#'   br(),
#'   verbatimTextOutput("textOutput")
#' )
#'
#' server <- function(input, output, session) {
#'   output[["textOutput"]] <- renderPrint({
#'     sprintf("You selected: %s.", toString(input[["select"]]))
#'   })
#' }
#'
#' if(interactive()){
#'   shinyApp(ui, server)
#' }
#'
#' # An example with tooltips ####
#' library(shiny)
#' library(bslib)
#' library(shinySelect)
#'
#' data(Countries, package = "jsTreeR")
#'
#' continents <- unique(Countries[["continentName"]])
#'
#' L <- lapply(continents, function(continent){
#'   indices <- Countries[["continentName"]] == continent
#'   countries <- Countries[["countryName"]][indices]
#'   pop <- Countries[["population"]][indices]
#'   mapply(function(x, y){tags$span(x, `data-toggle`="tooltip", title=y)},
#'          countries, pop, SIMPLIFY = FALSE, USE.NAMES = FALSE)
#' })
#'
#' countries <- lapply(continents, function(continent){
#'   indices <- Countries[["continentName"]] == continent
#'   Countries[["countryName"]][indices]
#' })
#'
#' countries <- HTMLgroupedChoices(
#'   groups = lapply(continents, function(nm) tags$h2(nm, style="color: blue;")),
#'   labels = L,
#'   values = countries
#' )
#'
#' CSS <- '
#' .tooltip {
#'   pointer-events: none;
#' }
#' .tooltip > .tooltip-inner {
#'   pointer-events: none;
#'   background-color: #73AD21;
#'   color: #FFFFFF;
#'   border: 1px solid green;
#'   padding: 5px;
#'   font-size: 15px;
#'   text-align: justify;
#'   margin-left: 10px;
#'   max-width: 1000px;
#' }
#' .tooltip > .arrow::before {
#'   border-top-color: #73AD21;
#' }
#' '
#'
#' ui <- fluidPage(
#'   theme = bs_theme(version = 4),
#'   tags$head(
#'     tags$style(HTML(CSS))
#'   ),
#'   titlePanel("Tooltips example"),
#'   sidebarLayout(
#'     sidebarPanel(
#'       selectControlInput(
#'         "select",
#'         label = tags$h3("Choose some countries", style="color: red;"),
#'         containerClass = NULL,
#'         choices = countries,
#'         selected = c("Tonga", "Austria"),
#'         multiple = TRUE,
#'         animated = TRUE
#'       )
#'     ),
#'     mainPanel(
#'       verbatimTextOutput("textOutput")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   output[["textOutput"]] <- renderPrint({
#'     sprintf("You selected: %s.", toString(input[["select"]]))
#'   })
#' }
#'
#' if(interactive()){
#'   shinyApp(ui, server)
#' }
#'
#'
#' # An example of custom styles ####
#' library(shiny)
#' library(shinySelect)
#'
#' states <- HTMLgroupedChoices(
#'   groups = lapply(list("East Coast", "West Coast", "Midwest"), function(x){
#'     tags$h2(x, style="text-decoration: underline")
#'   }),
#'   labels = list(
#'     lapply(list("NY", "NJ", "CT"), function(x){
#'       tags$span(HTML("&bull;"), x, style="color: red")
#'     }),
#'     lapply(list("WA", "OR", "CA"), function(x){
#'       tags$span(HTML("&bull;"), x, style="color: green")
#'     }),
#'     lapply(list("MN", "WI", "IA"), function(x){
#'       tags$span(HTML("&bull;"), x, style="color: blue")
#'     })
#'   ),
#'   values = list(
#'     list("NY", "NJ", "CT"),
#'     list("WA", "OR", "CA"),
#'     list("MN", "WI", "IA")
#'   )
#' )
#'
#' styles <- list(
#'   borderBottom = "2px dotted orange",
#'   backgroundColor = list(
#'     selected = "cyan",
#'     focused = "lemonchiffon",
#'     otherwise = "seashell"
#'   )
#' )
#' controlStyles = list(
#'   marginTop = "0",
#'   marginRight = "50px",
#'   boxShadow = toString(c(
#'     "rgba(50, 50, 93, 0.25) 0px 50px 100px -20px",
#'     "rgba(0, 0, 0, 0.3) 0px 30px 60px -30px",
#'     "rgba(10, 37, 64, 0.35) 0px -2px 6px 0px inset;"
#'   ))
#' )
#' multiValueStyles = list(
#'   backgroundColor = "lavenderblush"
#' )
#' multiValueLabelStyles = list(
#'   fontStyle = "italic",
#'   fontWeight = "bold"
#' )
#' multiValueRemoveStyles = list(
#'   color = "hotpink",
#'   ":hover" = list(
#'     backgroundColor = "navy",
#'     color = "white"
#'   )
#' )
#'
#' CSS <- '
#' div[class$="-group"][id^="react-select"][id$="-heading"] {
#'   background: #0F2027;  /* fallback for old browsers */
#'   background: -webkit-linear-gradient(to right, #2C5364, #203A43, #0F2027);
#'   background: linear-gradient(to right, #2C5364, #203A43, #0F2027);
#' }'
#'
#' ui <- fluidPage(
#'   tags$head(
#'     tags$style(HTML(CSS))
#'   ),
#'   titlePanel("Custom styles example"),
#'   splitLayout(
#'     selectControlInput(
#'       "select",
#'       label = tags$h1("Choose some states", style="color: red;"),
#'       containerClass = NULL,
#'       optionsStyles = styles,
#'       controlStyles = controlStyles,
#'       multiValueStyles = multiValueStyles,
#'       multiValueLabelStyles = multiValueLabelStyles,
#'       multiValueRemoveStyles = multiValueRemoveStyles,
#'       choices = states,
#'       selected = list("NY", "CT"),
#'       multiple = TRUE,
#'       sortable = TRUE,
#'       animated = TRUE
#'     ),
#'     tagList(
#'       verbatimTextOutput("textOutput"),
#'       br(),
#'       actionButton("toggle", "Toggle menu", class = "btn-primary")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   output[["textOutput"]] <- renderPrint({
#'     sprintf("You selected: %s.", toString(input[["select"]]))
#'   })
#'   observeEvent(input[["toggle"]], {
#'     toggleMenu(session, "select")
#'   })
#' }
#'
#' if(interactive()){
#'   shinyApp(ui, server)
#' }
selectControlInput <- function(
  inputId, label, choices, selected = NULL, multiple = FALSE,
  sortable = FALSE, optionsStyles = list(), controlStyles = list(),
  multiValueStyles = list(), multiValueLabelStyles = list(),
  multiValueRemoveStyles = list(),
  containerClass = "mt-4 col-md-6 col-offset-4", animated = FALSE,
  displayGroupSizes = TRUE, closeMenuOnSelect = !multiple,
  ignoreCaseOnFilter = TRUE, ignoreAccentsOnFilter = TRUE
){
  stopifnot(isBoolean(multiple))
  stopifnot(isBoolean(sortable))
  stopifnot(isEmpty(optionsStyles) || isNamedList(optionsStyles))
  stopifnot(isEmpty(controlStyles) || isNamedList(controlStyles))
  stopifnot(isEmpty(multiValueStyles) || isNamedList(multiValueStyles))
  stopifnot(isEmpty(multiValueLabelStyles) || isNamedList(multiValueLabelStyles))
  stopifnot(isEmpty(multiValueRemoveStyles) || isNamedList(multiValueRemoveStyles))
  stopifnot(isBoolean(animated))
  stopifnot(isBoolean(displayGroupSizes))
  stopifnot(isBoolean(closeMenuOnSelect))
  stopifnot(is.null(containerClass) || isString(containerClass))
  stopifnot(isBoolean(ignoreCaseOnFilter))
  stopifnot(isBoolean(ignoreAccentsOnFilter))

  # LIST <- processParameters(
  #   label, choices, selected, multiple,
  #   sortable, optionsStyles, controlStyles,
  #   multiValueStyles, multiValueLabelStyles,
  #   multiValueRemoveStyles,
  #   containerClass, animated,
  #   displayGroupSizes, closeMenuOnSelect,
  #   ignoreCaseOnFilter, ignoreAccentsOnFilter
  # )
  # LIST[["shinyId"]] <- inputId
  if(inherits(label, "shiny.tag")){
    label <- HTML(as.character(label))
  }
  if(inherits(label, "html")){
    label <- list("__html" = URLencode(label))
  }
  if(!isMarkedHTML(label) && !is.null(label) && !isString(label)){
    stop(
      "The `label` argument must be `NULL`, a character string, or a HTML element."
    )
  }
  if(sortable && !multiple){
    warning(
      "Setting `sortable` has no effect if `multiple = FALSE`."
    )
    sortable <- FALSE
  }
  if(!is.null(selected)){
    check <- tryCatch({
      all(vapply(selected, isString, logical(1L)))
    }, error = function(e) FALSE)
    if(!check){
      stop(
        "The `selected` argument must be a vector or a list of character values."
      )
    }
  }
  groupedOptions <- areGroupedOptions(choices)
  options <- if(groupedOptions){
    makeGroupedOptions(choices)
  }else{
    makeSingleOptions(choices)
  }
  if(!is.null(selected)){
    nselected <- length(selected)
    if(!multiple && nselected > 1L){
      stop(
        "You cannot provide more than one value in the `selected` argument if ",
        "you don't set the `multiple` argument to `TRUE`."
      )
    }
    selected <- getSelectedIndex(options, groupedOptions, selected)
    if(!groupedOptions && anyNA(selected[["selected"]])){
      if(nselected == 1L){
        stop(
          "The value you provided in the `selected` argument cannot be found."
        )
      }else{
        stop(
          "We cannot find all the values you provided in the `selected` argument."
        )
      }
    }
    if(groupedOptions){
      nfound <-
        sum(vapply(selected, function(x) length(x[["selected"]]), integer(1L)))
      if(nfound < nselected){
        if(nselected == 1L){
          stop(
            "The value you provided in the `selected` argument cannot be found."
          )
        }else{
          stop(
            "We cannot find all the values you provided in the `selected` argument."
          )
        }
      }
      values <- c()
      for(i in seq_along(selected)){
        group <- options[[selected[[i]][["group"]] + 1L]][["options"]]
        indices <- selected[[i]][["selected"]] + 1L
        values <- c(
          values,
          vapply(group[indices], `[[`, character(1L), "value")
        )
      }
    }else{
      indices <- selected[["selected"]] + 1L
      values <- vapply(options[indices], `[[`, character(1L), "value")
    }
  }else{
    values <- NULL
  }
  emptyNamedList <- `names<-`(list(), character(0L))
  createReactShinyInput(
    inputId,
    "selectControl",
    list(
      htmlDependency(
        name = "selectControl-input",
        version = "1.0.0",
        src = "www/shinySelect/selectControl",
        package = "shinySelect",
        script = "selectControl.js",
        stylesheet = "selectControl.css"
      ),
      fa_html_dependency(),
      KaTeX_html_dependency()
    ),
    default = as.list(values), # useless! -no
    # LIST,
    list(
      shinyId = inputId,
      containerClass = containerClass,
      label = label,
      optionsStyles = optionsStyles %OR% emptyNamedList,
      controlStyles = controlStyles %OR% emptyNamedList,
      multiValueStyles = multiValueStyles %OR% emptyNamedList,
      multiValueLabelStyles = multiValueLabelStyles %OR% emptyNamedList,
      multiValueRemoveStyles = multiValueRemoveStyles %OR% emptyNamedList,
      grouped = groupedOptions,
      isMulti = multiple,
      sortable = sortable,
      animated = animated,
      options = options,
      htmlGroups = attr(choices, "htmlgroups"),
      htmlLabels = attr(choices, "htmllabels"),
      selected = as.list(selected),
      displayGroupSizes = displayGroupSizes,
      closeMenuOnSelect = closeMenuOnSelect,
      filterConfig = list(
        ignoreCase    = ignoreCaseOnFilter,
        ignoreAccents = ignoreAccentsOnFilter
      )
    ),
    tags$div
  )
}

#' @title Update a select control widget
#' @description Change the value of a select control input.
#'
#' @param session the Shiny \code{session} object
#' @param inputId the id of the select control widget to be updated
#' @param choices new choices, or \code{NULL}
#' @param selected new value(s) for the selected items, or \code{NULL}
#'
#' @return No returned value, called for side effect.
#'
#' @export
updateSelectControlInput <- function(
  session, inputId, choices = NULL, selected = NULL
){
  if(is.null(choices)) {
    session$sendCustomMessage(
      paste0("updateValue_", inputId),
      as.list(selected)
    )
  } else {
    config <- process_choices_selected(choices, selected)
    #config <- c(config, list(isMulti = TRUE))
    session$sendInputMessage(inputId, message)
  }
}
