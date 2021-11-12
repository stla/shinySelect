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


#' Title
#'
#' @param values
#' @param names
#'
#' @return
#' @export
#'
#' @examples
HTMLgroupedChoices <- function(groups, labels, values){
  groups <- vapply(groups, function(nm){
    URLencode(as.character(nm))
  }, character(1L))
  labels <- lapply(labels, function(labelist){
    vapply(labelist, function(label){
      URLencode(as.character(label))
    }, character(1L))
  })
  names(values) <- as.character(seq_along(values))
  #names(labels) <- as.character(seq_along(labels))
  out <- values
  attr(out, "htmlgroups") <- groups
  attr(out, "htmllabels") <- labels
  out
}

#' Title
#'
#' @param values
#' @param names
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param session
#' @param inputId
#'
#' @return
#' @export
#'
#' @examples
toggleMenu <- function(session, inputId){
  session$sendCustomMessage(paste0("toggleMenu_", inputId), TRUE)
}


#' <Add Title>
#'
#' <Add Description>
#'
#' @importFrom reactR createReactShinyInput
#' @importFrom htmltools htmlDependency tags HTML
#' @importFrom utils URLencode
#' @importFrom fontawesome fa_html_dependency
#'
#' @export
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
      fa_html_dependency()
    ),
    default = as.list(values), # useless!
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
      selected = selected,
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

#' <Add Title>
#'
#' <Add Description>
#'
#' @export
updateSelectControlInput <- function(session, inputId, value, configuration = NULL) {
  message <- list(value = value)
  if (!is.null(configuration)) message$configuration <- configuration
  session$sendInputMessage(inputId, message);
}
