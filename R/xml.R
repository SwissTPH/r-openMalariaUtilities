##
### Generators

## Adds multiple elements names "element" to xml object "x", each with the
## attributes set provided by "attributeList". This list is composed of multiple
## "attribute" = "value" or "attribute" = c("value", "value", ...) pairs. In
## case a vector is provided, a new element per value is generated. Vectors need
## to be of the same length. If a value is NA, it will be skipped in the xml
## generation. It can also be a dataframe.

##' @title Generate XML chunks
##' @param outlist TODO
##' @param element TODO
##' @param attributeList TODO
##' @return XML entries
.xmlAddChunks <- function(outlist, element, attributeList) {
  ## Test if all attribute vectors have the same length and abort if not
  if (!all(sapply(attributeList, length) == length(attributeList[[1]]))) {
    stop("Value vectors in attributeList need to be of same length.")
  }
  ## Loop over row, generating an entry each time and appending it to outlist
  for (i in seq_len(length(attributeList[[1]]))) {
    entry <- list()
    add <- list()
    for (attribute in names(attributeList)) {
      entry[[attribute]] <- attributeList[[attribute]][[i]]
    }
    add[[element]] <- entry
    outlist <- append(outlist, add)
  }
  return(outlist)
}

##' @title Collect if current value is a placeholder
##' @param x A R object.
.placeholderCollect <- function(x) {
  ## Check if x is a string
  if (is.character(x)) {
    ## Test if x confirms to @...@ pattern
    matched <- grepl("@(.*?)@", x)
    ## If true, store it in the cache
    if (matched == TRUE) {
      value <- gsub("@(.*?)@", "\\1", x)
      omuCache$placeholders <<- unique(
        c(value, omuCache$placeholders)
      )
    }
  }
}


##
### Create xml document

## Recursive function which step through the list and generate the xml entries
## accordingly
##' @title TODO
##' @param x TODO
##' @param data List with experiment information
##' @param errCol TODO
##' @param recLevel TODO
##' @param validate Boolean, if data should be validated
##' @return
recXML <- function(x, data, errCol, recLevel = list(), validate = TRUE) {
  ## Name of the current entry
  top <- names(data)
  ## Store entry for debugging
  recLevel <- append(recLevel, top)
  ## Rest of the list
  rest <- data[[top]]
  ## Generate a new root for the current entry
  chunk <- xml2::xml_new_root(top)
  ## Loop over the list
  for (i in seq_len(length(rest))) {
    ## If current rest is not another list and does not have a name, insert
    ## it. Results in something like '<a>foo</a>'
    if (!is.list(rest[[i]])) {
      if (is.null(names(rest)[[i]]) | names(rest)[[i]] == "") {
        ## Check if it is a placeholder and if yes, store it in the cache
        .placeholderCollect(rest[[i]])
        xml2::xml_set_text(chunk, as.character(rest[[i]]))
      } else {
        ## If it has a validation entry, run its function.
        ## Funky way to construct the entry!
        func <- Reduce(`[[`, append(.omuValidation, append(recLevel, names(rest)[[i]])))
        if (validate == TRUE & !is.null(func)) {
          do.call(
            what = func,
            args = list(rest[[i]], paste0(paste0(recLevel, collapse = ":"), ":", names(rest)[[i]]), errCol)
          )
        }
        ## Check if it is a placeholder and if yes, store it in the cache
        .placeholderCollect(rest[[i]])
        ## Set the attribute for the current named entry (e.g. '<a
        ## foo="baz"/>')
        if (!is.na(rest[[i]])) {
          xml2::xml_set_attr(
            chunk,
            names(rest)[[i]],
            rest[[i]]
          )
        }
      }
    } else {
      ## Recurse
      recXML(chunk, rest[i], errCol, recLevel, validate)
    }
  }
  ## Return merged xml2 document
  return(xml2::xml_add_child(x, chunk))
}

##' @title Create xml entries recursively
##' @param baseXML The root xml object
##' @param data List with experiment information
##' @param validate Boolean, if data should be validated
##' @return xml document
.xmlMakeDocRec <- function(baseXML, data, validate = TRUE) {
  ## Entries which should be generated
  xmlEntries <- c(
    "demography",
    "monitoring",
    "interventions",
    "healthSystem",
    "entomology",
    "parasiteGenetics",
    "pharmacology",
    "diagnostics",
    "model"
  )
  ## Assertion collection for input validation
  assertCol <- checkmate::makeAssertCollection()

  ## Clear cached placehoders, if any
  if (hash::has.key("placeholders", omuCache) == TRUE) {
    omuCache$placeholders <- NULL
  }

  ## Run the recursion
  for (i in seq_len(length(data))) {
    if (names(data[i]) %in% xmlEntries & is.list(data[i])) {
      recXML(x = baseXML, data = data[i], errCol = assertCol, recLevel = list(), validate = validate)
    }
  }
  ## Report errors
  checkmate::reportAssertions(collection = assertCol)
  ## Return scenario
  return(baseXML)
}
