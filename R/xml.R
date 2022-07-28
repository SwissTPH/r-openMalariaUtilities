### Generate a xml document for openMalaria

## Adds multiple elements names "element" to xml object "x", each with the
## attributes set provided by "attributeList". This list is composed of multiple
## "attribute" = "value" or "attribute" = c("value", "value", ...) pairs. In
## case a vector is provided, a new element per value is generated. Vectors need
## to be of the same length. If a value is NA, it will be skipped in the xml
## generation. It can also be a dataframe.

##' @include cache.R printing.R
NULL


##' @title Generate XML chunks
##' @param outlist List which will get modified and returned.
##' @param element Element to add
##' @param attributeList Attributes to add
##' @return XML entries
##' @keywords internal
.xmlAddChunks <- function(outlist, element, attributeList) {
  ## Test if all attribute vectors have the same length and abort if not
  if (!all(vapply(
    attributeList, length,
    FUN.VALUE = numeric(1), USE.NAMES = FALSE
  ) == length(attributeList[[1]]))) {
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
      putCache(x = "placeholders", unique(
        c(value, getCache(x = "placeholders"))
      ))
    }
  }
}


##
### Create xml document

## Recursive function which step through the list and generate the xml entries
## accordingly
##' @title Recursively create XML
##' @param x XML node
##' @param data List with experiment information
##' @param errCol Checkmate error collection
##' @param recLevel List containing current recursion level
##' @keywords internal
recXML <- function(x, data, errCol, recLevel = list()) {
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
      if (is.null(names(rest)[[i]]) || names(rest)[[i]] == "") {
        ## Check if it is a placeholder and if yes, store it in the cache
        .placeholderCollect(rest[[i]])
        xml2::xml_set_text(chunk, as.character(rest[[i]]))
      } else {
        ## REVIEW The way the validation was implemented was by far to tedious.
        ##        Thus, this is deprecated and removed for now until we have a
        ##        way to parse the openMalaria xsd file and extract the valid
        ##        types. Or we decide to not validate all entries and be done
        ##        with it.

        ## If it has a validation entry, run its function. Funky way to
        ## construct the entry!
        ## func <- Reduce(`[[`, append(.omuValidation, append(recLevel, names(rest)[[i]])))
        ## if (validate == TRUE & !is.null(func)) {
        ##   do.call(
        ##     what = func,
        ##     args = list(rest[[i]], paste0(paste0(recLevel, collapse = ":"), ":", names(rest)[[i]]), errCol)
        ##   )
        ## }
        ## Check if it is a placeholder and if yes, store it in the cache
        .placeholderCollect(rest[[i]])
        ## Set the attribute for the current named entry (e.g. '<a
        ## foo="baz"/>')
        if (is.null(rest[[i]])) {
          stop(
            paste0(
              paste0(
                paste0(recLevel, collapse = ":"), ":", names(rest)[[i]]
              ),
              " is not allowed to be NULL!"
            )
          )
        }
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
      recXML(chunk, rest[i], errCol, recLevel)
    }
  }
  ## Return merged xml2 document
  return(xml2::xml_add_child(x, chunk))
}

##' @title Create xml entries recursively
##' @param baseXML The root xml object
##' @param data List with experiment information
##' @keywords internal
.xmlMakeDocRec <- function(baseXML, data) {
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
  putCache(x = "placeholders", value = NULL)

  ## Run the recursion
  for (i in seq_len(length(data))) {
    if (names(data[i]) %in% xmlEntries & is.list(data[i])) {
      recXML(x = baseXML, data = data[i], errCol = assertCol, recLevel = list())
    }
  }
  ## Report errors
  checkmate::reportAssertions(collection = assertCol)
  ## Return scenario
  return(baseXML)
}

##' @title Add sublist to list
##' @param data List to add to.
##' @param sublist Sublist, under which the entry should get added. Must be
##'   given as a vector, e.g. c("level1", "level2", ...). Can be NULL.
##' @param entry Name of the entry to add, as string. Can be NULL.
##' @param input List to add.
##' @param append Boolean. Determines if the list should be appended to existing
##'   entries or overwrite them.
##' @keywords internal
.xmlAddList <- function(data, sublist, entry, input, append = TRUE) {
  ## If we don't want to append, we will override the present entry.
  if (append == FALSE) {
    ## Make sure to remove all entries corresponding to sublist
    if (!is.null(sublist)) {
      data[[c(sublist)]] <- data[[c(sublist)]][
        names(data[[c(sublist)]]) %in% c(entry) == FALSE
      ]
    }
    ## Add new entry
    if (!is.null(entry)) {
      data[[c(sublist, entry)]] <- input
    } else {
      data <- input
    }
    return(data)
  } else {
    ## Otherwise we append the data
    ## If sublist is not given, backup whole list
    if (!is.null(sublist)) {
      oldEntry <- data[[sublist]]
    } else {
      oldEntry <- data
    }
    ## Create new list
    newEntry <- list()
    if (!is.null(entry)) {
      newEntry[[entry]] <- input
    } else {
      newEntry <- input
    }
    ## Appen to old list
    if (!is.null(sublist)) {
      data[[sublist]] <- append(oldEntry, newEntry)
    } else {
      data <- append(oldEntry, newEntry)
    }
    return(data)
  }
}
