## -----
## Utils
## -----
"%without%" <-  function(x, y) x[!(x %in% y)]

## Function to 'extract' description from naming scheme
description <- function(x) {
  if (grepl("^E\\d\\d", x, perl = TRUE)) {
    ## Exercises match E\n\nXXX
    sub("^E(\\d\\d)(.*)" ,
        "Dataset for Exercise \\2, Chapter \\1",
        x,
        perl = TRUE)
  } else if (grepl("^\\d\\dEX\\d$", x, perl = TRUE)) {
    ## Examples match \n\nEx\n
    sub("^(\\d\\d)EX(.*)$" ,
        "Dataset for Example \\2, Chapter \\1",
        x,
        perl = TRUE)
  } else if (grepl("^\\d\\d[A-Z]$", x, perl = TRUE)) {
    ## Appendixes
    sub("^(\\d\\d)([A-Z])$" ,
        "Dataset for Appendix \\2, Chapter \\1",
        x,
        perl = TRUE)
  } else if (grepl("^\\d\\d-\\d-\\d$", x, perl = TRUE)) {
    ## Equations
    sub("^(\\d\\d)-(\\d)-(\\d)$" ,
        "Dataset for Equation \\1.\\2.\\3",
        x,
        perl = TRUE)
  } else if (grepl("^\\d\\d-\\d{1,2}$", x, perl = TRUE)) {
    ## Sections
    sub("^(\\d\\d)-(\\d{1,2})$" ,
        "Dataset for Table \\1.\\2",
        x,
        perl = TRUE)
  } else
    ## When everything else fails ...
    sprintf("%s data", x)
}

## ------
## Status
## ------
## numbering refers to order given by `make` in cleaned subdir

TODO <- c(3, 93, 99, 123, 125, 126, 130, 132, 133, 134, 135)

processed <- 1:150 %without% TODO

## ---------------
## Standard import
## ---------------
import.fun <- function(path) {
  cat("Importing", path , "\n")
  res <- read.table(file = path, header = TRUE, fill = TRUE)
  res <- res[, names(res) %without% "id"]
  ## original name of dataset (from file path)
  originalName <- strsplit(path, "/")[[1]][2]
  attr(res, "originalName") <- originalName
  ## data.frame name included as comment
  dfName <- gsub("-","_", originalName)
  dfName <- paste0("ds", dfName)
  dfName <- tolower(dfName)
  comment(res) <- dfName
  ## Description of the data, from the name?
  attr(res, "description") <- description(originalName)
  res
}
pathList <- as.list( paste0("cleaned", "/", list.files("cleaned")) )
dfList <- lapply(pathList[processed], import.fun)

## -------------------
## making roxygen2 doc
## -------------------
doc.fun <- function(x){
  dfName <- comment(x)
  ## Header section
  Header <- c(
    sprintf("#' %s", attr(x, "description") ),
    "#' ",
    sprintf("#' %s"     , attr(x, "description")),
    "#' "
  )
  ## Format/Describe section
  makeRdItems <- function(item){
    unlist(lapply(item, function(x) sprintf("#' \\item{%s}{}", x)))
  }
  Format <- c(
    sprintf("#' @format A data.frame with %s rows and %s variables:",
            nrow(x), ncol(x)),
    "#' \\describe{",
    makeRdItems(names(x)),
    "#' }"
    )
  ## The source book
  Source <- paste0("#' @source Draper, N.R., Smith, H., (1998) ",
                   "Applied Regression Analyis, 3rd ed., ",
                   "New York: Wiley")
  ## Simple examples
  Examples <- c("#' @examples",
                sprintf("#' %s", dfName))
  ## and the name of the dataset as string
  Dataset <- sprintf("'%s'", dfName)
  ## write all to the appropriate .R file
  writeLines(text = c(Header, Format, Source, Examples, Dataset),
             con = sprintf("../R/%s.R", dfName))
}
lapply(dfList, doc.fun)

## ---------------
## Standard export
## ---------------
export.fun <- function(x) {
  dfName <- comment(x)
  eval(parse(text = sprintf("%s <- x", dfName)))
  save(list = c(dfName),
       file = sprintf("../data/%s.rda", dfName),
       compress = "bzip2")
}
lapply(dfList, export.fun)

