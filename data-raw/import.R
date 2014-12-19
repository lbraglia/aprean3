dataset <- list()

"%without%" <-  function(x, y) x[!(x %in% y)]

TODO <- c(3, 5, 6, 7, 15, 20, 24, 27, 29, 48, 55, 81, 85, 92:99, 110, 123,
          125, 126, 130, 131, 132, 133, 134, 135, 146) 

processed <- 1:150 %without% TODO
## ---------------
## Standard import
## ---------------
import.fun <- function(path) {
  cat("Importing", path , "\n")
  res <- read.table(file = path, header = TRUE, fill = TRUE)
  ## saving dataset name in comment format = dsfilename
  baseName <- strsplit(path, "/")[[1]][2]
  baseName <- gsub("-","_", baseName)
  comment(res) <- tolower(paste0("ds", baseName ))
  res
}
pathList <- as.list( paste0("cleaned", "/", list.files("cleaned")) )
dfList <- lapply(pathList[processed], import.fun)

## -------------------
## making roxygen2 doc
## -------------------
originalName <- function(x) {
  x <- toupper(x)
  x <- gsub("_", "-", x)
  x <- gsub("^DS", "", x)
  x
}

doc.fun <- function(x){
  dfName <- comment(x)
  ## Header section
  Header <- c(
    sprintf("#' %s data", originalName(dfName)),
    "#' ",
    sprintf("#' %s data", dfName),
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

