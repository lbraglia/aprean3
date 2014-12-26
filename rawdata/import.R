## -----
## Utils
## -----
"%without%" <-  function(x, y) x[!(x %in% y)]

## Function for 'extracting' description from naming scheme
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
        "Dataset for Section \\1.\\2",
        x,
        perl = TRUE)
  } else if (grepl("^\\d\\d-\\d{1,2}-TABLE$", x, perl = TRUE)) {
    ## Tables
    sub("^(\\d\\d)-(\\d{1,2})-TABLE$" ,
        "Dataset from Table \\1.\\2",
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

TODO <- c("03-2", "E05G", "E14C", "E14K", "E22A", "E22C", "E22D", "E23A",
          "E23E", "E23F", "E23G", "E23H")

processed <- list.files("cleaned") %without% c(TODO, "Makefile")

## ---------------
## Standard import
## ---------------
import.fun <- function(path) {
  cat("Importing", path , "\n")
  res <- read.table(file = path, header = TRUE, fill = TRUE)
  res <- res[, names(res) %without% "id"]
  ## filenam from cleaned directory
  fileName <- strsplit(path, "/")[[1]][2]
  ## Exceptions to data.frame of fully numeric data here ...
  if (fileName == "16EX1") {
    res <- unname(as.matrix(res))
  } else if (fileName == "E07C") {
    res$x4 <- factor(res$x4, levels = c(-1,1), labels = c("No","Yes"))
  } else if (fileName == "E14B") {
    res <- within(res, {
      tiller <- factor(tiller)
      type <- factor(type, levels = c("w","c"), labels = c("Waldron","Ciano"))
      nrate <- factor(nrate, levels = c(0,50,270))
    })
  } else if (fileName == "E14R") {
    res <- within(res, {
      boot <- factor(boot, levels = c(-1,1), labels = c("A","B"))
      temperature <- factor(temperature)
      subject <- factor(subject)
    })
  }
  ## Now, attach a bit of metadata (useful in what follows) to the object
  attr(res, "fileName") <- fileName
  ## data.frame name
  ## prefix a 't' for tables (eg t03-3)
  dfName <- gsub("^(.+)(-TABLE)$", "t\\1" , fileName, perl = TRUE)
  dfName <- gsub("-","_", dfName)
  dfName <- paste0("ds", dfName)
  dfName <- tolower(dfName)
  attr(res, "dfName") <- dfName
  ## Description of the data
  attr(res, "dfDescription") <- description(fileName)
  res
}
pathList <- as.list( paste0("cleaned", "/", processed) )
dfList <- lapply(pathList, import.fun)

## -------------------
## making roxygen2 doc
## -------------------
doc.fun <- function(x){
  
  dfName <- attr(x, "dfName")
  ## Header section
  Header <- c(
    sprintf("#' %s", attr(x, "dfDescription") ),
    "#' ",
    sprintf("#' %s", attr(x, "dfDescription")),
    "#' "
  )
  ## Format/Describe section (if data.frame give names of columns,
  ## otherwise if matrix list only ncol and nrow.
  makeRdItems <- function(item){
    unlist(lapply(item, function(x) sprintf("#' \\item{%s}{}", x)))
  }
  Format <- sprintf("#' @format A \\code{%s} with %s rows and %s %s%s",
                    class(x), nrow(x), ncol(x),
                    ifelse(is.matrix(x), "columns", "variables"),
                    ifelse(is.matrix(x), ".", ":"))
  if (class(x) == "data.frame")
    Format <- c(Format , "#' \\describe{", makeRdItems(names(x)), "#' }")

  
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
  dfName <- attr(x, "dfName")
  ## remove used metadata
  myAttrs <- c("fileName", "dfName", "dfDescription")
  attributes(x)[myAttrs] <- NULL
  ## now save in proper name and export
  eval(parse(text = sprintf("%s <- x", dfName)))
  save(list = c(dfName),
       file = sprintf("../data/%s.rda", dfName),
       compress = "bzip2")
}
lapply(dfList, export.fun)

