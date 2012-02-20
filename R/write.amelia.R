##
## write.amelia - function for writing imputed datasets to file
##
## INPUTS: obj - output of class "amelia"
##         file.stem - the the stem of the filename to use (imp number added)
##         extension - extension to add after the imputation number
##         format - csv, dta or table for their respective writing fns
##         separate - TRUE: write to separate files, FALSE: write to
##                    long file
##         impvar - if separate is FALSE, the name of the imp number
##                   var
##         orig.data - if separate if FALSE, include original data?
##
##
## OUTPUTS: none (writes to file)
##

write.amelia <- function(obj, separate = TRUE, file.stem,
                         extension = NULL, format = "csv",
                         impvar = "imp", orig.data = TRUE, ...) {

  if(!(format %in% c("csv","table","dta"))) {
    stop("The writing format is not supported")
  }

  ## smart defaults for the extensions
  if (missing(extension)) {
    if (format == "dta") extension <- ".dta"
    if (format == "csv") extension <- ".csv"
  }

  m <- length(obj$imputations)
  Call <- match.call(expand.dots = TRUE)
  Call[[1]] <- as.name(paste("write",format, sep="."))

  ## these arugments should not be passed to write.format
  Call$obj <- NULL
  Call$file.stem <- NULL
  Call$extension <- NULL
  Call$format <- NULL
  Call$separate <- NULL
  Call$orig.data <- NULL
  Call$impvar <- NULL

  if (separate) {
    for (i in 1:m) {
      if (format == "dta")
        Call$dataframe <- obj$imputations[[i]]
      else
        Call$x <- obj$imputations[[i]]

      Call$file <- paste(file.stem, i, extension,sep="")
      eval.parent(Call)
    }
  } else {
    if (orig.data) {
      odata <- obj$imputations[[1]]
      is.na(odata) <- obj$missMatrix
      odata[, impvar] <- 0
    }
    obj$imputations[[1]][, impvar] <- 1

    if (orig.data) {
      obj$imputations[[1]] <- rbind(odata, obj$imputations[[1]])
    }
    if (format == "dta") {
      Call$dataframe <- obj$imputations[[1]]
    } else {
      Call$x <- obj$imputations[[1]]
    }
    for (i in 2:m) {
      obj$imputations[[i]][, impvar] <- i
      if (format == "dta") {
        Call$dataframe <- rbind(Call$dataframe, obj$imputations[[i]])
      } else {
        Call$x <- rbind(Call$x, obj$imputations[[i]])
      }
    }
    Call$file <- paste(file.stem, extension, sep = "")
    eval.parent(Call)
  }
  invisible()
}
