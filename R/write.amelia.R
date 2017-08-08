#' Write Amelia imputations to file
#'
#'  @description {Writes the imptuted datasets to file from a run of \code{amelia}.}
#'
#'
#'  @param {obj}an object of class "amelia"; typically output from the
#'    function \code{amelia}.
#'  @param {separate}logical variable. If \code{TRUE} (default), the
#'    imputed datasets will be written to separate files, whose names come
#'    from the \code{file.stem} and \code{extension} arguments. If \code{FALSE},
#'    the imputations are stacked and written as a single file.
#'  @param file.stem}{the leading part of the filename to save to
#'    output The imputation number and \code{extension} will be added to
#'    complete the filename. This can include a directory path.
#' @param {extension}the extension of the filename. This is simply what
#'    follows \code{file.stem} and the imputation number.
#'  @param {format}one of the following output formats: \code{csv},
#'    \code{dta} or \code{table}. See details.
#'  @param {impvar}the name of imputation number variable written to the
#'    stacked dataset when \code{separate} is \code{FALSE}.
#'  @param {orig.data}logical variable indicating whether the original,
#'    unimputed dataset should be included in the stacked dataset when
#'    @param {separate} is \code{FALSE}.
#'  @param {\dots}further arguments for the \code{write} functions.
#'  
#'@details 
#' \code{write.amelia} writes the imputed datasets to a file or a set of files
#'  using one of the following functions: \code{write.csv},
#'  \code{write.dta}, or \code{write.table}. You can pass arguments to
#'  these functions from \code{write.amelia}.
#'  
#'  When \code{separate} is \code{TRUE}, each imputed dataset is written
#'  to its own file. If you were to set \code{file.stem} to
#'  \code{"outdata"} and the \code{extension} to \code{".csv"} , then the
#'  resulting filename of the written files will be
#'  \preformatted{
#'    outdata1.csv
#'    outdata2.csv
#'    outdata3.csv
#' 
#'  When \code{separate} is \code{FALSE}, the function adds a variable
#'  called \code{impvar} to each dataset which indicates the imputed
#'  dataset to which the row belongs. Then, each of the datasets are
#'  stacked together to create one dataset. If \code{orig.data} is \code{TRUE},
#'  then the original, unimputed dataset is included at the top of the
#'  stack, with its imputation number set to 0. 
#'
#'@seealso{\code{\link{write.csv}}, \code{\link{write.table}}, \code{\link{write.dta}}}



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
