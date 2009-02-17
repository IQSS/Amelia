##
## write.amelia - function for writing imputed datasets to file 
##
## INPUTS: obj - output of class "amelia"
##         file.stem - the the stem of the filename to use (imp number added)
##         extension - extension to add after the imputation number
##         format - csv, dta or table for their respective writing fns
##
## OUTPUTS: none (writes to file)
##

write.amelia <- function(obj, file.stem, extension=NULL, format="csv", ...) {

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
  
  for (i in 1:m) {
    if (format == "dta")
      Call$dataframe <- obj$imputation[[i]]
    else
      Call$x <- obj$imputation[[i]]
    
    Call$file <- paste(file.stem, i, extension,sep="")
    eval.parent(Call)
  }
  invisible()
}
