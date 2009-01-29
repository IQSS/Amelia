.First.lib <- function(...) {
  mylib     <- dirname(system.file(package = "Amelia"))
  ver       <- packageDescription("Amelia", lib = mylib)$Version
  builddate <- packageDescription("Amelia", lib = mylib)$Date
  curryear  <- format(Sys.time(), "%Y")
  cat("## \n## Amelia II: Multiple Imputation\n")
  cat(paste("## (Version ",ver,", built: ", builddate,")\n", sep=""))
  cat(paste("## Copyright (C) 2005-",curryear,
            " James Honaker, Gary King and Matthew Blackwell\n",sep=""))
  cat(paste("## Refer to http://gking.harvard.edu/amelia/",
            "for more information\n##"))
}
