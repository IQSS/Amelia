.onAttach <- function(...) {
  mylib     <- dirname(system.file(package = "Amelia"))
  ver       <- packageVersion("Amelia")
  builddate <- packageDescription("Amelia")$Date
  curryear  <- format(Sys.time(), "%Y")
  mess <- c("## ", "## Amelia II: Multiple Imputation",
            paste("## (Version ",ver,", built: ", builddate,")", sep=""),
            paste("## Copyright (C) 2005-",curryear,
            " James Honaker, Gary King and Matthew Blackwell",sep=""),
            paste("## Refer to http://gking.harvard.edu/amelia/",
            "for more information"), "## ")
  mess <- paste(mess, collapse = "\n")
  packageStartupMessage(mess)

}
