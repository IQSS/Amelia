#' Interactive GUI for Amelia
#' 
#'@description Brings up the AmeliaView graphical interface, which allows users
#'             to load datasets, manage options and run Amelia from a traditional 
#'             windowed environment.

main.close<-function() {
  qvalue<-tcltk::tkmessageBox(parent=getAmelia("gui"), message="Are you sure you want to exit Amelia?",
                       icon="question",
                       type="okcancel",
                       default="cancel")
  if (tcltk::tclvalue(qvalue)=="ok") {
    tcltk::tkdestroy(getAmelia("gui"))
  }
}



setWorkingDir <- function() {
  newwd <- tcltk::tkchooseDirectory(parent = getAmelia("gui"),
                             initialdir = getwd(),
                             title = "Set output directory...",
                             mustexist = TRUE)
  if (tcltk::tclvalue(newwd) != "")
    setwd(tcltk::tclvalue(newwd))

  return(NULL)
}

loadStata <- function() {

  filetype <- c("{{Stata files} {.dta}} {{All files} *}")

  putAmelia("am.filename", tcltk::tclvalue(tcltk::tkgetOpenFile(parent=getAmelia("gui"), filetypes=filetype)))
  if (getAmelia("am.filename") == "")
    return(NULL)
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tcltk::tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tcltk::tclvalue(sure) == "no")
      return(NULL)
  }
  putAmelia("amelia.data",try(read.dta(getAmelia("am.filename"),convert.factors=FALSE)))
  putAmelia("am.filetype", "Stata")
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tcltk::tkmessageBox(parent=getAmelia("gui"), message="Failure in loading the data.  Try again.",icon="error",type="ok")
    putAmelia("amelia.data",NULL)
    return(NULL)
  }
  activateGUI()
}

loadSPSS <- function() {

  filetype <- c("{{SPSS} {.sav}} {{All files} *}")
  putAmelia("am.filename", tcltk::tclvalue(tcltk::tkgetOpenFile(parent=getAmelia("gui"), filetypes=filetype)))

  if (getAmelia("am.filename") == "")
    return(NULL)
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tcltk::tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tcltk::tclvalue(sure) == "no")
      return(NULL)
  }
  putAmelia("amelia.data",try(read.spss(getAmelia("am.filename"),use.value.labels=FALSE,to.data.frame=TRUE)))
  putAmelia("am.filetype", "SPSS")
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tcltk::tkmessageBox(parent=getAmelia("gui"), message="Failure in loading the data.  Try again.",icon="error",type="ok")
    putAmelia("amelia.data",NULL)
    return(NULL)
  }
  activateGUI()
}

loadSAS <- function() {

  filetype <- c("{{SAS Transport} {.xpt}} {{All files} *}")
  putAmelia("am.filename", tcltk::tclvalue(tcltk::tkgetOpenFile(parent=getAmelia("gui"), filetypes=filetype)))

  if (getAmelia("am.filename") == "")
    return(NULL)
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tcltk::tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tcltk::tclvalue(sure) == "no")
      return(NULL)
  }
  putAmelia("amelia.data",try(read.xport(getAmelia("am.filename"))))
  putAmelia("am.filetype", "SAS")
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tcltk::tkmessageBox(parent=getAmelia("gui"), message="Failure in loading the data.  Try again.",icon="error",type="ok")
    putAmelia("amelia.data",NULL)
    return(NULL)
  }
  activateGUI()
}

loadTAB <- function() {

  filetype <- c("{{Tab-delimited files} {.txt .tab .dat}} {{All files} *}")
  putAmelia("am.filename", tcltk::tclvalue(tcltk::tkgetOpenFile(parent=getAmelia("gui"), filetypes=filetype)))
  if (getAmelia("am.filename") == "")
    return(NULL)
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tcltk::tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tcltk::tclvalue(sure) == "no")
      return(NULL)
  }
  putAmelia("amelia.data",try(read.table(getAmelia("am.filename"),header=TRUE)))
  putAmelia("am.filetype", "TAB")
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tcltk::tkmessageBox(parent=getAmelia("gui"), message="Failure in loading the data.  Try again.",icon="error",type="ok")
    putAmelia("amelia.data",NULL)
    return(NULL)
  }
  activateGUI()
}

loadCSV <- function() {

  filetype <- c("{{Comma-delimited files} {.csv}} {{All files} *} ")
  putAmelia("am.filename", tcltk::tclvalue(tcltk::tkgetOpenFile(parent=getAmelia("gui"), filetypes=filetype)))
  if (getAmelia("am.filename") == "")
    return(NULL)
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tcltk::tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tcltk::tclvalue(sure) == "no")
      return(NULL)
  }
  putAmelia("amelia.data",try(read.csv(getAmelia("am.filename"),header=TRUE)))
  putAmelia("am.filetype", "CSV")
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tcltk::tkmessageBox(parent=getAmelia("gui"), message="Failure in loading the data.  Try again.",icon="error",type="ok")
    putAmelia("amelia.data",NULL)
    return(NULL)
  }
  activateGUI()
}


loadRData <- function() {
  onOK <- function() {
    putAmelia("amelia.data", eval(as.name(tcltk::tclvalue(tcltk::tkget(objectChooser)))))
    tcltk::tkdestroy(chooseObjectWindow)
    tcltk::tkfocus(getAmelia("gui"))
    tcltk::tkgrab.release(chooseObjectWindow)
    activateGUI()
    return()
  }
  onCancel <- function() {
    rm(list=getAmelia("amelia.data"))
    tcltk::tkdestroy(chooseObjectWindow)
    tcltk::tkfocus(getAmelia("gui"))
    tcltk::tkgrab.release(chooseObjectWindow)
    return()
  }

  filetype <- c("{{R Data files} {.RData .Rdata .Rda .rda}} {{All files} *} ")
  putAmelia("am.filename", tcltk::tclvalue(tcltk::tkgetOpenFile(parent=getAmelia("gui"), filetypes=filetype)))
  if (getAmelia("am.filename") == "")
    return(NULL)
  if (!is.null(getAmelia("amelia.data"))) {
    sure <- tcltk::tkmessageBox(parent = getAmelia("gui"),
                         message = "If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",
                         icon = "question", type = "yesno")
    if (tcltk::tclvalue(sure) == "no")
      return(NULL)
  }
  putAmelia("amelia.data",try(load(getAmelia("am.filename"))))
  putAmelia("am.filetype", "RData")
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tcltk::tkmessageBox(parent=getAmelia("gui"), message="Failure in loading the data.  Try again.",icon="error",type="ok")
    putAmelia("amelia.data",NULL)
    return(NULL)
  }
  if (length(getAmelia("amelia.data")) == 1) {
    putAmelia("amelia.data", eval(as.name(getAmelia("amelia.data"))))
  } else {
    datasets <- sapply(getAmelia("amelia.data"), function(x) is.data.frame(eval(as.name(x))))
    datasets <- getAmelia("amelia.data")[datasets]
    chooseObjectWindow <- tcltk::tktoplevel(parent=getAmelia("gui"))
    tcltk::tkwm.title(chooseObjectWindow, "Find Data Set")
    chooseFrame <- tcltk::ttkframe(chooseObjectWindow)
    objectChooser <- tcltk::ttkcombobox(chooseFrame, width = 20)
    tcltk::tkconfigure(objectChooser, values = datasets)
    tcltk::tkset(objectChooser, datasets[1])
    objectOK <- tcltk::ttkbutton(chooseFrame, text = "OK", width = 10, command = onOK)
    objectCancel <- tcltk::ttkbutton(chooseFrame, text = "Cancel", width = 10, command = onCancel)

    tcltk::tkgrid(tcltk::ttklabel(chooseFrame, text = "Please select your dataset from the following objects:"),
           row = 0, column = 0, columnspan = 2, padx = 10, pady = 10)
    tcltk::tkgrid(objectChooser, row = 1, column = 0, columnspan = 2, padx = 10, pady = 10)
    tcltk::tkgrid(objectOK, row = 2, column = 0, padx = 10, pady = 10)
    tcltk::tkgrid(objectCancel, row = 2, column = 1, padx = 10, pady = 10)
    tcltk::tkgrid(chooseFrame, padx = 10, pady = 10)
    tcltk::tkgrab(chooseObjectWindow)
    tcltk::tkfocus(chooseObjectWindow)
    tcltk::tkwm.protocol(chooseObjectWindow, "WM_DELETE_WINDOW", onCancel)
    centerModalDialog(chooseObjectWindow, resize=FALSE)
  }
  return()
}

loadDemo <- function(name) {
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tcltk::tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tcltk::tclvalue(sure) == "no")
      return(NULL)
  }
  data(list=name, package="Amelia", envir = ameliaEnv)
  putAmelia("amelia.data", eval(as.name(name)))
  putAmelia("am.filetype", "demo")
  putAmelia("am.filename", name)
  activateGUI()
}

drawMissMap <- function() {
  dev.new()
  missmap(getAmelia("amelia.data"), csvar = getAmelia("csvar"), tsvar = getAmelia("tsvar"))
}

activateGUI <- function(session = FALSE) {
  temp.list <- strsplit(getAmelia("am.filename"),"/")[[1]]
  if (getAmelia("am.filetype") != "demo") {
    temp.list <- strsplit(getAmelia("am.filename"),"/")[[1]]
    putAmelia("am.directory",
              paste(temp.list[-length(temp.list)],"",sep="/",collapse=""))
    setwd(getAmelia("am.directory"))
  } else {
    putAmelia("am.directory", getwd())
  }
  filename <- temp.list[length(temp.list)]
  dotList <- strsplit(filename, "\\.")[[1]]
  if (length(dotList) > 1)
    dotList <- dotList[-length(dotList)]
  filestub <- paste(paste(dotList, collapse = "."), "-imp", sep="")
  putAmelia("varnames" , names(getAmelia("amelia.data")))


  tcltk::tkgrid.remove(getAmelia("error.label"))
  tcltk::tkgrid.remove(getAmelia("allgood.label"))
  tcltk::tkgrid(getAmelia("noimps.label"), row = 2, column = 7,
         sticky ="e", padx = 10)
  ## Get rid of welcome frame
  if (as.logical(tcltk::tkwinfo("ismapped", getAmelia("gui.welcome")))) {
    tcltk::tkgrid.remove(getAmelia("gui.welcome"))
    tcltk::tkgrid(getAmelia("gui.skel"), row = 0, column = 0, sticky ="news")
    tcltk::tkgrid(getAmelia("statusbar"), sticky = "sew")
  }
  ## initialize values



  ## turn on various forms and buttons
  tcltk::tkconfigure(getAmelia("output.run"), state = "normal")
                                        #tcltk::tkconfigure(getAmelia("output.entry"), textvariable=getAmelia("outname"))
                                        #tcltk::tkconfigure(getAmelia("output.num"), textvariable=getAmelia("outnum"))
  tcltk::tkentryconfigure(getAmelia("main.menu.file"),"Edit Data...", state="normal")
  tcltk::tkentryconfigure(getAmelia("main.menu.options"),"Draw Missingness Map", state="normal")
  tcltk::tkentryconfigure(getAmelia("main.menu.file"),"Save Session...",
                   state = "normal")
  tcltk::tkentryconfigure(getAmelia("main.menu.options"),"Output File Type...",
                   state = "normal")
  tcltk::tkentryconfigure(getAmelia("main.menu.options"),"Output File Options...",
                   state = "normal")
  tcltk::tkconfigure(getAmelia("missmapButton"), state = "normal")
  tcltk::tkconfigure(getAmelia("editDataButton"), state = "normal")
  tcltk::tkconfigure(getAmelia("plotHistButton"), state = "normal")
  tcltk::tkconfigure(getAmelia("showLogButton"), state = "disabled")
  fillMainTree()


  ## Mark factors as ID by default.

  classes <- sapply(getAmelia("amelia.data"), class)
  factorVars <- which(classes == "factor" |
                      classes == "character")

  if (!session) {
    opt.holder <- vector("numeric",ncol(getAmelia("amelia.data")))
    names(opt.holder) <- getAmelia("varnames")
    putAmelia("noms", opt.holder)
    putAmelia("ords", opt.holder)
    putAmelia("logs", opt.holder)
    putAmelia("sqrt", opt.holder)
    putAmelia("lgstc", opt.holder)
    putAmelia("idvar", opt.holder)
    putAmelia("lags", opt.holder)
    putAmelia("leads", opt.holder)

    boundsholder <- matrix(NA, nrow = ncol(getAmelia("amelia.data")),
                           ncol = 3)
    boundsholder[,1] <- 1:ncol(getAmelia("amelia.data"))
    rownames(boundsholder) <- getAmelia("varnames")

    putAmelia("num.poly",tcltk::tclVar("0"))
    putAmelia("intercs",tcltk::tclVar("0"))
    putAmelia("priorsmat",  NULL)
    putAmelia("boundsmat",  boundsholder)
    putAmelia("max.resample", tcltk::tclVar("1000"))

    putAmelia("outname",  tcltk::tclVar(filestub))
    putAmelia("outnum",  tcltk::tclVar("5"))
    putAmelia("empri",  tcltk::tclVar("0"))
    putAmelia("tsvar", NULL)
    putAmelia("csvar", NULL)
    id.holder <- opt.holder
    id.holder[factorVars] <- 1
    putAmelia("idvar", id.holder)
    for (i in factorVars) {
      tcltk::tkset(getAmelia("main.tree"), getAmelia("varnames")[i],
            "transform", "ID")
    }
  } else {
    for (i in factorVars) {
      if (all(getAmelia("idvar")[i]==0,
              getAmelia("csvar")!=getAmelia("varnames")[i],getAmelia("noms")[i]==0)) {
        tcltk::tcl(getAmelia("main.tree"), "item", getAmelia("varnames")[i], image = getAmelia("redFlagIcon"))
      }
    }
  }
  tcltk::tkentryconfigure(getAmelia("main.menu.options"), "Add Observations Priors...", state="normal")

  tcltk::tkentryconfigure(getAmelia("main.menu.options"), "Numerical Options", state="normal")
  ## add the filename and rows/cols to statusbar
  tcltk::tkconfigure(getAmelia("statusbar.lab1b"), text = getAmelia("am.filename"), foreground = "blue")
  tcltk::tkconfigure(getAmelia("statusbar.n"), text = paste(nrow(getAmelia("amelia.data"))), foreground = "blue")
  tcltk::tkconfigure(getAmelia("statusbar.k"), text = paste(ncol(getAmelia("amelia.data"))), foreground = "blue")

}

save.session <- function() {
  if (is.null(getAmelia("amelia.data"))) {
    tcltk::tkmessageBox(parent=getAmelia("gui"), message="You must load a dataset before you can save a session.", icon="error", type="ok")
    return(NULL)
  }
  file.select <- tcltk::tclvalue(tcltk::tkgetSaveFile(parent=getAmelia("gui"),
                                        filetypes="{{RData files} {.RData}} {{All files} *}"))
  putAmelia("session.flag", TRUE)
  sessionList <- c("am.directory","amelia.data", "am.filename",
                   "am.filetype", "boundsmat", "csvar", "idvar", "lags",
                   "leads", "lgstc", "logs", "noms", "num.poly",
                   "ords", "outname.value", "outnum.value", "output.log", "outtype.value", "priorsmat",
                   "runState", "seed.value", "session.flag", "splinestime.value", "sqrt", "tol.value",
                   "tsvar", "empri.value", "intercs.value",
                   "max.resample.value", "ameliaObject")
  putAmelia("empri.value", tcltk::tclvalue(getAmelia("empri")))
  putAmelia("intercs.value", tcltk::tclvalue(getAmelia("intercs")))
  putAmelia("max.resample.value", tcltk::tclvalue(getAmelia("max.resample")))
  putAmelia("outname.value", tcltk::tclvalue(getAmelia("outname")))
  putAmelia("outnum.value", tcltk::tclvalue(getAmelia("outnum")))
  putAmelia("outtype.value", tcltk::tclvalue(getAmelia("outtype")))
  putAmelia("seed.value", tcltk::tclvalue(getAmelia("seed")))
  putAmelia("tol.value", tcltk::tclvalue(getAmelia("tol")))
  putAmelia("splinestime.value", tcltk::tclvalue(getAmelia("splinestime")))

  save(list = sessionList, envir=ameliaEnv, file = file.select)
  return(NULL)
}

load.session <- function() {

  ## diaglog to get RData file
  file.select <- tcltk::tclvalue(tcltk::tkgetOpenFile(parent=getAmelia("gui"), filetypes=
                                        "{{RData files} {.RData}} {{All files} *}"))
  if (nchar(file.select) <= 0)
    return(NULL)

  ## try loading the RData file and stop if it doesn't work
  tryloadsess <- try(load(file=file.select, envir=ameliaEnv), silent=TRUE)

  if (inherits(tryloadsess,"try-error")) {
    tcltk::tkmessageBox(parent=getAmelia("gui"),message="Error loading session.  This is not a valid session file.",icon="error",type="ok")
    return(NULL)
  }

  ## make sure that the RData file loaded the right list
  if (!("session.flag" %in% ls(ameliaEnv)) | !getAmelia("session.flag")) {
    tcltk::tkmessageBox(parent=getAmelia("gui"), message="Not an Amelia session file.  Try again.",icon="error",type="ok")
    return(NULL)
  }
  activateGUI(session = TRUE)

  nn <- ncol(getAmelia("amelia.data"))
  if (!is.null(getAmelia("tsvar"))) {
    tcltk::tcl(getAmelia("main.tree"), "item", getAmelia("tsvar"), image = getAmelia("clockIcon"))
    tcltk::tkentryconfigure(getAmelia("main.menu.options"),0, state="normal")
    for (i in 1:nn) {
      if (getAmelia("lags")[i] == 1)
        tcltk::tkset(getAmelia("main.tree"), getAmelia("varnames")[i], "lag", "X")
      if (getAmelia("leads")[i] == 1)
        tcltk::tkset(getAmelia("main.tree"), getAmelia("varnames")[i], "lead", "X")
    }
  }
  if (!is.null(getAmelia("csvar"))) {
    tcltk::tcl(getAmelia("main.tree"), "item", getAmelia("csvar"), image = getAmelia("userIcon"))
    tcltk::tkentryconfigure(getAmelia("main.menu.options"), 1, state="normal")
    tcltk::tkentryconfigure(getAmelia("main.menu.options"), 1, variable = getAmelia("intercs"))
  }

  for (i in 1:nn) {
    if (getAmelia("idvar")[i] == 1)
      tcltk::tkset(getAmelia("main.tree"), getAmelia("varnames")[i], "transform", "ID")
    if (getAmelia("ords")[i] == 1)
      tcltk::tkset(getAmelia("main.tree"), getAmelia("varnames")[i], "transform", "Ordinal")
    if (getAmelia("noms")[i] == 1)
      tcltk::tkset(getAmelia("main.tree"), getAmelia("varnames")[i], "transform", "Nominal")
    if (getAmelia("logs")[i] == 1)
      tcltk::tkset(getAmelia("main.tree"), getAmelia("varnames")[i], "transform", "Log")
    if (getAmelia("sqrt")[i] == 1)
      tcltk::tkset(getAmelia("main.tree"), getAmelia("varnames")[i], "transform", "Square Root")
    if (getAmelia("lgstc")[i] == 1)
      tcltk::tkset(getAmelia("main.tree"), getAmelia("varnames")[i], "transform", "Logistic")
  }
  for (i in 1:nn) {
    bdMin <- getAmelia("boundsmat")[i,2]
    bdMax <- getAmelia("boundsmat")[i,3]
    if (!is.na(bdMin)) {
      treeBounds <- paste("[",bdMin,", ", bdMax,"]", sep = "")
    } else {
      treeBounds <- ""
    }
    tcltk::tkset(getAmelia("main.tree"), getAmelia("varnames")[i], "bounds", treeBounds)
  }

  tcltk::tcl("set", getAmelia("seed"), getAmelia("seed.value"))
  tcltk::tcl("set", getAmelia("tol"), getAmelia("tol.value"))
  tcltk::tcl("set", getAmelia("empri"), getAmelia("empri.value"))
  tcltk::tcl("set", getAmelia("outname"), getAmelia("outname.value"))
  tcltk::tcl("set", getAmelia("outnum"), getAmelia("outnum.value"))
  tcltk::tcl("set", getAmelia("outtype"), getAmelia("outtype.value"))
  tcltk::tcl("set", getAmelia("intercs"), getAmelia("intercs.value"))
  tcltk::tcl("set", getAmelia("splinestime"), getAmelia("splinestime.value"))
  tcltk::tcl("set", getAmelia("max.resample"),
      getAmelia("max.resample.value"))

  tcltk::tkgrid.remove(getAmelia("noimps.label"))
  tcltk::tkgrid.remove(getAmelia("error.label"))
  tcltk::tkgrid.remove(getAmelia("allgood.label"))
  tcltk::tkgrid(getAmelia(paste(getAmelia("runState"),"label", sep = ".")),
         row = 2, column = 7, sticky ="e", padx = 10)
  if (getAmelia("runState") != "noimps") {
    tcltk::tkentryconfigure(getAmelia("main.menu.output"), "Output Log",
                     state="normal")
    tcltk::tkconfigure(getAmelia("showLogButton"), state = "normal")
  }
  if (getAmelia("runState") == "allgood") {
    tcltk::tkentryconfigure(getAmelia("main.menu.output"), 0,
                     state = "normal")
    tcltk::tkentryconfigure(getAmelia("main.menu.output"), 2,
                     state = "normal")
    resave <- tcltk::tkmessageBox(parent = getAmelia("gui"), message =
                           "Re-save imputed data sets to the working directory?", icon =
                           "question", default = "yes", type = "yesno")
    if (tcltk::tclvalue(resave) == "yes") {
      amelia.save(getAmelia("ameliaObject"),
                  tcltk::tclvalue(getAmelia("outname")), as.numeric(tcltk::tclvalue(getAmelia("outnum"))))
    }

  }
  return(NULL)
}


run.amelia <- function() {
  save.type <- as.numeric(tcltk::tclvalue(getAmelia("outtype")))
  if (file.access(getwd(), mode = 2) == -1 & !(save.type %in% c(0,6)))
    {
      tcltk::tkmessageBox(parent = getAmelia("gui"), message =
                   "The current working directory is not writable. Please select a different working directory or chose to not save the imputed data sets.",
                   type ="ok")
      return(NULL)

    }

  ## Let's not allow people to overwrite their data.
  temp.list <- strsplit(getAmelia("am.filename"),"/")[[1]]
  filename <- temp.list[length(temp.list)]
  outfiles <- paste(tcltk::tclvalue(getAmelia("outname")),
                    1:as.numeric(tcltk::tclvalue(getAmelia("outnum"))), sep
                    ="")
  save.type <- as.numeric(tcltk::tclvalue(getAmelia("outtype")))
  exten <- switch(save.type, "csv","txt","dta","dta","RData")
  outfiles <- paste(outfiles, exten, sep = ".")
  outfiles <- paste(paste(temp.list[-length(temp.list)], collapse =
                          "/"), outfiles, sep = "/")
  if (getAmelia("am.filename") %in% outfiles) {
    tcltk::tkmessageBox(parent = getAmelia("gui"), message =
                 "Current settings would overwrite the original data. Please change the output file name.",
                 icon = "error", type ="ok")
    return(NULL)
  }


  ts <- getAmelia("tsvar")
  cs <- getAmelia("csvar")
  nn <- ncol(getAmelia("amelia.data"))

  am.intercs  <- as.logical(as.numeric(tcltk::tclvalue(getAmelia("intercs"))))
  sptime <- as.numeric(tcltk::tclvalue(getAmelia("splinestime")))

  if (sptime == 0)
    if (am.intercs == FALSE)
      sptime <- NULL
  if (is.null(ts))
    sptime <- NULL
  if (is.null(cs))
    am.intercs <- FALSE

  id    <- getAmelia("varnames")[getAmelia("idvar")==1]
  ord   <- getAmelia("varnames")[getAmelia("ords")==1]
  nom   <- getAmelia("varnames")[getAmelia("noms")==1]
  logs  <- getAmelia("varnames")[getAmelia("logs")==1]
  sqrts <- getAmelia("varnames")[getAmelia("sqrt")==1]
  lgstc <- getAmelia("varnames")[getAmelia("lgstc")==1]
  amlags<- getAmelia("varnames")[getAmelia("lags")==1]
  amfut <- getAmelia("varnames")[getAmelia("leads")==1]

  if (length(id)   == 0) id   <- NULL
  if (length(ord)  == 0) ord  <- NULL
  if (length(nom)  == 0) nom  <- NULL
  if (length(logs) == 0) logs <- NULL
  if (length(sqrts)== 0) sqrts<- NULL
  if (length(lgstc)== 0) lgstc<- NULL
  if (length(amlags)==0) amlags <-  NULL
  if (length(amfut)== 0) amfut<- NULL

  pmat <- getAmelia("priorsmat")
  colnames(pmat) <- NULL
  rownames(pmat) <- NULL

  bdmat <- getAmelia("boundsmat")
  colnames(bdmat) <- NULL
  rownames(bdmat) <- NULL
  bdmat <- bdmat[!is.na(bdmat[,2]) & !is.na(bdmat[,3]),,drop=FALSE]
  if (nrow(bdmat) == 0)
    bdmat <- NULL

  tol <- as.numeric(tcltk::tclvalue(getAmelia("tol")))
  max.re <- as.numeric(tcltk::tclvalue(getAmelia("max.resample")))
  num.imp <- as.numeric(tcltk::tclvalue(getAmelia("outnum")))
  emp <- as.numeric(tcltk::tclvalue(getAmelia("empri")))
  if (!is.na(as.numeric(tcltk::tclvalue(getAmelia("seed")))))
    set.seed(as.numeric(tcltk::tclvalue(getAmelia("seed"))))
  tcltk::tkgrid.remove(getAmelia("noimps.label"))
  tcltk::tkgrid.remove(getAmelia("error.label"))
  tcltk::tkgrid.remove(getAmelia("allgood.label"))
  tcltk::tkgrid(getAmelia("runAmeliaProgress"), row = 2, column = 7,
         sticky ="e", padx = 10)
  amcall <- substitute(amelia(x = getAmelia("amelia.data"), m = num.imp,
                              idvars = id, ts = ts, cs= cs,
                              priors = pmat, lags = amlags, empri = emp,
                              intercs = am.intercs, leads = amfut,
                              splinetime = sptime,
                              logs = logs, sqrts = sqrts, lgstc = lgstc,
                              ords = ord, noms = nom, bounds = bdmat,
                              max.resample = max.re, tolerance= tol))

  putAmelia("output.log", c(getAmelia("output.log"),
                            sub("    ","\n       ",deparse(amcall, control=NULL, width.cutoff=60)),"\n\n"))
  putAmelia("wdForLastImputation", getwd())
  ## run amelia! or at least try, and put the output in a list
  ## the name of the list will be the output name set by user
  output.connection <- textConnection(".Output", open="w", local = TRUE)
  sink(output.connection, type="output")
  putAmelia("ameliaObject",
            try(amelia.default(x        = getAmelia("amelia.data"),
                               m        = as.numeric(tcltk::tclvalue(getAmelia("outnum"))),
                               p2s      = 1,
                               idvars   = id,
                               ts       = ts,
                               cs       = cs,
                               priors   = pmat,
                               lags     = amlags,
                               empri    = as.numeric(tcltk::tclvalue(getAmelia("empri"))),
                               intercs  = am.intercs,
                               leads    = amfut,
                               splinetime = sptime,
                               frontend = TRUE,
                               logs     = logs,
                               sqrts    = sqrts,
                               lgstc    = lgstc,
                               ords     = ord,
                               noms     = nom,
                               bounds   = bdmat,
                               max.resample = as.numeric(tcltk::tclvalue(getAmelia("max.resample"))),
                               tolerance= as.numeric(tcltk::tclvalue(getAmelia("tol")))),
                silent=TRUE))
  sink(type = "output")
  putAmelia("output.log", c(getAmelia("output.log"), paste(textConnectionValue(output.connection), "\n")))

  tcltk::tkgrid.remove(getAmelia("runAmeliaProgress"))
  tcltk::tkconfigure(getAmelia("runAmeliaProgress"), value = 0)
  ## check for errors in the process.
  if (inherits(getAmelia("ameliaObject"),"try-error")) {
    putAmelia("output.log", c(getAmelia("output.log"),"\nThere was an unexpected error in the execution of Amelia.  \nDouble check all inputs for errors and take note of the error message:\n\n"))
    putAmelia("output.log", c(getAmelia("output.log"),paste(getAmelia("ameliaObject"))))
                                        #tcltk::tkconfigure(getAmelia("pass.fail.label"), foreground = "red")
                                        #tmp <- getAmelia("pass.fail")
                                        #tcltk::tclvalue(tmp) <- "Error! See log."
    show.output.log()
    tcltk::tkentryconfigure(getAmelia("main.menu.output"), 1, state =
                     "normal")
    tcltk::tkconfigure(getAmelia("showLogButton"), state = "normal")
    tcltk::tkgrid(getAmelia("error.label"), row = 2, column = 7,
           sticky ="e", padx = 10)
    putAmelia("runState", "error")
    return(NULL)
  }
  if (all(getAmelia("ameliaObject")$code!=c(1,2))) {
    putAmelia("output.log", c(getAmelia("output.log"),"\n"))
    putAmelia("output.log", c(getAmelia("output.log"),paste("Amelia Error Code:",
                                                            getAmelia("ameliaObject")[[1]],"\n",
                                                            getAmelia("ameliaObject")[[2]])))
                                        #tcltk::tkconfigure(getAmelia("pass.fail.label"), foreground = "red")
                                        #tmp <- getAmelia("pass.fail")
                                        #tcltk::tclvalue(tmp) <- "Error! See log."
    show.output.log()
    tcltk::tkentryconfigure(getAmelia("main.menu.output"), 1, state =
                     "normal")
    tcltk::tkconfigure(getAmelia("showLogButton"), state = "normal")
    tcltk::tkgrid(getAmelia("error.label"), row = 2, column = 7,
           sticky ="e", padx = 10)
    putAmelia("runState", "error")
  } else {
    putAmelia("output.log", c(getAmelia("output.log"),"Amelia has run successfully.\n"))
    tcltk::tkentryconfigure(getAmelia("main.menu.output"), 0, state = "normal")
    tcltk::tkentryconfigure(getAmelia("main.menu.output"), 1, state =
                     "normal")
    tcltk::tkentryconfigure(getAmelia("main.menu.output"), 2, state = "normal")
    tcltk::tkconfigure(getAmelia("showLogButton"), state = "normal")
    amelia.save(getAmelia("ameliaObject"),
                tcltk::tclvalue(getAmelia("outname")), as.numeric(tcltk::tclvalue(getAmelia("outnum"))))
    tcltk::tkgrid(getAmelia("allgood.label"), row = 2, column = 7,
           sticky ="e", padx = 10)
    putAmelia("runState", "allgood")
  }

}

amelia.save <- function(out,outname,m)  {
  save.type <- as.numeric(tcltk::tclvalue(getAmelia("outtype")))
  if (save.type == 1) {
    write.amelia(out, file.stem = outname, format = "csv",
                 row.names = FALSE)
  }

  if (save.type == 2) {
    write.amelia(out, file.stem = outname, extension = "txt",
                 format = "table", row.names = FALSE)
  }
  if (save.type == 3) {
    write.amelia(out, file.stem = outname, format = "dta",
                 version = 6)
  }
  if (save.type == 4) {
    write.amelia(out, file.stem = outname, format = "dta",
                 version = 7)
  }
  if (save.type == 5) {
    write.amelia(out, file.stem = outname, format = "dta",
                 version = 8)
  }
  if (save.type == 6) {
    write.amelia(out, file.stem = outname, format = "dta",
                 version = 10)
  }
  if (save.type == 7) {
    write.amelia(out, file.stem = outname, format = "dta",
                 separate = FALSE, version = 10)
  }
  if (save.type == 8) {
    save(list = "ameliaObject", envir = ameliaEnv,
         file = paste(outname, ".RData", sep = ""))
  }
}

set.out<-function(...) {
  putAmelia("output.select",as.numeric(tcltk::tkget(getAmelia("output.drop.box"))))
}

setTS <- function() {
  tsvartemp <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"),"selection")), " ")[[1]]
  if (length(tsvartemp) > 1) {
    tcltk::tkmessageBox(parent=getAmelia("gui"), message="Only one variable can be set as the times-series variable.",icon="error",type="ok")
    return(NULL)
  }
  if (!is.null(getAmelia("csvar"))) {
    if (getAmelia("csvar") == tsvartemp) {
      tcltk::tkmessageBox(parent=getAmelia("gui"), message="A variable cannot be both the time-series and cross-section index.",icon="error",type="ok")
      return(NULL)
    }
  }
  if (!(sapply(getAmelia("amelia.data"), class)[tsvartemp] %in% c("numeric","integer"))) {
    tcltk::tkmessageBox(parent=getAmelia("gui"),
                 message="The time-series index must be numeric.",icon="error",type="ok")
    return(NULL)
  }
  children <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"),"children","")), " ")[[1]]
  for(i in setdiff(children, getAmelia("csvar")))
    tcltk::tcl(getAmelia("main.tree"), "item", i , image="")

  tcltk::tcl(getAmelia("main.tree"), "item", tsvartemp, image = getAmelia("clockIcon"))
  putAmelia("tsvar", tsvartemp)
  tcltk::tkentryconfigure(getAmelia("main.menu.options"),0, state="normal")
  dropTrans()
}

unsetTS <- function() {
  tsvartemp <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"),"selection")), " ")[[1]]
  sure<-tcltk::tkmessageBox(parent=getAmelia("gui"), message="If you unset the time-series variable, you will lose any time-series settings such as lags, leads, or polynomials of time. Unset the time-series variable?",icon="question",type="yesno")
  if (tcltk::tclvalue(sure) == "no")
    return(NULL)

  tcltk::tcl(getAmelia("main.tree"), "item", tsvartemp, image = "")
  putAmelia("tsvar", NULL)
  tcltk::tkentryconfigure(getAmelia("main.menu.options"),0, state="disabled")
  putAmelia("lags",vector("numeric",ncol(getAmelia("amelia.data"))))
  putAmelia("leads",vector("numeric",ncol(getAmelia("amelia.data"))))
  children <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"),"children","")), " ")[[1]]
  for(i in children) {
    tcltk::tkset(getAmelia("main.tree"), i, "lag", "")
    tcltk::tkset(getAmelia("main.tree"), i, "lead", "")
  }
}

unsetCS <- function() {
  csvartemp <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"),"selection")), " ")[[1]]
  sure<-tcltk::tkmessageBox(parent=getAmelia("gui"), message="If you unset the cross-section variable, you will lose any cross-section settings. Unset the cross-section variable?",icon="question",type="yesno")
  if (tcltk::tclvalue(sure) == "no")
    return(NULL)

  tcltk::tcl(getAmelia("main.tree"), "item", csvartemp, image = "")
  putAmelia("csvar", NULL)
  tcltk::tkentryconfigure(getAmelia("main.menu.options"),0, state="normal")
  if (is.factor(getAmelia("amelia.data")[,csvartemp]) |
      is.character(getAmelia("amelia.data")[,csvartemp])) {
    tcltk::tcl(getAmelia("main.tree"), "item", csvartemp, image = getAmelia("redFlagIcon"))
  }
}

setCS <- function() {
  csvartemp <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"),"selection")), " ")[[1]]
  if (length(csvartemp) > 1) {
    tcltk::tkmessageBox(parent=getAmelia("gui"), message="Only one variable can be set as the cross-section variable.",icon="error",type="ok")
    return(NULL)
  }
  if (!is.null(getAmelia("tsvar"))) {
    if (getAmelia("tsvar") == csvartemp) {
      tcltk::tkmessageBox(parent=getAmelia("gui"), message="A variable cannot be both the time-series and cross-section index.",icon="error",type="ok")
      return(NULL)
    }
  }

  if (!is.null(getAmelia("csvar"))) {
    if (is.factor(getAmelia("amelia.data")[,getAmelia("csvar")]) |
        is.character(getAmelia("amelia.data")[,getAmelia("csvar")])) {
      tcltk::tcl(getAmelia("main.tree"), "item", getAmelia("csvar"), image = getAmelia("redFlagIcon"))
    } else {
      tcltk::tcl(getAmelia("main.tree"), "item", getAmelia("csvar"), image = "")
    }
  }

  dropTrans()
  tcltk::tcl(getAmelia("main.tree"), "item", csvartemp, image = getAmelia("userIcon"))
  putAmelia("csvar", csvartemp)
  tcltk::tkentryconfigure(getAmelia("main.menu.options"),1,state="normal")
  tcltk::tkentryconfigure(getAmelia("main.menu.options"), 1, variable = getAmelia("intercs"))

}


fillMainTree <- function() {
  children <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"),"children","")), " ")[[1]]
  tcltk::tkdelete(getAmelia("main.tree"), children)
  for (i in names(getAmelia("amelia.data"))) {

    if (is.factor(getAmelia("amelia.data")[,i]) |
        is.character(getAmelia("amelia.data")[,i])) {
      vals <- c("","","","","(factor)","...","...","...")
      vals <- c(vals,paste(sum(is.na(getAmelia("amelia.data")[,i])),
                           nrow(getAmelia("amelia.data")), sep="/"))
    } else {
      vals <- c(min(getAmelia("amelia.data")[,i],na.rm=T), max(getAmelia("amelia.data")[,i],na.rm=T),
                mean(getAmelia("amelia.data")[,i],na.rm=T), sd(getAmelia("amelia.data")[,i],na.rm=T))
      vals <- signif(vals, digits = 4)
      vals <- c("","","","", vals, paste(sum(is.na(getAmelia("amelia.data")[,i])),
                                         nrow(getAmelia("amelia.data")), sep="/"))
    }
    tcltk::tkinsert(getAmelia("main.tree"),"","end", id = i,tag="normal",text
             = i, values = vals)
  }
  bandTree()
  return()
}

AmeliaView<-function() {

  ##Preamble
  requireNamespace("tcltk") || stop("The package 'tcltk' is required")

  if (.Platform$OS.type != "windows") {
    tcltk::tcl("ttk::style", "theme", "use", "clam")
    tcltk::tkfont.configure("TkHeadingFont", weight="normal")
    tcltk::tkfont.configure("TkCaptionFont", weight="normal")
  }

  ## If the current working directory is not writable, move to a
  ## sensible default locations: the HOME dir
  if (file.access(getwd(), mode = 2) == -1) {
    if (file.access(Sys.getenv("HOME"), mode = 0) == 0 &
        file.access(Sys.getenv("HOME"), mode = 2) == 0) {
      setwd(Sys.getenv("HOME"))
    }

  }
  tcltk::tclServiceMode(on=FALSE)
  putAmelia("outname",    tcltk::tclVar("outdata"))
  putAmelia("outnum",     tcltk::tclVar("5"))
  putAmelia("empri",      tcltk::tclVar("0"))
  putAmelia("tol",        tcltk::tclVar("0.0001"))
  putAmelia("amelia.data",NULL)
  putAmelia("am.filename",NULL)
  putAmelia("varnames",   NULL)
  putAmelia("tsvar",      NULL)
  putAmelia("csvar",      NULL)
  putAmelia("varmin",     NULL)
  putAmelia("varmax",     NULL)
  putAmelia("runState", "noimps")
  putAmelia("session.flag", FALSE)
  putAmelia("intercs",tcltk::tclVar("0"))
  putAmelia("splinestime",tcltk::tclVar("0"))
  putAmelia("outtype", tcltk::tclVar("1"))
  putAmelia("max.resample", tcltk::tclVar("1000"))
  putAmelia("inname",     tcltk::tclVar(""))
  putAmelia("seed",       tcltk::tclVar(""))
  putAmelia("output.log", NULL)
  putAmelia("boundMin", tcltk::tclVar(""))
  putAmelia("boundMax", tcltk::tclVar(""))
  putAmelia("wdForLastImputation", getwd())

  output.types <- c("(no save)",
                    "CSV",
                    "Tab Delimited",
                    "Stata 6",
                    "Stata 7",
                    "Stata 8/9",
                    "Stata 10+",
                    "Stata 10+ (stacked)",
                    "RData")
  ampath <- find.package(package = "Amelia")[1]
  ameliaFile <- file.path(ampath, "gui/gallery19.gif")
  goFile <- file.path(ampath, "gui/action_go.gif")
  tableFile <- file.path(ampath, "gui/table.gif")
  rFile <- file.path(ampath, "gui/page-R.gif")
  dtaFile <- file.path(ampath, "gui/page_dta.gif")
  spssFile <- file.path(ampath, "gui/page_spss.gif")
  clockFile <- file.path(ampath, "gui/icon_clock.gif")
  userFile <- file.path(ampath, "gui/icon_user.gif")
  upFile <- file.path(ampath, "gui/arrow_up.gif")
  downFile <- file.path(ampath, "gui/arrow_down.gif")
  worldFile <- file.path(ampath, "gui/icon_world.gif")
  pageTextFile <- file.path(ampath, "gui/page_text.gif")
  pageEditFile <- file.path(ampath, "gui/page_edit.gif")
  histFile <- file.path(ampath, "gui/histogram.gif")
  saveFile <- file.path(ampath, "gui/action_save.gif")
  pageUpFile <- file.path(ampath, "gui/page_up.gif")
  redStopFile <- file.path(ampath, "gui/action_stop.gif")
  redFlagFile <- file.path(ampath, "gui/flag_red.gif")
  greenCheckFile <- file.path(ampath, "gui/icon_accept.gif")

  putAmelia("ameliaPic", tcltk::tkimage.create("photo", file=ameliaFile))
  putAmelia("action.go.icon", tcltk::tkimage.create("photo", file = goFile))
  putAmelia("tablePic", tcltk::tkimage.create("photo", file = tableFile))
  putAmelia("rPic", tcltk::tkimage.create("photo", file = rFile))
  putAmelia("dtaPic", tcltk::tkimage.create("photo", file = dtaFile))
  putAmelia("spssPic", tcltk::tkimage.create("photo", file = spssFile))
  putAmelia("clockIcon", tcltk::tkimage.create("photo", file = clockFile))
  putAmelia("userIcon", tcltk::tkimage.create("photo", file = userFile))
  putAmelia("worldIcon", tcltk::tkimage.create("photo", file = worldFile))
  putAmelia("upArrowIcon", tcltk::tkimage.create("photo", file = upFile))
  putAmelia("downArrowIcon", tcltk::tkimage.create("photo", file = downFile))
  putAmelia("histIcon", tcltk::tkimage.create("photo", file = histFile))
  putAmelia("saveIcon", tcltk::tkimage.create("photo", file = saveFile))
  putAmelia("pageUpIcon", tcltk::tkimage.create("photo", file = pageUpFile))
  putAmelia("redFlagIcon", tcltk::tkimage.create("photo", file =
                                          redFlagFile))
  putAmelia("redStopIcon", tcltk::tkimage.create("photo", file = redStopFile))
  putAmelia("greenCheckIcon", tcltk::tkimage.create("photo", file = greenCheckFile))
  putAmelia("pageTextIcon", tcltk::tkimage.create("photo", file =
                                           pageTextFile))
  putAmelia("pageEditIcon", tcltk::tkimage.create("photo", file = pageEditFile))
  putAmelia("gui", tcltk::tktoplevel())
  tcltk::tkwm.title(getAmelia("gui"), "AmeliaView")
  tcltk::tkwm.protocol(getAmelia("gui"),"WM_DELETE_WINDOW", function() main.close())
  tcltk::tkwm.geometry(getAmelia("gui"), "800x500")
  ##Menu
  putAmelia("main.menu", tcltk::tkmenu(getAmelia("gui")))
  putAmelia("main.menu.file", tcltk::tkmenu(getAmelia("main.menu"), tearoff=0))
  putAmelia("main.menu.demo", tcltk::tkmenu(getAmelia("main.menu"), tearoff=0))
  putAmelia("main.menu.import", tcltk::tkmenu(getAmelia("main.menu"), tearoff=0))
  putAmelia("main.menu.options", tcltk::tkmenu(getAmelia("main.menu"), tearoff=0))
  putAmelia("main.menu.splines", tcltk::tkmenu(getAmelia("main.menu"), tearoff=0))
  putAmelia("main.menu.output", tcltk::tkmenu(getAmelia("main.menu"), tearoff=0))
  putAmelia("main.menu.help", tcltk::tkmenu(getAmelia("main.menu"), tearoff=0))
  putAmelia("main.menu.variables", tcltk::tkmenu(getAmelia("main.menu"), tearoff=0,
                                          postcommand = variableOptionsPost))
  putAmelia("main.menu.trans", tcltk::tkmenu(getAmelia("main.menu"), tearoff=0))
  putAmelia("main.menu.outfile", tcltk::tkmenu(getAmelia("main.menu"), tearoff=0))
  tcltk::tkadd(getAmelia("main.menu.file"),"command",label="Load R Data File...",command=function()loadRData(),
        underline = 5)
  tcltk::tkadd(getAmelia("main.menu.import"),"command",label="Import comma-separated value data...",
        command=loadCSV, underline = 7)
  tcltk::tkadd(getAmelia("main.menu.import"),"command",label="Import tab-delimited data...",
        command=loadTAB, underline = 7)
  tcltk::tkadd(getAmelia("main.menu.import"),"command",label="Import Stata dta file...",
        command=loadStata, underline = 13)
  tcltk::tkadd(getAmelia("main.menu.import"),"command",label="Import SPSS data...",
        command=loadSPSS, underline = 7)
  tcltk::tkadd(getAmelia("main.menu.import"),"command",label="Import SAS Transport data...",
        command=loadSAS, underline = 8)
  tcltk::tkadd(getAmelia("main.menu.file"),"cascade",menu=getAmelia("main.menu.import"),label="Import Data",
        underline = 0)
  tcltk::tkadd(getAmelia("main.menu.demo"),"command",label="africa", command=function()
        loadDemo(name="africa"), underline = 0)
  tcltk::tkadd(getAmelia("main.menu.demo"),"command",label="freetrade", command=function()
        loadDemo(name="freetrade"), underline = 0)
  tcltk::tkadd(getAmelia("main.menu.file"),"cascade",menu=getAmelia("main.menu.demo"),label="Load Package Data",
        underline = 5)
  tcltk::tkadd(getAmelia("main.menu.file"),"command",command =
        setWorkingDir,label="Set Working Directory...", underline = 4)
  tcltk::tkadd(getAmelia("main.menu.file"),"command",label="Edit Data...",
        command=function(){putAmelia("amelia.data",
          edit(getAmelia("amelia.data")));updateTreeStats()},state="disabled",
        underline = 0)
  tcltk::tkadd(getAmelia("main.menu.file"),"separator")
  tcltk::tkadd(getAmelia("main.menu.file"),"command",label="Load Session...",command=function()load.session(),
        underline = 0)
  tcltk::tkadd(getAmelia("main.menu.file"),"command",label="Save Session...",command=function()save.session(),
        state="disabled", underline = 0)
  tcltk::tkadd(getAmelia("main.menu.file"),"separator")
  tcltk::tkadd(getAmelia("main.menu.file"),"command",label="Quit Amelia",command=function()main.close(),
        underline = 0)
  tcltk::tkadd(getAmelia("main.menu.variables"), "command", label =
        "Set as Time-Series Variable", command = setTS, state = "disabled",
        underline = 0)
  tcltk::tkadd(getAmelia("main.menu.variables"), "command", label =
        "Set as Cross-Section Variable", command = setCS, state =
        "disabled", underline = 7)
  tcltk::tkadd(getAmelia("main.menu.variables"), "command", label =
        "Unset as Time-Series Variable", command = unsetTS, state =
        "disabled", underline = 0)
  tcltk::tkadd(getAmelia("main.menu.variables"), "command", label =
        "Unset as Cross-Section Variable", command = unsetCS, state =
        "disabled", underline = 23)
  tcltk::tkadd(getAmelia("main.menu.variables"),"separator")
  tcltk::tkadd(getAmelia("main.menu.variables"), "command", label = "Add Lag", command =
        function() addLag(), state = "disabled", underline = 0)
  tcltk::tkadd(getAmelia("main.menu.variables"), "command", label = "Add Lead", command =
        function() addLead(), state = "disabled", underline = 4)
  tcltk::tkadd(getAmelia("main.menu.variables"), "command", label = "Remove Lag", command
        = function() dropLag(), state = "disabled", underline = 0)
  tcltk::tkadd(getAmelia("main.menu.variables"), "command", label = "Remove Lead", command
        = function() dropLead(), state = "disabled", underline = 1)
  tcltk::tkadd(getAmelia("main.menu.variables"),"separator")

  tcltk::tkadd(getAmelia("main.menu.variables"), "command", label =
        "Plot Histogram(s) of Selected", command = plotHist, state =
        "disabled", underline = 0)
  tcltk::tkadd(getAmelia("main.menu.trans"), "command", label = "Log", command =
        function(x) setTrans("logs"), underline = 0)
  tcltk::tkadd(getAmelia("main.menu.trans"), "command", label = "Square Root", command =
        function(x) setTrans("sqrt"), underline = 0)
  tcltk::tkadd(getAmelia("main.menu.trans"), "command", label = "Logistic", command =
        function(x) setTrans("lgstc"), underline = 1)
  tcltk::tkadd(getAmelia("main.menu.trans"), "command", label = "Nominal", command =
        function(x) setTrans("noms"), underline = 0)
  tcltk::tkadd(getAmelia("main.menu.trans"), "command", label = "Ordinal", command =
        function(x) setTrans("ords"), underline = 0)
  tcltk::tkadd(getAmelia("main.menu.trans"), "command", label = "ID Variable", command =
        function(x) setTrans("idvar"), underline = 0)
  tcltk::tkadd(getAmelia("main.menu.variables"), "cascade", label =
        "Add Transformation...", menu = getAmelia("main.menu.trans"), state = "disabled",
        underline = 4)
  tcltk::tkadd(getAmelia("main.menu.variables"), "command", label =
        "Remove Transformations", command = dropTrans, state =
        "disabled", underline = 2)
  tcltk::tkadd(getAmelia("main.menu.variables"),"separator")
  tcltk::tkadd(getAmelia("main.menu.variables"), "command", label =
        "Add or Edit Bounds", command = addBounds, state = "disabled",
        underline = 12)
  for (i in 0:10)
    tcltk::tkadd(getAmelia("main.menu.splines"), "radiobutton", variable =
          getAmelia("splinestime"), label = paste(i,"knots"), value = i,
          underline = 0)
  tcltk::tkadd(getAmelia("main.menu.options"), "cascade", label =
        "Splines of Time with...", menu = getAmelia("main.menu.splines"),
        state="disabled", underline = 0)
  tcltk::tkadd(getAmelia("main.menu.options"), "checkbutton", label =
        "Interact Spline With Cross-Section?", variable =
        getAmelia("intercs"), onvalue=1,offvalue=0, state="disabled",
        underline = 0)

  tcltk::tkadd(getAmelia("main.menu.options"),"separator")
  tcltk::tkadd(getAmelia("main.menu.options"),"command", label =
        "Add Observations Priors...", command = gui.pri.setup,
        state="disabled", underline = 17)
  tcltk::tkadd(getAmelia("main.menu.options"), "separator")
  tcltk::tkadd(getAmelia("main.menu.options"), "command", label = "Numerical Options",
        command = buildNumericalOptions, state = "disabled", underline
        = 0)
  tcltk::tkadd(getAmelia("main.menu.options"), "command", label = "Draw Missingness Map",
        command = drawMissMap, state="disabled", underline = 5)
  tcltk::tkadd(getAmelia("main.menu.options"), "command", label = "Output File Options...",
        command = buildOutputOptions, state = "disabled", underline = 0)
  for (i in 1:length(output.types)) {
    tcltk::tkadd(getAmelia("main.menu.outfile"), "radiobutton", variable =
          getAmelia("outtype"), label = output.types[i], value = i-1)
  }
  tcltk::tkadd(getAmelia("main.menu.options"), "cascade", label = "Output File Type...",
        menu = getAmelia("main.menu.outfile"), state = "disabled", underline = 7)
  tcltk::tkadd(getAmelia("main.menu.output"),"command", label =
        "Imputation Diagnostics...", command = gui.diag.setup,
        state="disabled", underline = 11)
  tcltk::tkadd(getAmelia("main.menu.output"),"command", label = "Output Log", command =
        show.output.log, state="disabled", underline = 0)
  tcltk::tkadd(getAmelia("main.menu.output"),"command", label =
        "Open Folder Containing Imputated Data", command =
        showImputedFiles, state="disabled", underline = 12)
  tcltk::tkadd(getAmelia("main.menu.help"),"command",label="Amelia Website",command=
        function()browseURL("http://gking.harvard.edu/amelia/"),
        underline = 7)
  tcltk::tkadd(getAmelia("main.menu.help"),"command",label="Documentation",command=
        function() browseURL("http://gking.harvard.edu/amelia/docs/"),
        underline = 0)

  tcltk::tkadd(getAmelia("main.menu.help"),"command",label="About...",command=
        function()buildAboutDialog(), underline = 0)

  tcltk::tkadd(getAmelia("main.menu"),"cascade",label="File",
        menu = getAmelia("main.menu.file"), underline = 0)
  tcltk::tkadd(getAmelia("main.menu"),"cascade",label="Variables",
        menu = getAmelia("main.menu.variables"), underline = 0)
  tcltk::tkadd(getAmelia("main.menu"),"cascade",label="Options",
        menu = getAmelia("main.menu.options"), underline = 0)
  tcltk::tkadd(getAmelia("main.menu"),"cascade",label="Output",
        menu = getAmelia("main.menu.output"), underline = 1)
  tcltk::tkadd(getAmelia("main.menu"),"cascade",label="Help",
        menu = getAmelia("main.menu.help"), underline = 0)
  tcltk::tkconfigure(getAmelia("gui"), menu = getAmelia("main.menu"))


  ## Welcome Screen
  putAmelia("gui.welcome", tcltk::ttkframe(getAmelia("gui")))
  ameliaPicLabel <- tcltk::ttklabel(getAmelia("gui.welcome"), relief = "groove", image = getAmelia("ameliaPic"))
  loadRButton <- tcltk::ttkbutton(getAmelia("gui.welcome"), text = "Load R Data",
                           image = getAmelia("rPic"), compound = "top",
                           command = loadRData)
  loadCSVButton <- tcltk::ttkbutton(getAmelia("gui.welcome"), text = "Import CSV",
                             image = getAmelia("tablePic"), compound = "top",
                             command = loadCSV)
  loadStataButton <- tcltk::ttkbutton(getAmelia("gui.welcome"), text = "Import STATA",
                               image = getAmelia("dtaPic"), compound = "top",
                               command = loadStata)
  loadSPSSButton <- tcltk::ttkbutton(getAmelia("gui.welcome"), text = "Import SPSS",
                              image = getAmelia("spssPic"), compound = "top",
                              command = loadSPSS)
  loadDemoButton <- tcltk::ttkbutton(getAmelia("gui.welcome"), text = "Load Demo",
                              image = getAmelia("tablePic"), compound = "top",
                              command = function () loadDemo(name = "africa"))

  tcltk::tkgrid(ameliaPicLabel, row = 0, column = 0, columnspan = 6, padx =
         10, pady = 10)
  tcltk::tkgrid(tcltk::ttklabel(getAmelia("gui.welcome"),
                  text=paste("Welcome to AmeliaView ",packageDescription("Amelia",
                    fields="Version"), "!", sep="")),
         row = 1, column = 0, columnspan = 6, padx = 10, pady = 10)
  tcltk::tkgrid(tcltk::ttklabel(getAmelia("gui.welcome"), text="Please load a dataset:"),
         row = 2, column = 0, columnspan = 6, padx = 10, pady = 10)
  tcltk::tkgrid(loadRButton, row = 3, column = 0, padx = 10, pady = 10)
  tcltk::tkgrid(loadCSVButton, row = 3, column = 1, padx = 10, pady = 10)
  tcltk::tkgrid(loadStataButton, row = 3, column = 2, padx = 10, pady = 10)
  tcltk::tkgrid(loadSPSSButton, row = 3, column = 3, padx = 10, pady = 10)
  tcltk::tkgrid(loadDemoButton, row = 3, column = 4, padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("gui.welcome"), row = 0, column = 0)
  ##Frame
  putAmelia("gui.skel", tcltk::ttkpanedwindow(getAmelia("gui"), orient = "vertical"))

###############
### Toolbar ###
###############

  toolbar <- tcltk::ttkframe(getAmelia("gui.skel"))
  putAmelia("loadSessionButton",
            tcltk::ttkbutton(toolbar, text = "Load Session",
                      command = load.session, image = getAmelia("pageUpIcon"), compound = "top",
                      style="Toolbutton"))
  putAmelia("saveSessionButton",
            tcltk::ttkbutton(toolbar, text = "Save Session",
                      command = save.session, image = getAmelia("saveIcon"), compound = "top",
                      style="Toolbutton"))
  putAmelia("plotHistButton",
            tcltk::ttkbutton(toolbar, text = "Plot Histogram", state =
                      "disabled", command = plotHist, image = getAmelia("histIcon"), compound = "top",
                      style="Toolbutton"))
  putAmelia("editDataButton",
            tcltk::ttkbutton(toolbar, text = "Edit Data", state = "disabled",
                      command = function(){putAmelia("amelia.data", edit(getAmelia("amelia.data")));updateTreeStats()}, image =
                      getAmelia("pageEditIcon"), compound = "top",
                      style="Toolbutton"))
  putAmelia("missmapButton",
            tcltk::ttkbutton(toolbar, text = "Missingness Map", state = "disabled",
                      command = drawMissMap, image =
                      getAmelia("worldIcon"), compound = "top",
                      style="Toolbutton"))
  putAmelia("output.run",
            tcltk::ttkbutton(toolbar,text="Impute!", state = "disabled",
                      command = run.amelia, image =
                      getAmelia("action.go.icon"), compound = "top",
                      style="Toolbutton"))

  putAmelia("showLogButton",
            tcltk::ttkbutton(toolbar, text = "Output Log", state = "disabled",
                      command = show.output.log, image =
                      getAmelia("pageTextIcon"), compound = "top",
                      style="Toolbutton"))
  tcltk::tkgrid(getAmelia("loadSessionButton"), row =0, column = 0, sticky = "ew")
  tcltk::tkgrid(getAmelia("saveSessionButton"), row =0, column = 1, sticky = "ew")
  tcltk::tkgrid(tcltk::ttkseparator(toolbar, orient = "vertical"), row = 0, column =
         2, padx=5, pady=5, sticky="ns")
  tcltk::tkgrid(getAmelia("plotHistButton"), row = 0, column = 3, sticky = "ew")
  tcltk::tkgrid(getAmelia("editDataButton"), row = 0, column = 4, sticky = "ew")
  tcltk::tkgrid(getAmelia("missmapButton"), row = 0, column = 5, sticky="ew")
  tcltk::tkgrid(tcltk::ttkseparator(toolbar, orient = "vertical"), row = 0, column =
         6, padx=5, pady=5, sticky="ns")
  tcltk::tkgrid(getAmelia("output.run"), row = 0 , column = 7, sticky = "ew")
  tcltk::tkgrid(getAmelia("showLogButton"), row = 0, column = 8, sticky = "ew")

##########################
### Variable Dashboard ###
##########################

  dashboard   <- tcltk::ttkframe(getAmelia("gui.skel"))

  yscr <- tcltk::ttkscrollbar(dashboard,  orient = "vertical",
                       command=function(...)tcltk::tkyview(getAmelia("main.tree"),...))
  xscr <- tcltk::ttkscrollbar(dashboard, orient = "horizontal",
                       command=function(...)tcltk::tkxview(getAmelia("main.tree"),...))

  sorts <- rep(FALSE, times = 10)
  names(sorts) <- c("#0","transform","lag", "lead","bounds", "min", "max",
                    "mean", "sd", "miss")
  putAmelia("sortDirs", sorts)
  putAmelia("main.tree", tcltk::ttktreeview(dashboard, columns =
                                     "transform lag lead bounds  min max mean sd miss",
                                     yscrollcommand=function(...)tcltk::tkset(yscr,...), xscrollcommand=function(...)tcltk::tkset(xscr,...),
                                     selectmode = "extended"))

                                        #putAmelia("sum.right.click",tcltk::tkmenu(getAmelia("main.tree"), tearoff = FALSE) )
                                        #tcltk::tkadd(getAmelia("sum.right.click"), "command", label = "Plot Histogram of Selected", command = function() sum.plot())
                                        #tcltk::tkbind(getAmelia("main.tree"), "<Button-3>", RightClick)
                                        #putAmelia("sum.right.dis",tcltk::tkmenu(getAmelia("main.tree"), tearoff = FALSE) )
                                        #tcltk::tkadd(getAmelia("sum.right.dis"), "command", label = "Plot Histogram of Selected", state = "disabled")

  tcltk::tcl(getAmelia("main.tree"), "column", "#0", width = 70, minwidth = 80)
  tcltk::tcl(getAmelia("main.tree"), "column", 0, width = 78, minwidth = 78, anchor = "center")
  tcltk::tcl(getAmelia("main.tree"), "column", 1, width = 20, minwidth = 20, anchor = "center")
  tcltk::tcl(getAmelia("main.tree"), "column", 2, width = 20, minwidth = 20,
      anchor = "center")
  tcltk::tcl(getAmelia("main.tree"), "column", 3, width = 50, minwidth = 50, anchor = "e")
  tcltk::tcl(getAmelia("main.tree"), "column", 4, width = 50, minwidth = 50, anchor = "e")
  tcltk::tcl(getAmelia("main.tree"), "column", 5, width = 50, minwidth = 50, anchor = "e")
  tcltk::tcl(getAmelia("main.tree"), "column", 6, width = 50, minwidth = 50, anchor = "e")
  tcltk::tcl(getAmelia("main.tree"), "column", 7, width = 50, minwidth = 50, anchor = "e")
  tcltk::tcl(getAmelia("main.tree"), "column", 8, width = 50, minwidth = 50, anchor = "e")

  tcltk::tcl(getAmelia("main.tree"), "heading", "#0", text = "Variable",
      command = function() sortTreeBy("#0"))
  tcltk::tcl(getAmelia("main.tree"), "heading", 0, text = "Transformation",
      command = function() sortTreeBy("transform"))
  tcltk::tcl(getAmelia("main.tree"), "heading", 1, text = "Lag",
      command = function() sortTreeBy("lag"))
  tcltk::tcl(getAmelia("main.tree"), "heading", 2, text = "Lead",
      command = function() sortTreeBy("lead"))
  tcltk::tcl(getAmelia("main.tree"), "heading", 3, text = "Bounds",
      command = function() sortTreeBy("lower"))
  tcltk::tcl(getAmelia("main.tree"), "heading", 4, text = "Min",
      command = function() sortTreeBy("min"))
  tcltk::tcl(getAmelia("main.tree"), "heading", 5, text = "Max",
      command = function() sortTreeBy("max"))
  tcltk::tcl(getAmelia("main.tree"), "heading", 6, text = "Mean",
      command = function() sortTreeBy("mean"))
  tcltk::tcl(getAmelia("main.tree"), "heading", 7, text = "SD",
      command = function() sortTreeBy("sd"))
  tcltk::tcl(getAmelia("main.tree"), "heading", 8, text = "Missing",
      command = function() sortTreeBy("miss"))
  tcltk::tkbind(getAmelia("main.tree"), "<Button-3>", mainTreeRightClick)

  ## Windows 7 doesn't handle treeview selection correctly
  selectbg <- tcltk::tcl("ttk::style","configure",".","-selectbackground")
  selectfg <- tcltk::tcl("ttk::style","configure",".","-selectforeground")
  tcltk::tktag.configure(getAmelia("main.tree"),"normal", background="white")
  tcltk::tktag.configure(getAmelia("main.tree"),"selected",
                  background=selectbg, foreground=selectfg)
  tcltk::tkbind(getAmelia("main.tree"),"<<TreeviewSelect>>",function()
         refreshSelection(getAmelia("main.tree")))
  putAmelia("legendFrame", tcltk::ttkframe(dashboard))
  tcltk::tkgrid(tcltk::ttklabel(getAmelia("legendFrame"), text="= Time-Series Variable", image =
                  getAmelia("clockIcon"), compound = "left"), row = 0, column = 0,  sticky="w",
         padx = 5)
  tcltk::tkgrid(tcltk::ttklabel(getAmelia("legendFrame"), text="= Cross-Section Variable", image =
                  getAmelia("userIcon"), compound = "left"), row = 0, column = 1,
         sticky="w", padx = 5)
  tcltk::tkgrid(tcltk::ttklabel(getAmelia("legendFrame"), text="= Unhandled Factor Variable", image =
                  getAmelia("redFlagIcon"), compound = "left"), row = 0, column =
         2, sticky="w", padx = 5)

  tcltk::tkgrid(getAmelia("main.tree"), row=0,column=0, sticky="news")
  tcltk::tkgrid(yscr, row = 0, column = 1, sticky = "ns")
  tcltk::tkgrid(xscr, row = 1, column = 0, sticky = "ew")
  tcltk::tkgrid(getAmelia("legendFrame"), row = 2, column = 0, sticky = "ew")
  tcltk::tkgrid.rowconfigure(dashboard, 0, weight = 1)
  tcltk::tkgrid.columnconfigure(dashboard, 0, weight = 1)
  ##Output Frame
  ##output options, run button, diag

  ##output options


  ##grid the whole thing
  tcltk::tkadd(getAmelia("gui.skel"), toolbar)
  tcltk::tkadd(getAmelia("gui.skel"), dashboard)

  tcltk::tkgrid(toolbar, row = 0, column = 1, padx = 2, pady=2, sticky = "ew")
  tcltk::tkgrid(dashboard,row = 1,  column = 1, sticky = "news", padx = 10,
         pady = 5)

  tcltk::tkgrid.rowconfigure(getAmelia("gui.skel"), 1, weight = 1)
  tcltk::tkgrid.columnconfigure(getAmelia("gui.skel"), 1, weight = 1)
                                        #tcltk::tkgrid(gui.skel,sticky="news")
  tcltk::tkgrid.rowconfigure(getAmelia("gui"), 0, weight = 1)
  tcltk::tkgrid.columnconfigure(getAmelia("gui"), 0, weight = 1)

  ##statusbar at the bottom.
  putAmelia("statusbar", tcltk::ttkframe(getAmelia("gui"), relief = "groove", borderwidth = 3))
  statusbar.lab1a <- tcltk::ttklabel(getAmelia("statusbar"), text = "Data Loaded:", anchor = "w",
                              padding = c(2,0))
  putAmelia("statusbar.lab1b",
            tcltk::ttklabel(getAmelia("statusbar"), text = "Unspecified", relief = "sunken",
                     anchor = "w", foreground = "red",padding = c(2,0),
                     width = 35))
  statusbar.nlab <- tcltk::ttklabel(getAmelia("statusbar"), text = "Obs:", anchor="e", padding = c(2,0))
  putAmelia("statusbar.n",
            tcltk::ttklabel(getAmelia("statusbar"), text = "----", relief = "sunken",
                     anchor = "w", foreground = "red",padding = c(2,0,0,0),
                     width = 6))

  statusbar.klab <- tcltk::ttklabel(getAmelia("statusbar"), text = "Vars:", anchor="e",
                             padding = c(2,0))

  putAmelia("statusbar.k",
            tcltk::ttklabel(getAmelia("statusbar"), text = "----", relief = "sunken", anchor = "w",
                     foreground = "red", padding = c(2,0,0,0), width = 6))

  putAmelia("runAmeliaProgress",
            tcltk::ttkprogressbar(getAmelia("statusbar"), value = 0, length = 200,
                           mode = "determinate"))
  putAmelia("error.label", tcltk::ttkbutton(getAmelia("statusbar"), text =
                                     "Error! See Output Log.", image =
                                     getAmelia("redStopIcon"), compound = "left", style =
                                     "Toolbutton", command = show.output.log))
  putAmelia("allgood.label", tcltk::ttkbutton(getAmelia("statusbar"), text = "Successful Imputation.", image =
                                       getAmelia("greenCheckIcon"), compound = "left",
                                       style = "Toolbutton", command = showImputedFiles))
  putAmelia("noimps.label", tcltk::ttklabel(getAmelia("statusbar"), text =
                                     "No imputations run.", justify = "right"))

  tcltk::tkgrid(statusbar.lab1a,row = 2, column = 1, sticky="w")
  tcltk::tkgrid(getAmelia("statusbar.lab1b"),row = 2, column = 2, sticky="w")
  tcltk::tkgrid(statusbar.nlab,row = 2, column = 3, sticky="w")
  tcltk::tkgrid(getAmelia("statusbar.n"),row = 2, column = 4, sticky="w")
  tcltk::tkgrid(statusbar.klab,row = 2, column = 5, sticky="w")
  tcltk::tkgrid(getAmelia("statusbar.k"), row = 2, column = 6, sticky = "w")
  tcltk::tkgrid(getAmelia("noimps.label"), row = 2, column = 7,
         sticky ="e", padx = 10)
  tcltk::tkgrid.rowconfigure(getAmelia("statusbar"), 2, weight = 1)
                                        #tcltk::tkgrid(statusbar, sticky = "sew")


  bindTooltip(widget = "output.run", tip = "Run Amelia on your input dataset with the current settings.")
                                        #  bindTooltip(widget = "output.diag", tip = "Post-imputation checks for problems in the imputation.")
  bindTooltip(widget = "runAmeliaProgress", tip =
              "Amelia is currently running and this shows its progress. On large datasets, Amelia may take quite some time.")
                                        #  bindTooltip(widget = "output.drop.label", tip = "Set the file format for saving the imputed datasets, if you want to save them.")
                                        #  bindTooltip(widget = "output.drop.box", tip = "Set the file format for saving the imputed datasets, if you want to save them.")
  bindTooltip(widget = "showLogButton", tip = "Show the output log for the Amelia run. From here, you can save the output. Look here if something went wrong.")
  bindTooltip(widget = "missmapButton", tip = "Show a map of the missingnes in the data.")
  bindTooltip(widget = "editDataButton", tip =
              "Edit individual cells of the data set.")

  bindTooltip(widget = "plotHistButton", tip =
              "Plot histogram(s) of the selected variable(s).")

  bindTooltip(widget = "loadSessionButton", tip =
              "Load a previously saved Amelia session. This will remove any current settings.")

  bindTooltip(widget = "saveSessionButton", tip =
              "Save the current Amelia session. This will save the data, settings, and any imputed data in the Amelia session.")
  bindTooltip(widget = "legendFrame", tip =
              "A legend for the icons used in the variable dashboard.")
  bindTooltip(widget = "noimps.label", tip =
              "No imputations have been run yet. To run Amelia, hit the 'Impute!' button in the toolbar.")
  bindTooltip(widget = "allgood.label", tip =
              "Amelia has run successfully! You can now run imputation diagnostics from the 'Output' menu above. If you chose to save the imputations to file, they should be saved in the working directory. Click here to open the containing folder..")
  bindTooltip(widget = "error.label", tip =
              "There was an error the last time you ran Amelia. Click here to open the output log to identify the problem and to see how to fix it.")
  ## these commands force R to wait for tcltk
  if (.Platform$OS.type == "windows")
    tcltk::tkwm.iconbitmap(getAmelia("gui"),file.path(find.package(package = "Amelia")[1], "gui/amelia.ico"))
  tcltk::tkraise(getAmelia("gui"))
  tcltk::tkwm.deiconify(getAmelia("gui"))
  tcltk::tkfocus(getAmelia("gui"))
  tcltk::tclServiceMode(on = TRUE)
  tcltk::tkwait.window(getAmelia("gui"))


}

buildNumericalOptions <- function() {
  onCancel <- function(){
    tcltk::tcl("set", getAmelia("seed"), getAmelia("temp.seed"))
    tcltk::tcl("set", getAmelia("tol"), getAmelia("temp.tol"))
    tcltk::tkwm.withdraw(getAmelia("numericalWindow"))
    tcltk::tkgrab.release(getAmelia("numericalWindow"))
    tcltk::tkfocus(getAmelia("gui"))
  }

  putAmelia("temp.seed", tcltk::tclvalue(getAmelia("seed")))
  putAmelia("temp.tol", tcltk::tclvalue(getAmelia("tol")))

  if (exists("numericalWindow", envir = ameliaEnv)) {
    tcltk::tkwm.deiconify(getAmelia("numericalWindow"))
    tcltk::tkraise(getAmelia("numericalWindow"))
    return()
  }

  putAmelia("numericalWindow", tcltk::tktoplevel())
  tcltk::tkwm.title(getAmelia("numericalWindow"), "Numerical Options")
  numericalBox <- tcltk::ttkframe(getAmelia("numericalWindow"))
  putAmelia("output.seedlab", tcltk::ttklabel(numericalBox, text="Seed:"))
  putAmelia("output.seed",
            tcltk::ttkentry(numericalBox, width="7", textvariable=getAmelia("seed")))
  putAmelia("output.tollab", tcltk::ttklabel(numericalBox, text="Tolerance:"))
  putAmelia("output.tol",
            tcltk::ttkentry(numericalBox, width="7",
                     textvariable=getAmelia("tol")))
  putAmelia("empri.ent", tcltk::ttkentry(numericalBox, width=7,textvariable = getAmelia("empri")))
  putAmelia("empri.label", tcltk::ttklabel(numericalBox,text="Ridge prior:"))
  putAmelia("maxre.ent", tcltk::ttkentry(numericalBox, width=7,textvariable = getAmelia("max.resample")))
  putAmelia("maxre.label",
            tcltk::ttklabel(numericalBox,text="Maximum Resample for Bounds:"))

  buttonBox <- tcltk::ttkframe(numericalBox)
  okButton <- tcltk::ttkbutton(buttonBox, text = "OK", width = 10, command = function() {tcltk::tkwm.withdraw(getAmelia("numericalWindow"));tcltk::tkgrab.release(getAmelia("numericalWindow"));tcltk::tkfocus(getAmelia("gui"))})
  cancelButton <- tcltk::ttkbutton(buttonBox, width = 10, text = "Cancel", command = onCancel)


  tcltk::tkgrid(getAmelia("output.seedlab"), row = 1, column = 1, sticky = "w", padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("output.seed"), row = 1, column = 2, sticky = "w", padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("output.tollab"), row = 2, column = 1, sticky =
         "w", padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("output.tol"), row = 2, column = 2, sticky = "w",
         padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("empri.label"), row = 3, column = 1, sticky = "w", padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("empri.ent"), row = 3, column = 2, sticky = "w",
         padx = 10, pady = 10)

  tcltk::tkgrid(getAmelia("maxre.label"), row = 4, column = 1, sticky = "w", padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("maxre.ent"), row = 4, column = 2, sticky = "w",
         padx = 10, pady = 10)
  tcltk::tkgrid(okButton, row = 0, column = 0, padx = 10, pady = 10)
  tcltk::tkgrid(cancelButton, row = 0, column = 1, padx = 10, pady = 10)
  tcltk::tkgrid(buttonBox, row = 5, column = 1, sticky = "e", columnspan = 2)
  tcltk::tkgrid(numericalBox, sticky = "news")

  tcltk::tkwm.protocol(getAmelia("numericalWindow"), "WM_DELETE_WINDOW", onCancel)

  centerModalDialog(getAmelia("numericalWindow"), resize=FALSE)

  bindTooltip(widget = "empri.ent", "Ridge prior that shrinks the covariances, which stabilizes estimation. Five percent of the number of observations is a useful default.")
  bindTooltip(widget = "empri.label", "Ridge prior that shrinks the covariances, which stabilizes estimation. Five percent of the number of observations is a useful default.")
  bindTooltip(widget = "output.seed", tip = "Set seed for random number generator. Useful if you need to replicate the exact same imputations.")
  bindTooltip(widget = "output.seedlab", tip =
              "Set seed for random number generator. Useful if you need to replicate the exact same imputations.")
  bindTooltip(widget = "output.tol", tip =
              "Set the tolerance for the Amelia run. This is the value used to determine when Amelia has converged. Higher values mean Amelia will coverge more quickly, but this may lead to a poor approximation of the parameters.")
  bindTooltip(widget = "output.tollab", tip =
              "Set the tolerance for the Amelia run. This is the value used to determine when Amelia has converged. Higher values mean Amelia will coverge more quickly, but this may lead to a poor approximation of the parameters.")
  bindTooltip(widget = "maxre.ent", tip = "Amelia fits bounds by rejecting any draws that do not fall within the bounds. This value sets the number of times Amelia should attempt to resample to fit the bounds before setting the imputation to the bound.")
  bindTooltip(widget = "maxre.label", tip = "Amelia fits bounds by rejecting any draws that do not fall within the bounds. This value sets the number of times Amelia should attempt to resample to fit the bounds before setting the imputation to the bound.")

}


buildOutputOptions <- function() {
  onCancel <- function(){
    tcltk::tcl("set", getAmelia("outname"), getAmelia("temp.name"))
    tcltk::tcl("set", getAmelia("outnum"), getAmelia("temp.num"))
    tcltk::tkwm.withdraw(getAmelia("outputWindow"))
    tcltk::tkgrab.release(getAmelia("outputWindow"))
    tcltk::tkfocus(getAmelia("gui"))
  }

  putAmelia("temp.name", tcltk::tclvalue(getAmelia("outname")))
  putAmelia("temp.num", tcltk::tclvalue(getAmelia("outnum")))

  if (exists("outputWindow", envir = ameliaEnv)) {
    tcltk::tkwm.deiconify(getAmelia("outputWindow"))
    tcltk::tkraise(getAmelia("outputWindow"))
    return()
  }

  putAmelia("outputWindow", tcltk::tktoplevel())
  tcltk::tkwm.title(getAmelia("outputWindow"), "Output Options")
  outputBox <- tcltk::ttkframe(getAmelia("outputWindow"))

  putAmelia("output.label", tcltk::ttklabel(outputBox, text="Name the Imputed Dataset:"))
  putAmelia("output.entry",
            tcltk::ttkentry(outputBox, width="15",
                     textvariable = getAmelia("outname")))

  putAmelia("output.numlab", tcltk::ttklabel(outputBox, text = "Number of Imputed Datasets:"))
  putAmelia("output.num",
            tcltk::ttkentry(outputBox, width = "7",
                     textvariable = getAmelia("outnum")))

  buttonBox <- tcltk::ttkframe(outputBox)
  okButton <- tcltk::ttkbutton(buttonBox, text = "OK", width = 10, command = function() {tcltk::tkwm.withdraw(getAmelia("outputWindow"));tcltk::tkgrab.release(getAmelia("outputWindow"));tcltk::tkfocus(getAmelia("gui"))})
  cancelButton <- tcltk::ttkbutton(buttonBox, width = 10, text = "Cancel", command = onCancel)


  tcltk::tkgrid(getAmelia("output.label"), row = 1, column = 1, sticky = "w", padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("output.entry"), row = 1, column = 2, sticky = "w", padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("output.numlab"), row = 2, column = 1, sticky = "w", padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("output.num"), row = 2, column = 2, sticky = "w",
         padx = 10, pady = 10)
  tcltk::tkgrid(okButton, row = 0, column = 0, padx = 10, pady = 10)
  tcltk::tkgrid(cancelButton, row = 0, column = 1, padx = 10, pady = 10)
  tcltk::tkgrid(buttonBox, row = 3, column = 1, sticky = "e", columnspan = 2)
  tcltk::tkgrid(outputBox, sticky = "news")

  tcltk::tkwm.protocol(getAmelia("outputWindow"), "WM_DELETE_WINDOW", onCancel)

  centerModalDialog(getAmelia("outputWindow"), resize=FALSE)

  bindTooltip(widget = "output.entry", tip = "The prefix for the saved imputed datasets. For most saving options they will be in the following format: \n\nmyprefix1.out\nmyprefix2.out\n...\n\nAnd so on, where \"out\" is the file extension.")
  bindTooltip(widget = "output.label", tip = "The prefix for the saved imputed datasets. For most saving options they will be in the following format: \n\nmyprefix1.out\nmyprefix2.out\n...\n\nAnd so on, where \"out\" is the file extension.")
  bindTooltip(widget = "output.num", tip = "Set the number of imputed datasets.\n\nIn many cases, around 5 is sufficient, but if the fraction of missingness is high, you may need more. Use the Summarize Data and Missingness Map above to get a sense for the amount of missingness in your data.")
  bindTooltip(widget = "output.numlab", tip = "Set the number of imputed datasets.\n\nIn many cases, around 5 is sufficient, but if the fraction of missingness is high, you may need more. Use the Summarize Data and Missingness Map above to get a sense for the amount of missingness in your data.")

}



buildAboutDialog <- function() {
  if (exists("aboutWindow", envir = ameliaEnv)) {
    tcltk::tkwm.deiconify(getAmelia("aboutWindow"))
    tcltk::tkraise(getAmelia("aboutWindow"))
    return()
  }
  putAmelia("aboutWindow", tcltk::tktoplevel(parent=getAmelia("gui")))
  tcltk::tkwm.title(getAmelia("aboutWindow"), "About AmeliaView")
  aboutBox <- tcltk::ttkframe(getAmelia("aboutWindow"), height = 150, width = 200)
                                        #ameliaPic <- tcltk::tkimage.create("photo",file=ameliaFile)
  picLabel <- tcltk::ttklabel(aboutBox, image=getAmelia("ameliaPic"), relief="groove", borderwidth=2)
  tcltk::tkgrid(tcltk::ttkframe(aboutBox,width=100), row=0,column=1)
  tcltk::tkgrid(tcltk::ttkframe(aboutBox,height=150,width=0), row=0,column=0,rowspan=3)
  tcltk::tkgrid(picLabel, row = 1, column=1, pady = 20, padx = 20)
  tcltk::tkgrid(tcltk::ttklabel(aboutBox, text=paste("AmeliaView",packageDescription("Amelia", fields="Version")), justify="center"), row = 2, column = 1)
  tcltk::tkgrid(tcltk::ttklabel(aboutBox, text="James Honaker, Gary King, Matthew Blackwell", justify="center"), row = 3, column = 1, padx=20)
  tcltk::tkgrid(tcltk::ttklabel(aboutBox, text="\uA9 2006-2010", justify="center"), row = 4, column = 1, padx=20)
  buttonBox <- tcltk::ttkframe(aboutBox)
  closeButton <- tcltk::ttkbutton(buttonBox, text = "Close", command = function() {tcltk::tkwm.withdraw(getAmelia("aboutWindow"));tcltk::tkgrab.release(getAmelia("aboutWindow"));tcltk::tkfocus(getAmelia("gui"))}, width = 10)
  websiteButton <- tcltk::ttkbutton(buttonBox, text = "Website",
                             command = function() browseURL("http://gking.harvard.edu/amelia/"))
  tcltk::tkgrid(websiteButton, row=0, column = 0, sticky="w", padx=10, pady=10)
  tcltk::tkgrid(closeButton, row=0, column = 0, sticky="e", padx=10, pady=10)
  tcltk::tkgrid.columnconfigure(buttonBox, 0, weight=1)
  tcltk::tkgrid(buttonBox, row=5, column = 1, sticky="ew")
  tcltk::tkgrid(aboutBox, sticky = "nsew")
  tcltk::tkwm.protocol(getAmelia("aboutWindow"), "WM_DELETE_WINDOW", function() {tcltk::tkwm.withdraw(getAmelia("aboutWindow"));tcltk::tkgrab.release(getAmelia("aboutWindow"));tcltk::tkfocus(getAmelia("gui"))})

  centerModalDialog(getAmelia("aboutWindow"), resize=FALSE)
}





gui.pri.setup <- function() {
  cancelPriors <- function() {
    putAmelia("priorsmat", getAmelia("temp.priorsmat"))
  }
  onOK <- function() {

    nm <- c("dist","range")[getAmeliaInd("addpri.note")+1]
    varBox <- paste("add",nm,"var",sep=".")
    caseBox <- paste("add",nm,"case",sep=".")
    caseSelection <- as.numeric(tcltk::tcl(getAmelia(caseBox),"current"))
    varSelection  <- as.numeric(tcltk::tcl(getAmelia(varBox),"current")) +1

    thiscase <- tcltk::tclvalue(tcltk::tkget(getAmelia(caseBox)))
    thisvar  <- tcltk::tclvalue(tcltk::tkget(getAmelia(varBox)))


    if (caseSelection==0) {
      rowSelection <- 0
      colSelection <- which(anyMissing)[varSelection]
    } else {
      rowSelection  <- missingCases[caseSelection]
      colSelection <- which(is.na(getAmelia("amelia.data")[rowSelection,]))[varSelection]
    }

                                        # fork for range vs. dist
    if (nm == "range") {
      if (tcltk::tclvalue(getAmelia("priorMin"))=="") {
        tcltk::tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Please enter a minimum value.",
                     type="ok",icon="error")
        return()
      }
      if (tcltk::tclvalue(getAmelia("priorMax"))=="") {
        tcltk::tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Please enter a maximum value.",
                     type="ok",icon="error")
        return()
      }

      if (tcltk::tclvalue(getAmelia("priorConf"))=="") {
        tcltk::tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Please enter a confidence value.",
                     type="ok",icon="error")
        return()
      }
      if (isTRUE(as.numeric(tcltk::tclvalue(getAmelia("priorConf"))) <= 0
                 | as.numeric(tcltk::tclvalue(getAmelia("priorConf"))) >= 1)) {
        tcltk::tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Confidence levels must be between 0 and 1.",
                     type="ok",icon="error")
        return()
      }

      prMax <- as.numeric(tcltk::tclvalue(getAmelia("priorMax")))
      prMin <- as.numeric(tcltk::tclvalue(getAmelia("priorMin")))
      prCon <- as.numeric(tcltk::tclvalue(getAmelia("priorConf")))
      if (prMax <= prMin) {
        tcltk::tkmessageBox(title="Error",
                     message="The max is less than the min.",
                     type="ok",icon="error")
        return()
      }
      prMean<- prMin + ((prMax-prMin)/2)
      prSD  <-(prMax-prMin)/(2*qnorm(1-(1-prCon)/2))


                                        #if dist prior
    } else {
      if (tcltk::tclvalue(getAmelia("priorMean"))=="") {
        tcltk::tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Please enter a mean value.",
                     type="ok",icon="error")
        return()
      }
      if (tcltk::tclvalue(getAmelia("priorSD"))=="") {
        tcltk::tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Please enter a standard deviation.",
                     type="ok",icon="error")
        return()
      }
      if (isTRUE(as.numeric(tcltk::tclvalue(getAmelia("priorSD"))) == 0)) {
        tcltk::tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Standard deviations must be greater than 0.",
                     type="ok",icon="error")
        return()
      }
      prMean <- as.numeric(tcltk::tclvalue(getAmelia("priorMean")))
      prSD   <- as.numeric(tcltk::tclvalue(getAmelia("priorSD")))


    }
    newPrior <- c(rowSelection, colSelection,prMean,prSD)
    if (!is.null(getAmelia("priorsmat"))) {
      matchPrior <- apply(getAmelia("priorsmat"), 1,
                          function(x) all(x[1]==rowSelection,
                                          x[2]==colSelection))
    } else {
      matchPrior <- FALSE
    }

    if (any(matchPrior)) {
      mess <- "There is a prior associate with this case. Overwrite?"
      over <- tcltk::tkmessageBox(parent=getAmelia("priorsWindow"), title="Overwrite Prior",message=mess,
                           icon="question",type="yesno",default="no")
      if (tcltk::tclvalue(over)=="no") {
        return()
      } else {
        putAmelia("priorsmat",getAmelia("priorsmat")[-which(matchPrior),])
        tcltk::tkdelete(getAmelia("priors.tree"), paste(rowSelection,colSelection,sep="-"))
      }
    }

    putAmelia("priorsmat",rbind(getAmelia("priorsmat"),newPrior))

    ## need to change the treeview
                                        #updateTree()
    tcltk::tkinsert(getAmelia("priors.tree"),"","end", id = paste(rowSelection,colSelection,sep="-"), values = c(thisvar,prMean,prSD),
             text = thiscase,tag="normal")
    resetEntries()
    return()
  }
  validateNumeric <- function(x) {
    if (isTRUE(grep("(^-?[0-9]*\\.?[0-9]*$)",x)==1))
      return(tcltk::tclVar("TRUE"))
    else
      return(tcltk::tclVar("FALSE"))
  }
  validateSD <- function(x) {
    if (isTRUE(grep("^[0-9]*\\.?[0-9]*$",x)==1))
      return(tcltk::tclVar("TRUE"))
    else
      return(tcltk::tclVar("FALSE"))
  }
  validateConf <- function(x) {
    if (isTRUE(grep("^0*\\.[0-9]*$",x)==1))
      return(tcltk::tclVar("TRUE"))
    else
      return(tcltk::tclVar("FALSE"))
  }
  setMissingVars <- function() {

    currentSelection <- as.numeric(tcltk::tcl(getAmelia("add.dist.case"), "current"))
    currentCase      <- missingCases[currentSelection]
    if (currentSelection==0)
      missVars <- anyMissing
    else
      missVars    <- is.na(getAmelia("amelia.data")[currentCase,])
    missVarNames <- colnames(getAmelia("amelia.data"))[missVars]
    tcltk::tkconfigure(getAmelia("add.dist.var"),values = missVarNames)
    tcltk::tcl(getAmelia("add.dist.var"), "current", 0)
  }

  resetEntries <- function() {
    tcltk::tcl("set", getAmelia("priorMin"),"")
    tcltk::tcl("set", getAmelia("priorMax"),"")
    tcltk::tcl("set", getAmelia("priorMean"),"")
    tcltk::tcl("set", getAmelia("priorSD"),"")
    tcltk::tcl("set", getAmelia("priorConf"),"")
    return()
  }
  updateTree <- function() {
    allrows <- paste(tcltk::tcl(getAmelia("priors.tree"),"children",""))
    tcltk::tkdelete(getAmelia("priors.tree"), allrows)

    if (is.null(getAmelia("priorsmat"))) {
      return()
    }

    varnames <- names(getAmelia("amelia.data"))
    cases <- paste(rownames(getAmelia("amelia.data")), ") ",
                   getAmelia("amelia.data")[,getAmelia("csvar")]," ",
                   getAmelia("amelia.data")[,getAmelia("tsvar")], sep="")
    cases <- c("(whole variable)", cases)
    for (i in 1:nrow(getAmelia("priorsmat"))) {
      thiscase <- cases[getAmelia("priorsmat")[i,1]+1]
      thisvar <- varnames[getAmelia("priorsmat")[i,2]]
      tcltk::tkinsert(getAmelia("priors.tree"),"","end", id = paste(getAmelia("priorsmat")[i,1],getAmelia("priorsmat")[i,2],sep="-"), values = c(thisvar,getAmelia("priorsmat")[i,c(3,4)]),
               text = thiscase,tag="normal")
    }
    return()

  }
  dropPriors <- function() {
    sel.pri <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("priors.tree"), "selection")), " ")[[1]]
    pri.mat.rows <- c()
    for (i in 1:length(sel.pri)) {
      pri.mat.rows <- c(pri.mat.rows, tcltk::tclvalue(tcltk::tkindex(getAmelia("priors.tree"),sel.pri[i])))
    }
    pri.mat.rows <- as.numeric(pri.mat.rows) + 1
    putAmelia("priorsmat", getAmelia("priorsmat")[-pri.mat.rows,, drop = FALSE])
    tcltk::tkdelete(getAmelia("priors.tree"),paste(tcltk::tcl(getAmelia("priors.tree"), "selection")))
    if (nrow(getAmelia("priorsmat")) == 0) putAmelia("priorsmat", NULL)
    return(NULL)
  }
  RightClick <- function(x, y) { # x and y are the mouse coordinates
    rootx <- as.integer(tcltk::tkwinfo("rootx", getAmelia("priors.tree")))  # tcltk::tkwinfo() return several infos
    rooty <- as.integer(tcltk::tkwinfo("rooty", getAmelia("priors.tree")))
    xTxt <- as.integer(x) + rootx
    yTxt <- as.integer(y) + rooty
                                        # Create a Tcl command in a character string and run it
    tcltk::.Tcl(paste("tk_popup", tcltk::.Tcl.args(getAmelia("pri.right.click"), xTxt, yTxt)))
  }


  putAmelia("temp.priorsmat", getAmelia("priorsmat"))

  if (exists("priorsWindow", envir=ameliaEnv)) {
    updateTree()
    resetEntries()
    tcltk::tkwm.deiconify(getAmelia("priorsWindow"))
    tcltk::tkraise(getAmelia("priorsWindow"))
    tcltk::tkgrab(getAmelia("priorsWindow"))
    return()
  }
  putAmelia("priorsWindow", tcltk::tktoplevel())
  tcltk::tkwm.title(getAmelia("priorsWindow"),"Observational Priors")

  priorsBox <- tcltk::ttkframe(getAmelia("priorsWindow"))

  prior.frame <- tcltk::ttkpanedwindow(priorsBox, orient = "horizontal")
  prior.disp  <- tcltk::ttklabelframe(prior.frame, text = "Observational priors ", height = 200, width = 200)
  prior.add   <- tcltk::ttklabelframe(prior.frame, text = "Add priors", height = 200, width = 200)
  putAmelia("prior.add.but", tcltk::ttkbutton(prior.add, text = "Add", command = function() onOK()))

  yscr <- tcltk::ttkscrollbar(prior.disp,  orient = "vertical",
                       command=function(...)tcltk::tkyview(getAmelia("priors.tree"),...))
  xscr <- tcltk::ttkscrollbar(prior.disp, orient = "horizontal",
                       command=function(...)tcltk::tkxview(getAmelia("priors.tree"),...))
  putAmelia("priors.tree", tcltk::ttktreeview(prior.disp, columns = "Variable Mean SD",
                                       yscrollcommand=function(...)tcltk::tkset(yscr,...), xscrollcommand=function(...)tcltk::tkset(xscr,...)))

  putAmelia("pri.right.click",tcltk::tkmenu(getAmelia("priors.tree"), tearoff = FALSE) )
  tcltk::tkadd(getAmelia("pri.right.click"), "command", label = "Remove selected priors", command = function() dropPriors())
  tcltk::tkbind(getAmelia("priors.tree"), "<Button-3>", RightClick)

  tcltk::tcl(getAmelia("priors.tree"), "column", "#0", width = 120)
  tcltk::tcl(getAmelia("priors.tree"), "column", 0, width = 80, anchor = "center")
  tcltk::tcl(getAmelia("priors.tree"), "column", 1, width = 40, anchor = "center")
  tcltk::tcl(getAmelia("priors.tree"), "column", 2, width = 40, anchor = "center")
  tcltk::tcl(getAmelia("priors.tree"), "heading", "#0", text = "Case")
  tcltk::tcl(getAmelia("priors.tree"), "heading", 0, text = "Variable")
  tcltk::tcl(getAmelia("priors.tree"), "heading", 1, text = "Mean")
  tcltk::tcl(getAmelia("priors.tree"), "heading", 2, text = "SD")
  ## Windows 7 doesn't handle treeview selection correctly
  if (.Platform$OS.type == "windows") {
    tcltk::tktag.configure(getAmelia("priors.tree"),"normal", background="white")
    tcltk::tktag.configure(getAmelia("priors.tree"),"selected", background="SystemHighlight")
    tcltk::tkbind(getAmelia("priors.tree"),"<<TreeviewSelect>>",function() refreshSelection(getAmelia("priors.tree")))
  }
  putAmelia("addpri.note", tcltk::ttknotebook(prior.add))
  add.dist.frame <- tcltk::ttkframe(getAmelia("addpri.note"))
  add.range.frame <- tcltk::ttkframe(getAmelia("addpri.note"))



  missingCases <- which(!complete.cases(getAmelia("amelia.data")))
  anyMissing   <- apply(getAmelia("amelia.data"), 2, function(x) any(is.na(x)))

  cases1 <- paste(rownames(getAmelia("amelia.data"))[missingCases], ") ",
                  getAmelia("amelia.data")[missingCases, getAmelia("csvar")]," ",
                  getAmelia("amelia.data")[missingCases, getAmelia("tsvar")], sep="")


  cases <- c("(whole variable)",cases1)

  if (!is.null(getAmelia("priorsmat"))) updateTree()
  vars <- getAmelia("varnames")[anyMissing]


  ## Distribution prior note

  putAmelia("add.dist.case",tcltk::ttkcombobox(add.dist.frame, values=cases,
                                        state="readonly", width=15))
  putAmelia("add.dist.var",tcltk::ttkcombobox(add.dist.frame, values=vars,
                                       state="readonly", width=15))
  tcltk::tkbind(getAmelia("add.dist.case"), "<<ComboboxSelected>>", function(...) setMissingVars())
  tcltk::tkgrid(tcltk::ttklabel(add.dist.frame, text="Case:"), column=1, row=1, sticky = "e")
  tcltk::tkgrid(tcltk::ttklabel(add.dist.frame, text="Variable:"), column=1, row=2, sticky = "e")
  tcltk::tcl(getAmelia("add.dist.case"), "current", 0)
  tcltk::tcl(getAmelia("add.dist.var"), "current", 0)
  tcltk::tkconfigure(getAmelia("add.dist.var"), postcommand=function(...) setMissingVars())
  tcltk::tkgrid(getAmelia("add.dist.case"), column=2, row=1, pady=3)
  tcltk::tkgrid(getAmelia("add.dist.var"),  column=2, row=2, pady=3)



  putAmelia("priorMean", tcltk::tclVar())
  putAmelia("priorSD", tcltk::tclVar())


  tcltk::tkgrid(tcltk::ttkframe(add.dist.frame, width = 150, height = 0), column = 1, row = 0)
  putAmelia("meanBox", tcltk::ttkentry(add.dist.frame, textvar=getAmelia("priorMean"), validate="key",
                                validatecommand = function(P) validateNumeric(P)))

  putAmelia("sdBox", tcltk::ttkentry(add.dist.frame, textvar=getAmelia("priorSD"), validate="key",
                              validatecommand = function(P) validateSD(P)))

  tcltk::tkgrid(tcltk::ttklabel(add.dist.frame, text="Mean:"), column=1, row=3, sticky = "e")
  tcltk::tkgrid(tcltk::ttklabel(add.dist.frame, text="Standard Deviation:"), column=1,
         row=4, sticky = "e")

  tcltk::tkgrid(getAmelia("meanBox"), column=2, row=3, pady=5, padx=5)
  tcltk::tkgrid(getAmelia("sdBox"), column=2, row=4, pady=5, padx=5)

  ## Range prior note

  putAmelia("add.range.case",tcltk::ttkcombobox(add.range.frame, values=cases,
                                         state="readonly", width=15))
  putAmelia("add.range.var",tcltk::ttkcombobox(add.range.frame, values=vars,
                                        state="readonly", width=15))

  tcltk::tkgrid(tcltk::ttklabel(add.range.frame, text="Case:"), column=1, row=1, sticky = "e")
  tcltk::tkgrid(tcltk::ttklabel(add.range.frame, text="Variable:"), column=1, row=2, sticky = "e")
  tcltk::tcl(getAmelia("add.range.case"), "current", 0)
  tcltk::tcl(getAmelia("add.range.var"), "current", 0)
  tcltk::tkconfigure(getAmelia("add.range.var"), postcommand=function(...) setMissingVars())
  tcltk::tkgrid(getAmelia("add.range.case"), column=2, row=1, pady=3)
  tcltk::tkgrid(getAmelia("add.range.var"),  column=2, row=2, pady=3)

  tcltk::tkgrid(tcltk::ttkframe(add.range.frame, width = 150, height = 0), column = 1, row = 0)
  putAmelia("priorMax", tcltk::tclVar())
  putAmelia("priorMin", tcltk::tclVar())
  putAmelia("priorConf", tcltk::tclVar())


  putAmelia("minBox", tcltk::ttkentry(add.range.frame, textvar=getAmelia("priorMin"), validate="key",
                               validatecommand = function(P) validateNumeric(P)))

  putAmelia("maxBox", tcltk::ttkentry(add.range.frame, textvar=getAmelia("priorMax"), validate="key",
                               validatecommand = function(P) validateNumeric(P)))


  putAmelia("confBox", tcltk::ttkentry(add.range.frame, textvar=getAmelia("priorConf"), validate="key",
                                validatecommand = function(P) validateNumeric(P)))

  tcltk::tkgrid(tcltk::ttklabel(add.range.frame, text="Minimum:"), column=1, row=3, sticky = "e")
  tcltk::tkgrid(tcltk::ttklabel(add.range.frame, text="Maximum:"), column=1, row=4, sticky = "e")
  tcltk::tkgrid(tcltk::ttklabel(add.range.frame, text="Confidence:"), column=1, row=5, sticky = "e")
                                        #tcltk::tkgrid(tkframe(add.range.frame, width = 20, height = 0), column = 1, row = 6)

  tcltk::tkgrid(getAmelia("minBox"), column=2, row=3, pady=5, padx=5)
  tcltk::tkgrid(getAmelia("maxBox"), column=2, row=4, pady=5, padx=5)
  tcltk::tkgrid(getAmelia("confBox"), column=2, row=5, pady=5, padx=5)

  tcltk::tkadd(getAmelia("addpri.note"), add.dist.frame, text = "Add Distribution Prior")
  tcltk::tkadd(getAmelia("addpri.note"), add.range.frame, text = "Add Range Prior")


  tcltk::tkgrid(getAmelia("addpri.note"), row = 1, sticky = "nsew")

  tcltk::tkgrid(getAmelia("prior.add.but"), sticky = "se", padx = 10, pady = 10)
  but.frame <- tcltk::ttkframe(priorsBox)
  putAmelia("pri.ok", tcltk::ttkbutton(but.frame, text = "OK", command = function(){tcltk::tkwm.withdraw(getAmelia("priorsWindow"));tcltk::tkgrab.release(getAmelia("priorsWindow"));tcltk::tkfocus(getAmelia("gui"))}, width = 10))
  putAmelia("pri.can", tcltk::ttkbutton(but.frame, text = "Cancel", width = 10, command = function() {cancelPriors();tcltk::tkwm.withdraw(getAmelia("priorsWindow"));tcltk::tkgrab.release(getAmelia("priorsWindow"));tcltk::tkfocus(getAmelia("gui"))}))


  tcltk::tkgrid(getAmelia("priors.tree"), row = 1, column = 1, sticky = "nsew")
  tcltk::tkgrid(yscr, row = 1, column = 2, sticky = "nsew")
  tcltk::tkgrid(xscr, row = 2, column = 1, sticky = "nsew")
  tcltk::tkgrid.rowconfigure(prior.disp, 1, weight = 1)
  tcltk::tkgrid.columnconfigure(prior.disp, 1, weight = 1)
  tcltk::tkadd(prior.frame, prior.add)
  tcltk::tkadd(prior.frame, prior.disp)

  tcltk::tkgrid(prior.frame, row = 1, column = 0, columnspan = 2, padx = 10, pady = 10, sticky = "news")
  tcltk::tkgrid(getAmelia("pri.ok"), row = 0, column = 1, sticky = "ne", padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("pri.can"), row = 0, column = 2, sticky = "ne", padx = 10, pady = 10)
  tcltk::tkgrid(but.frame, row = 2, column = 1, sticky = "ne")
  tcltk::tkgrid.rowconfigure(priorsBox, 1, weight = 1)
  tcltk::tkgrid.columnconfigure(priorsBox, 0, weight = 1)
  tcltk::tkgrid.columnconfigure(priorsBox, 1, weight = 1)
  tcltk::tkgrid(priorsBox, row = 0, column = 0, sticky = "news")
  tcltk::tkgrid.rowconfigure(getAmelia("priorsWindow"), 0, weight = 1)
  tcltk::tkgrid.columnconfigure(getAmelia("priorsWindow"), 0, weight = 1)

  tcltk::tkwm.protocol(getAmelia("priorsWindow"), "WM_DELETE_WINDOW", function() {tcltk::tkwm.withdraw(getAmelia("priorsWindow"));tcltk::tkgrab.release(getAmelia("priorsWindow"));tcltk::tkfocus(getAmelia("gui"))})

  centerModalDialog(getAmelia("priorsWindow"), resize = TRUE)

  bindTooltip(widget = "priors.tree", "Currently set observation-level priors for the data. You can remove these using the right-click menu.")
  bindTooltip(widget = "pri.ok", tip = "Save changes and close window.")
  bindTooltip(widget = "pri.can", tip = "Cancel any changes and close window.")
  bindTooltip(widget = "prior.add.but", tip = "Add the above prior for the selected observation and variable to the list of priors for this data set.")
  bindTooltip(widget = "meanBox", tip = "The mean of a normal prior distribution on the value of the selected missing cell.")
  bindTooltip(widget = "sdBox", tip = "The standard deviation of a normal prior distribution on the value of the selected missing cell.")
  bindTooltip(widget = "add.dist.case", tip = "Select the case name or row number of the case for the cell-level prior.")
  bindTooltip(widget = "add.dist.var", tip = "Select the variable name for the cell-level prior.")
  bindTooltip(widget = "confBox", tip = "A confidence level between 0 and 1 for the confidence bound on the distribution of the selected missing cell. These confidence bounds are converted into a normal distribution prior on the value.")
  bindTooltip(widget = "minBox", tip = "A lower confidence bound on the distribution of the selected missing cell. These confidence bounds are converted into a normal distribution prior on the value.")
  bindTooltip(widget = "maxBox", tip = "An upper confidence bound on the distribution of the selected missing cell.  These confidence bounds are converted into a normal distribution prior on the value.")
  bindTooltip(widget = "add.range.case", tip = "Select the case name or row number of the case for the cell-level prior.")
  bindTooltip(widget = "add.range.var", tip = "Select the variable name for the cell-level prior.")
}



gui.diag.setup <- function() {

  if (exists("diagWindow", envir = ameliaEnv)) {
    tcltk::tkwm.deiconify(getAmelia("diagWindow"))
    tcltk::tkraise(getAmelia("diagWindow"))
    tcltk::tkfocus(getAmelia("diagWindow"))
    return()
  }
  putAmelia("diagWindow", tcltk::tktoplevel())
  tcltk::tkwm.title(getAmelia("diagWindow"), "Diagnostics")

  diagBox <- tcltk::ttkframe(getAmelia("diagWindow"))
  gui.top<-tcltk::ttkpanedwindow(diagBox, orient = "vertical")
  var.diags <- tcltk::ttklabelframe(gui.top, text = "Individual Variable Plots", width = 100, height = 100)
  tscs.diags <- tcltk::ttklabelframe(gui.top, text = "Time-Series Cross-Sectional Plots", width = 100, height = 100)
  disp.diags <- tcltk::ttklabelframe(gui.top, text = "Overdispersion Plots", width = 100, height = 100)

  tcltk::tcl("set","indvar","")

  ## get variable names that are actually numeric
  variables <- getAmelia("varnames")
  variables <- variables[sapply(getAmelia("amelia.data"), is.numeric)]

  putAmelia("var.diags.combo", tcltk::ttkcombobox(var.diags,textvariable="indvar",
                                           values = variables, state = "readonly"))
  indvar.lab <- tcltk::ttklabel(var.diags, text = "Variable:")
  var.button.frame <- tcltk::ttkframe(var.diags)
  putAmelia("diag.but.compare",tcltk::ttkbutton(var.button.frame, text="Compare",
                                         command = function() compare.density(getAmelia("ameliaObject"),
                                           var=tcltk::tclvalue("indvar"),frontend=TRUE)))

  putAmelia("diag.overimp",tcltk::ttkbutton(var.button.frame,text="Overimpute",state="normal",
                                     command = function() overimpute(getAmelia("ameliaObject"),
                                       var=tcltk::tclvalue("indvar"),frontend=TRUE)))
  tcltk::tcl(getAmelia("var.diags.combo"), "current", 0)
  tcltk::tkgrid(indvar.lab, row = 0, column = 0, padx = 5)
  tcltk::tkgrid(getAmelia("var.diags.combo"), row = 0, column = 1, padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("diag.but.compare"), row = 0, column = 0, padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("diag.overimp"), row = 0, column = 1, padx = 10, pady = 10)
  tcltk::tkgrid(var.button.frame, row =0, column = 2)
  tcltk::tkgrid(tcltk::ttkframe(var.diags, width = 50, height = 0), row = 1)


  ## tscs plots
  csvar <- getAmelia("ameliaObject")$arguments$cs
  tsvar <- getAmelia("ameliaObject")$arguments$ts

  ## can't do tscsplots for the ts or cs variable
  tscsvariables <- variables[variables != getAmelia("varnames")[csvar] &
                             variables != getAmelia("varnames")[tsvar]]

  if (is.null(tsvar) | is.null(csvar)) {
    st <- "disabled"
    but.st <- st
  } else {
    st <- "readonly"
    but.st <- "normal"
  }
  if (!is.null(csvar)) {
    cases <- unique(getAmelia("amelia.data")[,csvar])
    if (is.factor(getAmelia("amelia.data")[,csvar])) {
      cases <- levels(getAmelia("amelia.data")[,csvar])[cases]
    }

  } else {
    cases <- 1:nrow(getAmelia("amelia.data"))
  }
  tcltk::tcl("set", "casename","")
  tcltk::tcl("set", "tscsvarname", "")
  putAmelia("tscs.case.combo",
            tcltk::ttkcombobox(tscs.diags,textvariable="casename", values = cases,
                        state = st))
  putAmelia("tscs.var.combo",
            tcltk::ttkcombobox(tscs.diags,textvariable="tscsvarname",
                        values = tscsvariables, state = st))
  putAmelia("tscs.plot.but",
            tcltk::ttkbutton(tscs.diags, text = "TSCS Plot", state = but.st,
                      command = function() tscsPlot(getAmelia("ameliaObject"),
                        cs = tcltk::tclvalue("casename"),
                        var = tcltk::tclvalue("tscsvarname"),
                        frontend = TRUE)))
  if (st == "readonly") {
    tcltk::tcl(getAmelia("tscs.case.combo"), "current", 0)
    tcltk::tcl(getAmelia("tscs.var.combo"), "current", 0)
  }
  tcltk::tkgrid(tcltk::ttklabel(tscs.diags, text = "Case:"), row = 0, column = 0, sticky = "e", padx = 5)
  tcltk::tkgrid(getAmelia("tscs.case.combo"), row = 0, column = 1, padx = 10, pady = 10)
  tcltk::tkgrid(tcltk::ttklabel(tscs.diags, text = "Variable:"), row = 1, column = 0, sticky = "e", padx = 5)
  tcltk::tkgrid(getAmelia("tscs.var.combo"), row = 1, column = 1, padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("tscs.plot.but"), row = 1, column = 2, padx = 10, pady = 10, sticky = "se")
  tcltk::tkgrid(tcltk::ttkframe(tscs.diags, width = 50, height = 0), row = 2)

  dimvalue<-tcltk::tclVar("1")
  putAmelia("onedim", tcltk::ttkradiobutton(disp.diags, variable=dimvalue, value="1"))
  putAmelia("twodims", tcltk::ttkradiobutton(disp.diags, variable=dimvalue, value="2"))
  disp.imps.tcl<-tcltk::tclVar("5")
  putAmelia("disp.imps", tcltk::ttkentry(disp.diags,width="5",textvariable=disp.imps.tcl))
  putAmelia("disp.but", tcltk::ttkbutton(disp.diags,text="Overdisperse",state="normal",
                                  command = function() disperse(m=as.numeric(tcltk::tclvalue(disp.imps.tcl)),
                                    dims=as.numeric(tcltk::tclvalue(dimvalue)),frontend=TRUE,output=getAmelia("ameliaObject"))))
  tcltk::tkgrid(tcltk::ttklabel(disp.diags,text="Number of dispersions:"),row=2,column=1,
         sticky="e")
  tcltk::tkgrid(tcltk::ttkframe(disp.diags, width = 50, height = 0), row = 5)
  tcltk::tkgrid(getAmelia("disp.imps"),column=2,row=2,sticky="nw", padx = 10, pady = 10)
  tcltk::tkgrid(tcltk::ttklabel(disp.diags,text="One Dimension:"),row=3,column=1, sticky = "e")
  tcltk::tkgrid(tcltk::ttklabel(disp.diags,text="Two Dimensions:"),row=4,column=1, sticky = "e")
  tcltk::tkgrid(getAmelia("onedim"),row=3,column=2,padx=10,pady=5)
  tcltk::tkgrid(getAmelia("twodims"),row=4,column=2,padx=10)
  tcltk::tkgrid(getAmelia("disp.but"),row=4,column=3,padx=15, pady=10,sticky="news")

  tcltk::tkadd(gui.top, var.diags)
  tcltk::tkadd(gui.top, tscs.diags)
  tcltk::tkadd(gui.top, disp.diags)
  tcltk::tkgrid(gui.top, row = 0, padx = 20, pady = 20)
  tcltk::tkgrid(diagBox, sticky = "news", row = 0, column = 0)
  tcltk::tkgrid.rowconfigure(getAmelia("diagWindow"), 0, weight = 1)
  tcltk::tkgrid.columnconfigure(getAmelia("diagWindow"), 0, weight = 1)

  tcltk::tkwm.protocol(getAmelia("diagWindow"), "WM_DELETE_WINDOW", function() {tcltk::tkwm.withdraw(getAmelia("diagWindow"));tcltk::tkgrab.release(getAmelia("diagWindow"));tcltk::tkfocus(getAmelia("gui"))})
  centerModalDialog(getAmelia("diagWindow"), resize = FALSE)

  bindTooltip(widget = "var.diags.combo", tip = "Variable for either the density comparison plot or the overimputation plot.")
  bindTooltip(widget = "tscs.var.combo", tip = "Variable to use for the time-series cross-sectional plot.")
  bindTooltip(widget = "tscs.case.combo", tip = "Case to use for the time-series cross-sectional plot.")
  bindTooltip(widget = "diag.but.compare", tip = "Compare densities of the imputed values vs. observed values.")
  bindTooltip(widget = "diag.overimp", tip = "Overimpute and graph confidence intervals. ")

  bindTooltip(widget = "disp.but", tip = "Plot the convergence of the EM algorithm from overdispersed starting values.")

  bindTooltip(widget = "tscs.plot.but", tip = "Plot a time-series within one cross-section with imputation distributions in red.")

  bindTooltip(widget = "disp.imps", tip = "Number of different overdispersed starting values to use.")

  bindTooltip(widget = "onedim", tip = "Number of dimensions to visualize convergence.")

  bindTooltip(widget = "twodims", tip = "Number of dimensions to visualize convergence.")

}


## the following functions have been imported from Rcmdr

putAmelia <- function(x, value) {
  assign(x, value, envir = ameliaEnv)
}

getAmelia <- function(x, mode="any")
  get(x, envir = ameliaEnv, mode = mode, inherits = FALSE)


getAmeliaInd <- function(x) {
  as.numeric(tcltk::tkindex(getAmelia(x), "current"))
}
ameliaTclSet <- function(name, value){
  name <- ls(unclass(getAmelia(name))$env)
  tcltk::tcl("set", name, value)
}

save.log <- function() {
  file.select <- tcltk::tclvalue(tcltk::tkgetSaveFile(parent=getAmelia("gui"), filetypes="{{Text files} {*.txt}} {{All files} *}"))
  cat(getAmelia("output.log"), file = file.select)
}

show.output.log <- function() {

  RightClick <- function(x, y) { # x and y are the mouse coordinates
    rootx <- as.integer(tcltk::tkwinfo("rootx", getAmelia("log.viewer")))  # tcltk::tkwinfo() return several infos
    rooty <- as.integer(tcltk::tkwinfo("rooty", getAmelia("log.viewer")))
    xTxt <- as.integer(x) + rootx
    yTxt <- as.integer(y) + rooty
                                        # Create a Tcl command in a character string and run it
    tcltk::.Tcl(paste("tk_popup", tcltk::.Tcl.args(getAmelia("log.right.click"), xTxt, yTxt)))
  }


  if (exists("log.top", envir = ameliaEnv)) {
    tcltk::tkconfigure(getAmelia("log.viewer"), state = "normal")
    tcltk::tkdelete(getAmelia("log.viewer"), "0.0", "end")
    tcltk::tkinsert(getAmelia("log.viewer"), "end",
                    paste(getAmelia("output.log"), collapse = ""))
    tcltk::tkconfigure(getAmelia("log.viewer"), state = "disabled")
    tcltk::tkwm.deiconify(getAmelia("log.top"))
    tcltk::tkraise(getAmelia("log.top"))
    tcltk::tkfocus(getAmelia("log.top"))
    return()
  }
  putAmelia("log.top", tcltk::tktoplevel())
  tcltk::tkwm.title(getAmelia("log.top"), "Output Log")
  scr <- tcltk::ttkscrollbar(getAmelia("log.top"),
                      command=function(...)tcltk::tkyview(getAmelia("log.viewer"),...))

  putAmelia("log.viewer", tcltk::tktext(getAmelia("log.top"), width = 80, height = 25,
                                 yscrollcommand=function(...)tcltk::tkset(scr,...)))
  tcltk::tkinsert(getAmelia("log.viewer"), "end", paste(getAmelia("output.log"), collapse = ""))
  tcltk::tkconfigure(getAmelia("log.viewer"), state = "disabled")
  main.menu      <- tcltk::tkmenu(getAmelia("log.top"))
  main.menu.file <- tcltk::tkmenu(main.menu, tearoff=0)
  tcltk::tkadd(main.menu.file,"command",label="Save log file",command=function() save.log())
  tcltk::tkadd(main.menu.file,"command",label="Close",command=function(){tcltk::tkwm.withdraw(getAmelia("log.top"));tcltk::tkgrab.release(getAmelia("log.top"));tcltk::tkfocus(getAmelia("gui"))})

  tcltk::tkadd(main.menu,"cascade",label="File",menu=main.menu.file)
  tcltk::tkconfigure(getAmelia("log.top"),menu=main.menu)


  putAmelia("log.right.click",tcltk::tkmenu(getAmelia("log.viewer"), tearoff = FALSE) )
  tcltk::tkadd(getAmelia("log.right.click"), "command", label = "Copy <Ctrl-V>",
        command = function() tcltk::tkevent.generate(getAmelia("log.viewer"),"<<Copy>>"))
  tcltk::tkbind(getAmelia("log.viewer"), "<Button-3>", RightClick)

                                        #tcltk::tkgrid(main.menu, row = 0, sticky = "ew")
  tcltk::tkgrid(getAmelia("log.viewer"), row = 0, column = 0, sticky = "news")
  tcltk::tkgrid(scr, row =0, column = 1, sticky = "ns")
                                        #tcltk::tkgrid.columnconfigure(log.top, 1, weight = 1)
  tcltk::tkgrid.columnconfigure(getAmelia("log.top"), 0, weight = 1)
  tcltk::tkgrid.rowconfigure(getAmelia("log.top"), 0, weight = 1)
  tcltk::tkwm.protocol(getAmelia("log.top"), "WM_DELETE_WINDOW", function() {tcltk::tkwm.withdraw(getAmelia("log.top"));tcltk::tkgrab.release(getAmelia("log.top"));tcltk::tkfocus(getAmelia("gui"))})
  centerModalDialog(getAmelia("log.top"), resize=TRUE)


}

after <- function(ms, func) {
  tcltk::.Tcl(paste("after", ms, tcltk::.Tcl.callback(func)))
}

cancel.after <- function(id) {
  invisible(tcltk::.Tcl(paste("after","cancel", id)))
}

bindTooltip <- function(widget, tip) {
  after.name <- paste(widget, "after", sep = ".")
  tip.name <- paste(widget, "tip", sep = ".")
                                        #  tcltk::tkbind(getAmelia(widget), "<Any-Enter>", showTooltip(widget, tip))
  tcltk::tkbind(getAmelia(widget), "<Any-Enter>", function() putAmelia(after.name, after(400, showTooltip(widget, tip))))
  tcltk::tkbind(getAmelia(widget), "<Any-Leave>", function() {killTooltip(widget)
                                                       cancel.after(getAmelia(after.name))})
  tcltk::tkbind(getAmelia(widget), "<Any-Button>", function() cancel.after(getAmelia(after.name)))
  tcltk::tkbind(getAmelia(widget), "<Any-KeyPress>", function() cancel.after(getAmelia(after.name)))

}



showTooltip <- function(widget, text) {

  function() {
    if (getAmelia(widget)$ID != tcltk::tclvalue(tcltk::tkwinfo("containing", tcltk::tkwinfo("pointerx","."),
                           tcltk::tkwinfo("pointery",".")))) {
      return()
    }
    tip.name <- paste(widget, "tip", sep = ".")
    tiplabel.name <- paste(widget, "tiplabel",sep=".")


    if (exists(tip.name, envir = ameliaEnv)) {
      if (as.logical(tcltk::tkwinfo("exists",getAmelia(tip.name)))) {
        if (as.logical(tcltk::tkwinfo("ismapped",getAmelia(tip.name)))) {
          return()
        }
      }
    }


    scrh <- tcltk::tclvalue(tcltk::tkwinfo("screenheight", getAmelia(widget)))
    scrw <- tcltk::tclvalue(tcltk::tkwinfo("screenwidth", getAmelia(widget)))


    tcltk::tclServiceMode(on=FALSE)
    if (!exists(tip.name, envir = ameliaEnv)) {
      if (.Platform$OS.type == "windows") {
        borderColor <- "SystemWindowFrame"
        bgColor <- "SystemWindow"
        fgColor <- "SystemWindowText"
      }  else {
        borderColor <- "black"
        bgColor <- "lightyellow"
        fgColor <- "black"
      }
      putAmelia(tip.name, tcltk::tktoplevel(getAmelia(widget), bd = 1, bg = borderColor, relief = "raised"))
      tcltk::tkwm.geometry(getAmelia(tip.name), paste("+",scrh,"+",scrw,sep=""))
      tcltk::tcl("wm","overrideredirect", getAmelia(tip.name), 1)

      putAmelia(tiplabel.name, tcltk::ttklabel(getAmelia(tip.name), background = bgColor,
                                        foreground = fgColor, text = text, justify = "left",
                                        wraplength=300))
      tcltk::tkpack(getAmelia(tiplabel.name))

      tcltk::tkbind(getAmelia(tip.name), "<Any-Enter>",
             function() tcltk::tkwm.withdraw(getAmelia(tip.name)))
      tcltk::tkbind(getAmelia(tip.name), "<Any-Leave>",
             function() tcltk::tkwm.withdraw(getAmelia(tip.name)))
      tcltk::tkbind(getAmelia(tip.name), "<Any-Button>",
             function() tcltk::tkwm.withdraw(getAmelia(tip.name)))
    }

    width  <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("reqwidth", getAmelia(tiplabel.name))))
    height <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("reqheight",getAmelia(tiplabel.name))))

    posX <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("pointerx",".")))
    posY <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("pointery","."))) + 25
    screen <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("screenwidth",".")))

                                        # a.) Ad-hockery: Set positionX so the entire tooltip widget will be displayed.
    if  ((posX + width) > screen) {
      posX <-  posX - ((posX + width) - screen) - 3
    }
    tcltk::tclServiceMode(on = TRUE)
    tcltk::tkwm.geometry(getAmelia(tip.name),
                  paste("+",posX,"+",posY,sep = ""))

    tcltk::tkwm.deiconify(getAmelia(tip.name))
    tcltk::tkraise(getAmelia(tip.name))



  }
}

killTooltip <- function(widget) {
  tip.name <- paste(widget,"tip", sep = ".")
  if (exists(tip.name, envir = ameliaEnv)) {
    tcltk::tkwm.withdraw(getAmelia(tip.name))
  }
}


refreshSelection <- function(tree) {
  all <- strsplit(tcltk::tclvalue(tcltk::tcl(tree,"children","")), " ")[[1]]
  sel <- strsplit(tcltk::tclvalue(tcltk::tcl(tree, "selection")), " ")[[1]]
  bandTree()
  for (i in sel) {
    tcltk::tcl(tree, "item", i, tags = "selected")
  }
  return(NULL)
}

variableOptionStatus <- function(sel) {
  states <- rep("normal", 15)
  classes <- sapply(getAmelia("amelia.data"), class)[sel]
  if (length(sel) ==0) {
    states <- rep("disabled", 15)
    return(states)
  }
  if (length(sel) > 1)
    states[c(1:4,15)] <- "disabled"
  if (!is.null(getAmelia("tsvar")))
    if (getAmelia("tsvar") %in% sel)
      states[c(1:2,5:9,12:13,15)] <- "disabled"
    else
      states[3] <- "disabled"
  if (!is.null(getAmelia("csvar")))
    if (getAmelia("csvar") %in% sel)
      states[c(1:2,5:9,12:13,15)] <- "disabled"
    else
      states[4] <- "disabled"
  if (is.null(getAmelia("tsvar")))
    states[c(3,6:9)] <- "disabled"
  if (is.null(getAmelia("csvar")))
    states[4] <- "disabled"
  if ("factor" %in% classes | "character" %in% classes)
    states[c(11,15)] <- "disabled"
  if (is.null(getAmelia("amelia.data")))
    states <- rep("disabled", 15)
  return(states)
}

variableOptionsPost <- function() {
  sel <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  states <- variableOptionStatus(sel)
  for (i in 0:14) {
    if (tcltk::tclvalue(tcltk::tktype(getAmelia("main.menu.variables"), i)) != "separator")
      tcltk::tkentryconfigure(getAmelia("main.menu.variables"),i, state = states[i+1])
  }
  return(NULL)
}

mainTreeRightClick <- function(x, y) { # x and y are the mouse coordinates
  rootx <- as.integer(tcltk::tkwinfo("rootx", getAmelia("main.tree")))  # tcltk::tkwinfo() return several infos
  rooty <- as.integer(tcltk::tkwinfo("rooty", getAmelia("main.tree")))
  xTxt <- as.integer(x) + rootx
  yTxt <- as.integer(y) + rooty
                                        # Create a Tcl command in a character string and run it
  sel <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  states <- variableOptionStatus(sel)
  main.tree.right.click <- tcltk::tkmenu(getAmelia("main.tree"), tearoff = FALSE)
  main.tree.trans <- tcltk::tkmenu(getAmelia("main.tree"), tearoff = FALSE)
  tcltk::tkadd(main.tree.right.click, "command", label = "Set as Time-Series Variable", command = setTS, state = states[1])
  tcltk::tkadd(main.tree.right.click, "command", label = "Set as Cross-Section Variable", command = setCS, state = states[2])
  tcltk::tkadd(main.tree.right.click, "command", label = "Unset as Time-Series Variable", command = unsetTS, state = states[3])
  tcltk::tkadd(main.tree.right.click, "command", label = "Unset as Cross-Section Variable", command = unsetCS, state = states[4])
  tcltk::tkadd(main.tree.right.click,"separator")
  tcltk::tkadd(main.tree.right.click, "command", label = "Add Lag", command = function() addLag(), state = states[6])
  tcltk::tkadd(main.tree.right.click, "command", label = "Add Lead", command = function() addLead(), state = states[7])
  tcltk::tkadd(main.tree.right.click, "command", label = "Remove Lag", command = function() dropLag(), state = states[8])
  tcltk::tkadd(main.tree.right.click, "command", label = "Remove Lead", command = function() dropLead(), state = states[9])
  tcltk::tkadd(main.tree.right.click,"separator")

  tcltk::tkadd(main.tree.right.click, "command", label =
        "Plot Histogram(s) of Selected", command = plotHist, state = states[10])
  if (.Platform$OS.type == "windows") {
    tcltk::tkadd(main.tree.trans, "command", label = "Log", command = function(x) setTrans("logs"))
    tcltk::tkadd(main.tree.trans, "command", label = "Square Root", command = function(x) setTrans("sqrt"))
    tcltk::tkadd(main.tree.trans, "command", label = "Logistic", command = function(x) setTrans("lgstc"))
    tcltk::tkadd(main.tree.trans, "command", label = "Nominal", command = function(x) setTrans("noms"))
    tcltk::tkadd(main.tree.trans, "command", label = "Ordinal", command = function(x) setTrans("ords"))
    tcltk::tkadd(main.tree.trans, "command", label = "ID Variable", command = function(x) setTrans("idvar"))
    tcltk::tkadd(main.tree.right.click, "cascade", label = "Add Transformation...", menu = main.tree.trans, state = states[12])
  } else {
    tcltk::tkadd(main.tree.right.click, "command", label = "Mark as Log", command = function(x) setTrans("logs"), state = states[12])
    tcltk::tkadd(main.tree.right.click, "command", label = "Mark as Square Root", command = function(x) setTrans("sqrt"), state = states[12])
    tcltk::tkadd(main.tree.right.click, "command", label = "Mark as Logistic", command = function(x) setTrans("lgstc"), state = states[12])
    tcltk::tkadd(main.tree.right.click, "command", label = "Mark as Nominal", command = function(x) setTrans("noms"), state = states[12])
    tcltk::tkadd(main.tree.right.click, "command", label = "Mark as Ordinal", command = function(x) setTrans("ords"), state = states[12])
    tcltk::tkadd(main.tree.right.click, "command", label = "Mark as ID Variable", command = function(x) setTrans("idvar"), state = states[12])
  }
  tcltk::tkadd(main.tree.right.click, "command", label =
        "Remove Transformations", command = dropTrans, state = states[13])
  tcltk::tkadd(main.tree.right.click,"separator")
  tcltk::tkadd(main.tree.right.click, "command", label =
        "Add or Edit Bounds", command = addBounds, state = states[15])
  tcltk::tkpopup(main.tree.right.click, xTxt, yTxt)
}

addLag <- function() {
  sel <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  tmp <- getAmelia("lags")
  tmp[sel] <- 1
  putAmelia("lags", tmp)
  for (i in sel)
    tcltk::tkset(getAmelia("main.tree"), i, "lag", "X")
  return()
}
addLead <- function() {
  sel <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  tmp <- getAmelia("leads")
  tmp[sel] <- 1
  putAmelia("leads", tmp)
  for (i in sel)
    tcltk::tkset(getAmelia("main.tree"), i, "lead", "X")
  return()
}
dropLag <- function() {
  sel <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  tmp <- getAmelia("lags")
  tmp[sel] <- 0
  putAmelia("lags", tmp)
  for (i in sel)
    tcltk::tkset(getAmelia("main.tree"), i, "lag", "")
  return()
}
dropLead <- function() {
  sel <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  tmp <- getAmelia("leads")
  tmp[sel] <- 0
  putAmelia("leads", tmp)
  for (i in sel)
    tcltk::tkset(getAmelia("main.tree"), i, "lead", "")
  return()
}

setTrans <- function(trans) {
  all.trans <- c(logs = "Log",sqrt = "Square Root",
                 lgstc = "Logistic", noms = "Nominal", ords = "Ordinal", idvar = "ID")
  sel <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  tmp <- getAmelia(trans)
  tmp[sel] <- 1
  putAmelia(trans, tmp)
  for (j in sel) {
    tcltk::tkset(getAmelia("main.tree"), j,"transform", all.trans[trans])
    tcltk::tcl(getAmelia("main.tree"), "item", j, image = "")
  }
  return()
}

dropTrans <- function() {
  all.trans <- c("logs","sqrt","lgstc","noms","ords","idvar")
  sel <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  for (j in sel)
    tcltk::tkset(getAmelia("main.tree"), j,"transform", "")
  if (is.factor(getAmelia("amelia.data")[,j]) |
      is.character(getAmelia("amelia.data")[,j])) {
    tcltk::tcl(getAmelia("main.tree"), "item", j, image = getAmelia("redFlagIcon"))
  }

  for (i in all.trans) {
    tmp <- getAmelia(i)
    tmp[sel] <- 0
    putAmelia(i, tmp)
  }
}

addBounds <- function() {
  onOK <- function(sel) {

    bdMax <- as.numeric(tcltk::tclvalue(getAmelia("boundMax")))
    bdMin <- as.numeric(tcltk::tclvalue(getAmelia("boundMin")))

    if (is.na(bdMax) & !is.na(bdMin)) {
      tcltk::tkmessageBox(parent=getAmelia("addBoundsWindow"), title="Error",
                   message="Please enter a minimum and a maximum value or neither to clear the bounds.",
                   type="ok",icon="error")
      return()
    }
    if  (!is.na(bdMax) & is.na(bdMin)) {
      tcltk::tkmessageBox(parent=getAmelia("addBoundsWindow"), title="Error",
                   message="Please enter a minimum and a maximum value or neither to clear the bounds.",
                   type="ok",icon="error")
      return()
    }

    if (!is.na(bdMax) & !is.na(bdMin)) {
      if (bdMax <= bdMin) {
        tcltk::tkmessageBox(parent=getAmelia("addBoundsWindow"), title="Error",
                     message="The maximum is less than the minimum.",
                     type="ok",icon="error")
        return()
      }
    }
    tmpbmat <- getAmelia("boundsmat")
    tmpbmat[sel,2:3] <- c(bdMin, bdMax)
    putAmelia("boundsmat", tmpbmat)
    if (!is.na(bdMin)) {
      treeBounds <- paste("[",bdMin,", ", bdMax,"]", sep = "")
    } else {
      treeBounds <- ""
    }
    tcltk::tkset(getAmelia("main.tree"), sel, "bounds", treeBounds)
    tcltk::tkwm.withdraw(getAmelia("addBoundsWindow"))
    tcltk::tkgrab.release(getAmelia("addBoundsWindow"))
    tcltk::tkfocus(getAmelia("gui"))
    return()
  }
  validateNumeric <- function(x) {
    if (isTRUE(grep("(^-?[0-9]*\\.?[0-9]*$)",x)==1))
      return(tcltk::tclVar("TRUE"))
    else
      return(tcltk::tclVar("FALSE"))
  }


  sel <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"), "selection")),
                  " ")[[1]]
  if (sum(is.na(getAmelia("amelia.data")[,sel])) == 0) {
    tcltk::tkmessageBox(parent = getAmelia("gui"), message =
                 "No missing data on the selected variable.", type = "ok")
    return()

  }
  currMin <- getAmelia("boundsmat")[sel,2]
  currMax <- getAmelia("boundsmat")[sel,3]

  putAmelia("boundMin", tcltk::tclVar(ifelse(is.na(currMin), "",
                                      currMin)))
  putAmelia("boundMax", tcltk::tclVar(ifelse(is.na(currMax), "",
                                      currMax)))

  if (exists("addBoundsWindow", envir = ameliaEnv)) {
    tcltk::tkconfigure(getAmelia("maxBox"), textvar = getAmelia("boundMax"))
    tcltk::tkconfigure(getAmelia("minBox"), textvar = getAmelia("boundMin"))
    tcltk::tkconfigure(getAmelia("bd.ok"), command = function() onOK(sel))
    tcltk::tkwm.deiconify(getAmelia("addBoundsWindow"))
    tcltk::tkraise(getAmelia("addBoundsWindow"))
    return()
  }
  putAmelia("addBoundsWindow", tcltk::tktoplevel())
  tcltk::tkwm.title(getAmelia("addBoundsWindow"), "Add or Edit Bounds")
  bounds.add   <- tcltk::ttkframe(getAmelia("addBoundsWindow"))


  putAmelia("minBox", tcltk::ttkentry(bounds.add, textvar=getAmelia("boundMin"), validate="key",
                               validatecommand = function(P) validateNumeric(P)))

  putAmelia("maxBox", tcltk::ttkentry(bounds.add, textvar=getAmelia("boundMax"), validate="key",
                               validatecommand = function(P) validateNumeric(P)))

  tcltk::tkgrid(tcltk::ttklabel(bounds.add, text="Minimum:"), column=1, row=2,
         sticky = "e", padx = 10, pady = 10)
  tcltk::tkgrid(tcltk::ttklabel(bounds.add, text="Maximum:"), column=1, row=3,
         sticky = "e", padx = 10, pady = 10)

  tcltk::tkgrid(getAmelia("minBox"), column=2, row=2, pady=5, padx=5)
  tcltk::tkgrid(getAmelia("maxBox"), column=2, row=3, pady=5, padx=5)
  but.frame <- tcltk::ttkframe(bounds.add)
  putAmelia("bd.ok", tcltk::ttkbutton(but.frame, text = "OK", command = function() onOK(sel)))
  putAmelia("bd.can", tcltk::ttkbutton(but.frame, text = "Cancel", width =
                                10, command = function() {tcltk::tkwm.withdraw(getAmelia("addBoundsWindow"));tcltk::tkgrab.release(getAmelia("addBoundsWindow"));tcltk::tkfocus(getAmelia("gui"))}))
  tcltk::tkgrid(getAmelia("bd.ok"), row = 0, column = 1, sticky = "ne", padx = 10, pady = 10)
  tcltk::tkgrid(getAmelia("bd.can"), row = 0, column = 2, sticky = "ne", padx = 10, pady = 10)
  tcltk::tkgrid(but.frame, row = 4, column = 1, columnspan = 2, sticky =
         "ne")
  tcltk::tkgrid(bounds.add, sticky = "news")

  tcltk::tkwm.protocol(getAmelia("addBoundsWindow"), "WM_DELETE_WINDOW", function() {tcltk::tkwm.withdraw(getAmelia("addBoundsWindow"));tcltk::tkgrab.release(getAmelia("addBoundsWindow"));tcltk::tkfocus(getAmelia("gui"))})

  centerModalDialog(getAmelia("addBoundsWindow"), resize=FALSE)
}


plotHist <- function() {
  sel <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"), "selection")),
                  " ")[[1]]
  if (length(sel)==0) {
    tcltk::tkmessageBox(parent = getAmelia("gui"), type = "ok", message =
                 "No variable selected.")
    return(NULL)
  }
  sel <- sel[which(sapply(getAmelia("amelia.data")[sel], is.numeric))]
  if (length(sel)==0) {
    tcltk::tkmessageBox(parent = getAmelia("gui"), type = "ok", message =
                 "Cannot plot non-numeric variables.")
    return(NULL)
  }
  dev.new()
  mfrow <- set.mfrow(nvars = length(sel))
  on.exit(par(NULL))
  layout <- par(mfrow = mfrow)
  j <- 0
  for (i in sel) {
    j <- j + 1
    if (j > 9) {
      j <- 1
      dev.new()
      layout <- par(mfrow = mfrow)
    }

    hist(getAmelia("amelia.data")[,i],
         main = paste("Histogram of",i), ylab = "Frequnecy",
         xlab ="", col="grey", border = "white")

  }
  invisible()

}


sortTreeBy <- function(col) {
  coldata <- c()
  children <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"),
                                    "children","")), " ")[[1]]

  if (col == "#0") {
    coldata <- children
  } else {
    for (i in children) {
      coldata <- c(coldata, tcltk::tclvalue(tcltk::tkset(getAmelia("main.tree"), i, col)))
    }
  }
  dirs <- getAmelia("sortDirs")
  sortDir <- dirs[col]
  if (col %in% c("mean", "sd", "min", "max")) {
    coldata[coldata == "..."] <- "-Inf"
    coldata[coldata == "(factor)"] <- "-Inf"
    sortOrder <- order(as.numeric(coldata), decreasing = sortDir)
  } else if (col == "miss") {
    coldata <- matrix(unlist(strsplit(coldata,"/")), nrow=2)[1,]
    sortOrder <- order(as.numeric(coldata), decreasing = sortDir)
  } else {
    sortOrder <- order(coldata, decreasing = sortDir)
  }


  sorted <- children[sortOrder]
  for (i in 1:length(children)) {
    tcltk::tkmove(getAmelia("main.tree"), sorted[i],"", i-1)
  }
  drawArrow(col, sortDir)
  refreshSelection(getAmelia("main.tree"))
  dirs[col] <- !sortDir
  putAmelia("sortDirs", dirs)
}

drawArrow <- function(col, down) {
  treecols <- names(getAmelia("sortDirs"))
  for (i in treecols) {
    tcltk::tcl(getAmelia("main.tree"), "heading", i, image = "")
  }
  if (down) {
    tcltk::tcl(getAmelia("main.tree"), "heading", col,
        image = getAmelia("upArrowIcon"))
  } else {
    tcltk::tcl(getAmelia("main.tree"), "heading", col,
        image = getAmelia("downArrowIcon"))
  }
  return(NULL)
}

bandTree <- function() {
  children <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"),
                                    "children","")), " ")[[1]]
  j <- 0
  tcltk::tktag.configure(getAmelia("main.tree"),"white", background="white")
  tcltk::tktag.configure(getAmelia("main.tree"),"gray", background="gray92")
  for (i in children) {
    j <- j+1
    if ((j %% 2) == 0) {
      tcltk::tcl(getAmelia("main.tree"), "item", i, tag = "white")
    } else {
      tcltk::tcl(getAmelia("main.tree"), "item", i, tag = "gray")
    }
  }
}

updateTreeStats <- function(){
  children <- strsplit(tcltk::tclvalue(tcltk::tcl(getAmelia("main.tree"),"children","")), " ")[[1]]
  for (i in names(getAmelia("amelia.data"))) {

    if (is.factor(getAmelia("amelia.data")[,i]) |
        is.character(getAmelia("amelia.data")[,i])) {
      vals <- c("(factor)","...","...","...")
      vals <- c(vals,paste(sum(is.na(getAmelia("amelia.data")[,i])),
                           nrow(getAmelia("amelia.data")), sep="/"))
    } else {
      vals <- c(min(getAmelia("amelia.data")[,i],na.rm=T), max(getAmelia("amelia.data")[,i],na.rm=T),
                mean(getAmelia("amelia.data")[,i],na.rm=T), sd(getAmelia("amelia.data")[,i],na.rm=T))
      vals <- signif(vals, digits = 4)
      vals <- c(vals, paste(sum(is.na(getAmelia("amelia.data")[,i])),
                            nrow(getAmelia("amelia.data")), sep="/"))
    }
    tcltk::tkset(getAmelia("main.tree"), i, "min", vals[1])
    tcltk::tkset(getAmelia("main.tree"), i, "max", vals[2])
    tcltk::tkset(getAmelia("main.tree"), i, "mean", vals[3])
    tcltk::tkset(getAmelia("main.tree"), i, "sd", vals[4])
    tcltk::tkset(getAmelia("main.tree"), i, "miss", vals[5])
  }


}

centerModalDialog <- function(window, resize=TRUE) {
  xpos <- as.numeric(tcltk::tkwinfo("rootx",getAmelia("gui")))
  ypos <- as.numeric(tcltk::tkwinfo("rootx",getAmelia("gui")))
  rwidth <- as.numeric(tcltk::tkwinfo("width",getAmelia("gui")))
  rheight <- as.numeric(tcltk::tkwinfo("height", getAmelia("gui")))
  width <- as.numeric(tcltk::tkwinfo("reqwidth",window))
  height <- as.numeric(tcltk::tkwinfo("reqheight",window))
  newxpos <- xpos + .5*rwidth - .5*width
  newypos <- ypos + .5*rheight - .5*height
  if (.Platform$OS.type == "windows")
    tcltk::tkwm.geometry(window, paste("+",round(newxpos),"+",round(newypos),sep=""))
  tcltk::tkfocus(window)
  tcltk::tkgrab.set(window)
  if (!resize) {
    tcltk::tkwm.resizable(window, 0,0)
  }
  tcltk::tkwm.transient(window, getAmelia("gui"))
  tcltk::tcl("update","idletasks")
}


showImputedFiles <- function() {
  if (Sys.info()['sysname'] %in% c("Windows", "Darwin"))
    system(paste("open", shQuote(getAmelia("wdForLastImputation"))))
  else
    system(paste("xdg-open", shQuote(getAmelia("wdForLastImputation"))))
  return(NULL)
}


## Here is (finally) a decent solution to the tcl/tk issues with
## global variables. Here we create new environment, whose parent is
## the Amelia namespace. We then make sure that all of the GUI
## functions use that as their enclosure. This means that any of these
## functions can use values in the ameliaEnv. This eliminates the need
## for any "getAmelia" calls, but we still have to be careful since
## assigning values in these functions is local and doesn't
## automatically add the value to ameliaEnv. So, for assigning,
## 'putAmelia' probably still makes sense. We could use
## assign("foo", "bar", envir = parent.frame())
## but putAmelia is probably more clear. getAmelia() is probably still
## a little more safe to use because it will throw an error if
## something is missing, whereas relying on lexical scoping will try
## to use something with the same name in the search path.
ameliaEnv <- new.env()
environment(main.close) <- ameliaEnv
environment(setWorkingDir) <- ameliaEnv
environment(loadStata) <- ameliaEnv
environment(loadSPSS) <- ameliaEnv
environment(loadSAS) <- ameliaEnv
environment(loadTAB) <- ameliaEnv
environment(loadCSV) <- ameliaEnv
environment(loadRData) <- ameliaEnv
environment(loadDemo) <- ameliaEnv
environment(drawMissMap) <- ameliaEnv
environment(activateGUI) <- ameliaEnv
environment(save.session) <- ameliaEnv
environment(load.session) <- ameliaEnv
environment(run.amelia) <- ameliaEnv
environment(amelia.save) <- ameliaEnv
environment(set.out) <- ameliaEnv
environment(setTS) <- ameliaEnv
environment(unsetTS) <- ameliaEnv
environment(setCS) <- ameliaEnv
environment(unsetCS) <- ameliaEnv
environment(fillMainTree) <- ameliaEnv
environment(AmeliaView) <- ameliaEnv
environment(buildNumericalOptions) <- ameliaEnv
environment(buildOutputOptions) <- ameliaEnv
environment(buildAboutDialog) <- ameliaEnv
environment(gui.pri.setup) <- ameliaEnv
environment(gui.diag.setup) <- ameliaEnv
environment(save.log) <- ameliaEnv
environment(show.output.log) <- ameliaEnv
environment(bindTooltip) <- ameliaEnv
environment(showTooltip) <- ameliaEnv
environment(killTooltip) <- ameliaEnv
environment(refreshSelection) <- ameliaEnv
environment(variableOptionStatus) <- ameliaEnv
environment(variableOptionsPost) <- ameliaEnv
environment(mainTreeRightClick) <- ameliaEnv
environment(addLag) <- ameliaEnv
environment(addLead) <- ameliaEnv
environment(dropLag) <- ameliaEnv
environment(dropLead) <- ameliaEnv
environment(setTrans) <- ameliaEnv
environment(dropTrans) <- ameliaEnv
environment(addBounds) <- ameliaEnv
environment(plotHist) <- ameliaEnv
environment(sortTreeBy) <- ameliaEnv
environment(drawArrow) <- ameliaEnv
environment(bandTree) <- ameliaEnv
environment(updateTreeStats) <- ameliaEnv
environment(centerModalDialog) <- ameliaEnv
environment(showImputedFiles) <- ameliaEnv
