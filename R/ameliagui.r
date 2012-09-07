##  ameliagui.r
##  a R-tcl/tk based frontend for Amelia (hopefully crossplatform)
##  mb 23/01/06 - function to be included in amelia package.
##  05/05/06 mb - catches unexpected amelia errors
##  09/05/06 mb - cleaned up diags menu, added overimpute button
##  22/06/06 mb - checks added to session loading.
##  26/06/06 mb - session saving/loading now mirrors amelia output.
##  26/07/06 mb - added stata 6/7/8 compatibility.
##  27/07/06 mb - updated variable options screen.  fixed help links.  cosmetics.
##  04/08/06 mb - sessions load properly for non-csv files.
##  24/08/06 mb - added tolerance option on variables page.
##  11/09/06 mb - actually passes nominals now, fixed char problems on summarize
##  19/09/06 mb - changed function name to AmeliaView
##  01/12/06 mb - fixed session loading for 2.4.0, can't compare non-numerics
##  11/12/06 mb - rehauled priors
##  04/06/08 mb - added a self-contained gui enviroment to store everything (AmeliaEnv())
##              - added seed option to output
##              - added RData save output option
##              - added "hold in R memory" output option
##              - sesssions are now saved as .RData files for compression
## 17/07/08 mb - fixed frontend error bug (dumping output to screen
## 22/07/08 mb - good coding update: T->TRUE/F->FALSE



main.close<-function() {
  qvalue<-tkmessageBox(parent=getAmelia("gui"), message="Are you sure you want to exit Amelia?",
                       icon="question",
                       type="okcancel",
                       default="cancel")
  if (tclvalue(qvalue)=="ok") {
    tkdestroy(getAmelia("gui"))
    detach("ameliaEnv")
  }
}

setWorkingDir <- function() {
  newwd <- tkchooseDirectory(parent=getAmelia("gui"),
                             initialdir = getwd(),
                             title = "Set output directory...",
                             mustexist = TRUE)
  if (tclvalue(newwd) != "")
    setwd(tclvalue(newwd))

  return(NULL)
}

loadStata <- function() {

  filetype <- c("{{Stata files} {.dta}} {{All files} *}")

  putAmelia("am.filename", tclvalue(tkgetOpenFile(parent=getAmelia("gui"), filetypes=filetype)))
  if (getAmelia("am.filename") == "")
    return(NULL)
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tclvalue(sure) == "no")
      return(NULL)
  }
  putAmelia("amelia.data",try(read.dta(getAmelia("am.filename"),convert.factors=FALSE)))
  putAmelia("am.filetype", "Stata")
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tkmessageBox(parent=getAmelia("gui"), message="Failure in loading the data.  Try again.",icon="error",type="ok")
    putAmelia("amelia.data",NULL)
    return(NULL)
  }
  activateGUI()
}

loadSPSS <- function() {

  filetype <- c("{{SPSS} {.sav}} {{All files} *}")
  putAmelia("am.filename", tclvalue(tkgetOpenFile(parent=getAmelia("gui"), filetypes=filetype)))

  if (getAmelia("am.filename") == "")
    return(NULL)
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tclvalue(sure) == "no")
      return(NULL)
  }
  putAmelia("amelia.data",try(read.spss(getAmelia("am.filename"),use.value.labels=FALSE,to.data.frame=TRUE)))
  putAmelia("am.filetype", "SPSS")
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tkmessageBox(parent=getAmelia("gui"), message="Failure in loading the data.  Try again.",icon="error",type="ok")
    putAmelia("amelia.data",NULL)
    return(NULL)
  }
  activateGUI()
}

loadSAS <- function() {

  filetype <- c("{{SAS Transport} {.xpt}} {{All files} *}")
  putAmelia("am.filename", tclvalue(tkgetOpenFile(parent=getAmelia("gui"), filetypes=filetype)))

  if (getAmelia("am.filename") == "")
    return(NULL)
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tclvalue(sure) == "no")
      return(NULL)
  }
  putAmelia("amelia.data",try(read.xport(getAmelia("am.filename"))))
  putAmelia("am.filetype", "SAS")
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tkmessageBox(parent=getAmelia("gui"), message="Failure in loading the data.  Try again.",icon="error",type="ok")
    putAmelia("amelia.data",NULL)
    return(NULL)
  }
  activateGUI()
}

loadTAB <- function() {

  filetype <- c("{{Tab-delimited files} {.txt .tab .dat}} {{All files} *}")
  putAmelia("am.filename", tclvalue(tkgetOpenFile(parent=getAmelia("gui"), filetypes=filetype)))
  if (getAmelia("am.filename") == "")
    return(NULL)
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tclvalue(sure) == "no")
      return(NULL)
  }
  putAmelia("amelia.data",try(read.table(getAmelia("am.filename"),header=TRUE)))
  putAmelia("am.filetype", "TAB")
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tkmessageBox(parent=getAmelia("gui"), message="Failure in loading the data.  Try again.",icon="error",type="ok")
    putAmelia("amelia.data",NULL)
    return(NULL)
  }
  activateGUI()
}

loadCSV <- function() {

  filetype <- c("{{Comma-delimited files} {.csv}} {{All files} *} ")
  putAmelia("am.filename", tclvalue(tkgetOpenFile(parent=getAmelia("gui"), filetypes=filetype)))
  if (getAmelia("am.filename") == "")
    return(NULL)
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tclvalue(sure) == "no")
      return(NULL)
  }
  putAmelia("amelia.data",try(read.csv(getAmelia("am.filename"),header=TRUE)))
  putAmelia("am.filetype", "CSV")
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tkmessageBox(parent=getAmelia("gui"), message="Failure in loading the data.  Try again.",icon="error",type="ok")
    putAmelia("amelia.data",NULL)
    return(NULL)
  }
  activateGUI()
}


loadRData <- function() {
  onOK <- function() {
    putAmelia("amelia.data", eval(as.name(tclvalue(tkget(objectChooser)))))
    tkdestroy(chooseObjectWindow)
    tkfocus(getAmelia("gui"))
    tkgrab.release(chooseObjectWindow)
    activateGUI()
    return()
  }
  onCancel <- function() {
    rm(list=getAmelia("amelia.data"))
    tkdestroy(chooseObjectWindow)
    tkfocus(getAmelia("gui"))
    tkgrab.release(chooseObjectWindow)
    return()
  }

  filetype <- c("{{R Data files} {.RData .Rdata .Rda .rda}} {{All files} *} ")
  putAmelia("am.filename", tclvalue(tkgetOpenFile(parent=getAmelia("gui"), filetypes=filetype)))
  if (getAmelia("am.filename") == "")
    return(NULL)
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tclvalue(sure) == "no")
      return(NULL)
  }
  putAmelia("amelia.data",try(load(getAmelia("am.filename"))))
  putAmelia("am.filetype", "RData")
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tkmessageBox(parent=getAmelia("gui"), message="Failure in loading the data.  Try again.",icon="error",type="ok")
    putAmelia("amelia.data",NULL)
    return(NULL)
  }
  if (length(amelia.data) == 1) {
    putAmelia("amelia.data", eval(as.name(getAmelia("amelia.data"))))
  } else {
    datasets <- sapply(getAmelia("amelia.data"), function(x) is.data.frame(eval(as.name(x))))
    datasets <- getAmelia("amelia.data")[datasets]
    chooseObjectWindow <- tktoplevel(parent=getAmelia("gui"))
    tkwm.title(chooseObjectWindow, "Find Data Set")
    chooseFrame <- ttkframe(chooseObjectWindow)
    objectChooser <- ttkcombobox(chooseFrame, width = 20)
    tkconfigure(objectChooser, values = datasets)
    tkset(objectChooser, datasets[1])
    objectOK <- ttkbutton(chooseFrame, text = "OK", width = 10, command = onOK)
    objectCancel <- ttkbutton(chooseFrame, text = "Cancel", width = 10, command = onCancel)

    tkgrid(ttklabel(chooseFrame, text = "Please select your dataset from the following objects:"),
           row = 0, column = 0, columnspan = 2, padx = 10, pady = 10)
    tkgrid(objectChooser, row = 1, column = 0, columnspan = 2, padx = 10, pady = 10)
    tkgrid(objectOK, row = 2, column = 0, padx = 10, pady = 10)
    tkgrid(objectCancel, row = 2, column = 1, padx = 10, pady = 10)
    tkgrid(chooseFrame, padx = 10, pady = 10)
    tkgrab(chooseObjectWindow)
    tkfocus(chooseObjectWindow)
    tkwm.protocol(chooseObjectWindow, "WM_DELETE_WINDOW", onCancel)
    centerModalDialog(chooseObjectWindow, resize=FALSE)
  }
  return()
}

loadDemo <- function(name) {
  if (!is.null(getAmelia("amelia.data"))) {
    sure<-tkmessageBox(parent=getAmelia("gui"), message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")
    if (tclvalue(sure) == "no")
      return(NULL)
  }
  data(list=name, package="Amelia")
  putAmelia("amelia.data", eval(as.name(name)))
  putAmelia("am.filetype", "demo")
  putAmelia("am.filename", name)
  activateGUI()
}

drawMissMap <- function() {
  if (.Platform$OS.type == "windows")
    windows()
  else
    x11()

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


  tkgrid.remove(getAmelia("error.label"))
  tkgrid.remove(getAmelia("allgood.label"))
  tkgrid(noimps.label, row = 2, column = 7,
         sticky ="e", padx = 10)
  ## Get rid of welcome frame
  if (as.logical(tkwinfo("ismapped", getAmelia("gui.welcome")))) {
    tkgrid.remove(getAmelia("gui.welcome"))
    tkgrid(getAmelia("gui.skel"), row = 0, column = 0, sticky ="news")
    tkgrid(statusbar, sticky = "sew")
  }
  ## initialize values



  ## turn on various forms and buttons
  tkconfigure(getAmelia("output.run"), state = "normal")
                                        #tkconfigure(getAmelia("output.entry"), textvariable=getAmelia("outname"))
                                        #tkconfigure(getAmelia("output.num"), textvariable=getAmelia("outnum"))
  tkentryconfigure(getAmelia("main.menu.file"),"Edit Data...", state="normal")
  tkentryconfigure(getAmelia("main.menu.options"),"Draw Missingness Map", state="normal")
  tkentryconfigure(getAmelia("main.menu.file"),"Save Session...",
                   state = "normal")
  tkentryconfigure(getAmelia("main.menu.options"),"Output File Type...",
                   state = "normal")
  tkentryconfigure(getAmelia("main.menu.options"),"Output File Options...",
                   state = "normal")
  tkconfigure(getAmelia("missmapButton"), state = "normal")
  tkconfigure(getAmelia("editDataButton"), state = "normal")
  tkconfigure(getAmelia("plotHistButton"), state = "normal")
  tkconfigure(getAmelia("showLogButton"), state = "disabled")
  fillMainTree()


  ## Mark factors as ID by default.

  classes <- sapply(getAmelia("amelia.data"), class)
  factorVars <- which(classes == "factor" |
                      classes == "character")

  if (!session) {
    opt.holder <- vector("numeric",ncol(getAmelia("amelia.data")))
    names(opt.holder) <- varnames
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
    rownames(boundsholder) <- varnames

    putAmelia("num.poly",tclVar("0"))
    putAmelia("intercs",tclVar("0"))
    putAmelia("priorsmat",  NULL)
    putAmelia("boundsmat",  boundsholder)
    putAmelia("max.resample", tclVar("1000"))

    putAmelia("outname",  tclVar(filestub))
    putAmelia("outnum",  tclVar("5"))
    putAmelia("empri",  tclVar("0"))
    putAmelia("tsvar", NULL)
    putAmelia("csvar", NULL)
    id.holder <- opt.holder
    id.holder[factorVars] <- 1
    putAmelia("idvar", id.holder)
    for (i in factorVars) {
      tkset(getAmelia("main.tree"), getAmelia("varnames")[i],
            "transform", "ID")
    }
  } else {
    for (i in factorVars) {
      if (all(getAmelia("idvar")[i]==0,
              getAmelia("csvar")!=varnames[i],getAmelia("noms")[i]==0)) {
        tcl(getAmelia("main.tree"), "item", varnames[i], image = redFlagIcon)
      }
    }
  }
  tkentryconfigure(getAmelia("main.menu.options"), "Add Observations Priors...", state="normal")

  tkentryconfigure(getAmelia("main.menu.options"), "Numerical Options", state="normal")
  ## add the filename and rows/cols to statusbar
  tkconfigure(getAmelia("statusbar.lab1b"), text = getAmelia("am.filename"), foreground = "blue")
  tkconfigure(getAmelia("statusbar.n"), text = paste(nrow(getAmelia("amelia.data"))), foreground = "blue")
  tkconfigure(getAmelia("statusbar.k"), text = paste(ncol(getAmelia("amelia.data"))), foreground = "blue")

}

save.session <- function() {
  if (is.null(getAmelia("amelia.data"))) {
    tkmessageBox(parent=getAmelia("gui"), message="You must load a dataset before you can save a session.", icon="error", type="ok")
    return(NULL)
  }
  file.select <- tclvalue(tkgetSaveFile(parent=getAmelia("gui"),
                                        filetypes="{{RData files} {.RData}} {{All files} *}"))
  putAmelia("session.flag", TRUE)
  sessionList <- c("am.directory","amelia.data", "am.filename",
                   "am.filetype", "boundsmat", "csvar", "idvar", "lags",
                   "leads", "lgstc", "logs", "noms", "num.poly",
                   "ords", "outname.value", "outnum.value", "output.log", "outtype.value", "priorsmat",
                   "runState", "seed.value", "session.flag", "splinestime.value", "sqrt", "tol.value",
                   "tsvar", "empri.value", "intercs.value",
                   "max.resample.value", "ameliaObject")
  putAmelia("empri.value", tclvalue(getAmelia("empri")))
  putAmelia("intercs.value", tclvalue(getAmelia("intercs")))
  putAmelia("max.resample.value", tclvalue(getAmelia("max.resample")))
  putAmelia("outname.value", tclvalue(getAmelia("outname")))
  putAmelia("outnum.value", tclvalue(getAmelia("outnum")))
  putAmelia("outtype.value", tclvalue(getAmelia("outtype")))
  putAmelia("seed.value", tclvalue(getAmelia("seed")))
  putAmelia("tol.value", tclvalue(getAmelia("tol")))
  putAmelia("splinestime.value", tclvalue(getAmelia("splinestime")))

  save(list = sessionList, envir=ameliaEnv(), file = file.select)
  return(NULL)
}

load.session <- function() {

  ## diaglog to get RData file
  file.select <- tclvalue(tkgetOpenFile(parent=getAmelia("gui"), filetypes=
                                        "{{RData files} {.RData}} {{All files} *}"))
  if (nchar(file.select) <= 0)
    return(NULL)

  ## try loading the RData file and stop if it doesn't work
  tryloadsess <- try(load(file=file.select, envir=ameliaEnv()), silent=TRUE)

  if (inherits(tryloadsess,"try-error")) {
    tkmessageBox(parent=getAmelia("gui"),message="Error loading session.  This is not a valid session file.",icon="error",type="ok")
    return(NULL)
  }

  ## make sure that the RData file loaded the right list
  if (!("session.flag" %in% ls(ameliaEnv())) | !getAmelia("session.flag")) {
    tkmessageBox(parent=getAmelia("gui"), message="Not an Amelia session file.  Try again.",icon="error",type="ok")
    return(NULL)
  }
  activateGUI(session = TRUE)

  nn <- ncol(getAmelia("amelia.data"))
  if (!is.null(getAmelia("tsvar"))) {
    tcl(getAmelia("main.tree"), "item", getAmelia("tsvar"), image = clockIcon)
    tkentryconfigure(getAmelia("main.menu.options"),0, state="normal")
    for (i in 1:nn) {
      if (getAmelia("lags")[i] == 1)
        tkset(getAmelia("main.tree"), varnames[i], "lag", "X")
      if (getAmelia("leads")[i] == 1)
        tkset(getAmelia("main.tree"), varnames[i], "lead", "X")
    }
  }
  if (!is.null(getAmelia("csvar"))) {
    tcl(getAmelia("main.tree"), "item", getAmelia("csvar"), image = userIcon)
    tkentryconfigure(getAmelia("main.menu.options"), 1, state="normal")
    tkentryconfigure(getAmelia("main.menu.options"), 1, variable = getAmelia("intercs"))
  }

  for (i in 1:nn) {
    if (getAmelia("idvar")[i] == 1)
      tkset(getAmelia("main.tree"), varnames[i], "transform", "ID")
    if (getAmelia("ords")[i] == 1)
      tkset(getAmelia("main.tree"), varnames[i], "transform", "Ordinal")
    if (getAmelia("noms")[i] == 1)
      tkset(getAmelia("main.tree"), varnames[i], "transform", "Nominal")
    if (getAmelia("logs")[i] == 1)
      tkset(getAmelia("main.tree"), varnames[i], "transform", "Log")
    if (getAmelia("sqrt")[i] == 1)
      tkset(getAmelia("main.tree"), varnames[i], "transform", "Square Root")
    if (getAmelia("lgstc")[i] == 1)
      tkset(getAmelia("main.tree"), varnames[i], "transform", "Logistic")
  }
  for (i in 1:nn) {
    bdMin <- getAmelia("boundsmat")[i,2]
    bdMax <- getAmelia("boundsmat")[i,3]
    if (!is.na(bdMin)) {
      treeBounds <- paste("[",bdMin,", ", bdMax,"]", sep = "")
    } else {
      treeBounds <- ""
    }
    tkset(getAmelia("main.tree"), varnames[i], "bounds", treeBounds)
  }

  tcl("set", getAmelia("seed"), getAmelia("seed.value"))
  tcl("set", getAmelia("tol"), getAmelia("tol.value"))
  tcl("set", getAmelia("empri"), getAmelia("empri.value"))
  tcl("set", getAmelia("outname"), getAmelia("outname.value"))
  tcl("set", getAmelia("outnum"), getAmelia("outnum.value"))
  tcl("set", getAmelia("outtype"), getAmelia("outtype.value"))
  tcl("set", getAmelia("intercs"), getAmelia("intercs.value"))
  tcl("set", getAmelia("splinestime"), getAmelia("splinestime.value"))
  tcl("set", getAmelia("max.resample"),
      getAmelia("max.resample.value"))

  tkgrid.remove(getAmelia("noimps.label"))
  tkgrid.remove(getAmelia("error.label"))
  tkgrid.remove(getAmelia("allgood.label"))
  tkgrid(getAmelia(paste(getAmelia("runState"),"label", sep = ".")),
         row = 2, column = 7, sticky ="e", padx = 10)
  if (getAmelia("runState") != "noimps") {
    tkentryconfigure(getAmelia("main.menu.output"), "Output Log",
                     state="normal")
    tkconfigure(getAmelia("showLogButton"), state = "normal")
  }
  if (getAmelia("runState") == "allgood") {
    tkentryconfigure(getAmelia("main.menu.output"), 0,
                     state = "normal")
    tkentryconfigure(getAmelia("main.menu.output"), 2,
                     state = "normal")
    resave <- tkmessageBox(parent = getAmelia("gui"), message =
                           "Re-save imputed data sets to the working directory?", icon =
                           "question", default = "yes", type = "yesno")
    if (tclvalue(resave) == "yes") {
      amelia.save(getAmelia("ameliaObject"),
                  tclvalue(getAmelia("outname")), as.numeric(tclvalue(getAmelia("outnum"))))
    }

  }
  return(NULL)
}


run.amelia <- function() {
  save.type <- as.numeric(tclvalue(getAmelia("outtype")))
  if (file.access(getwd(), mode = 2) == -1 & !(save.type %in% c(0,6)))
    {
      tkmessageBox(parent = getAmelia("gui"), message =
                   "The current working directory is not writable. Please select a different working directory or chose to not save the imputed data sets.",
                   type ="ok")
      return(NULL)

    }

  ## Let's not allow people to overwrite their data.
  temp.list <- strsplit(getAmelia("am.filename"),"/")[[1]]
  filename <- temp.list[length(temp.list)]
  outfiles <- paste(tclvalue(getAmelia("outname")),
                    1:as.numeric(tclvalue(getAmelia("outnum"))), sep
                    ="")
  save.type <- as.numeric(tclvalue(getAmelia("outtype")))
  exten <- switch(save.type, "csv","txt","dta","dta","RData")
  outfiles <- paste(outfiles, exten, sep = ".")
  outfiles <- paste(paste(temp.list[-length(temp.list)], collapse =
                          "/"), outfiles, sep = "/")
  if (getAmelia("am.filename") %in% outfiles) {
    tkmessageBox(parent = getAmelia("gui"), message =
                 "Current settings would overwrite the original data. Please change the output file name.",
                 icon = "error", type ="ok")
    return(NULL)
  }


  ts <- getAmelia("tsvar")
  cs <- getAmelia("csvar")
  nn <- ncol(getAmelia("amelia.data"))

  am.intercs  <- as.logical(as.numeric(tclvalue(getAmelia("intercs"))))
  sptime <- as.numeric(tclvalue(getAmelia("splinestime")))

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

  tol <- as.numeric(tclvalue(getAmelia("tol")))
  max.re <- as.numeric(tclvalue(getAmelia("max.resample")))
  num.imp <- as.numeric(tclvalue(getAmelia("outnum")))
  emp <- as.numeric(tclvalue(getAmelia("empri")))
  if (!is.na(as.numeric(tclvalue(getAmelia("seed")))))
    set.seed(as.numeric(tclvalue(getAmelia("seed"))))
  tkgrid.remove(getAmelia("noimps.label"))
  tkgrid.remove(getAmelia("error.label"))
  tkgrid.remove(getAmelia("allgood.label"))
  tkgrid(getAmelia("runAmeliaProgress"), row = 2, column = 7,
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
  putAmelia("ameliaObject",
            try(amelia.default(x        = getAmelia("amelia.data"),
                               m        = as.numeric(tclvalue(getAmelia("outnum"))),
                               p2s      = FALSE,
                               idvars   = id,
                               ts       = ts,
                               cs       = cs,
                               priors   = pmat,
                               lags     = amlags,
                               empri    = as.numeric(tclvalue(getAmelia("empri"))),
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
                               max.resample = as.numeric(tclvalue(getAmelia("max.resample"))),
                               tolerance= as.numeric(tclvalue(getAmelia("tol")))),
                silent=TRUE))
  tkgrid.remove(getAmelia("runAmeliaProgress"))
  tkconfigure(getAmelia("runAmeliaProgress"), value = 0)
  ## check for errors in the process.
  if (inherits(getAmelia("ameliaObject"),"try-error")) {
    putAmelia("output.log", c(getAmelia("output.log"),"\nThere was an unexpected error in the execution of Amelia.  \nDouble check all inputs for errors and take note of the error message:\n\n"))
    putAmelia("output.log", c(getAmelia("output.log"),paste(getAmelia("ameliaObject"))))
                                        #tkconfigure(getAmelia("pass.fail.label"), foreground = "red")
                                        #tmp <- getAmelia("pass.fail")
                                        #tclvalue(tmp) <- "Error! See log."
    show.output.log()
    tkentryconfigure(getAmelia("main.menu.output"), 1, state =
                     "normal")
    tkconfigure(getAmelia("showLogButton"), state = "normal")
    tkgrid(getAmelia("error.label"), row = 2, column = 7,
           sticky ="e", padx = 10)
    putAmelia("runState", "error")
    return(NULL)
  }
  if (all(getAmelia("ameliaObject")$code!=c(1,2))) {
    putAmelia("output.log", c(getAmelia("output.log"),"\n"))
    putAmelia("output.log", c(getAmelia("output.log"),paste("Amelia Error Code:",
                                                            getAmelia("ameliaObject")[[1]],"\n",
                                                            getAmelia("ameliaObject")[[2]])))
                                        #tkconfigure(getAmelia("pass.fail.label"), foreground = "red")
                                        #tmp <- getAmelia("pass.fail")
                                        #tclvalue(tmp) <- "Error! See log."
    show.output.log()
    tkentryconfigure(getAmelia("main.menu.output"), 1, state =
                     "normal")
    tkconfigure(getAmelia("showLogButton"), state = "normal")
    tkgrid(getAmelia("error.label"), row = 2, column = 7,
           sticky ="e", padx = 10)
    putAmelia("runState", "error")
  } else {
    putAmelia("output.log", c(getAmelia("output.log"),"Amelia has run successfully.\n"))
    tkentryconfigure(getAmelia("main.menu.output"), 0, state = "normal")
    tkentryconfigure(getAmelia("main.menu.output"), 1, state =
                     "normal")
    tkentryconfigure(getAmelia("main.menu.output"), 2, state = "normal")
    tkconfigure(getAmelia("showLogButton"), state = "normal")
    amelia.save(getAmelia("ameliaObject"),
                tclvalue(getAmelia("outname")), as.numeric(tclvalue(getAmelia("outnum"))))
    tkgrid(getAmelia("allgood.label"), row = 2, column = 7,
           sticky ="e", padx = 10)
    putAmelia("runState", "allgood")
  }

}

amelia.save <- function(out,outname,m)  {
  save.type <- as.numeric(tclvalue(getAmelia("outtype")))
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
    save(list = "ameliaObject", envir = ameliaEnv(),
         file = paste(outname, ".RData", sep = ""))
  }
  if (save.type == 9)
    assign(x = outname, value = out,envir = .GlobalEnv)
}

set.out<-function(...) {
  putAmelia("output.select",as.numeric(tkget(getAmelia("output.drop.box"))))
}

setTS <- function() {
  tsvartemp <- strsplit(tclvalue(tcl(getAmelia("main.tree"),"selection")), " ")[[1]]
  if (length(tsvartemp) > 1) {
    tkmessageBox(parent=getAmelia("gui"), message="Only one variable can be set as the times-series variable.",icon="error",type="ok")
    return(NULL)
  }
  if (!is.null(getAmelia("csvar"))) {
    if (getAmelia("csvar") == tsvartemp) {
      tkmessageBox(parent=getAmelia("gui"), message="A variable cannot be both the time-series and cross-section index.",icon="error",type="ok")
      return(NULL)
    }
  }
  if (!(sapply(getAmelia("amelia.data"), class)[tsvartemp] %in% c("numeric","integer"))) {
    tkmessageBox(parent=getAmelia("gui"),
                 message="The time-series index must be numeric.",icon="error",type="ok")
    return(NULL)
  }
  children <- strsplit(tclvalue(tcl(getAmelia("main.tree"),"children","")), " ")[[1]]
  for(i in setdiff(children, getAmelia("csvar")))
    tcl(getAmelia("main.tree"), "item", i , image="")

  tcl(getAmelia("main.tree"), "item", tsvartemp, image = getAmelia("clockIcon"))
  putAmelia("tsvar", tsvartemp)
  tkentryconfigure(getAmelia("main.menu.options"),0, state="normal")
  dropTrans()
}

unsetTS <- function() {
  tsvartemp <- strsplit(tclvalue(tcl(getAmelia("main.tree"),"selection")), " ")[[1]]
  sure<-tkmessageBox(parent=getAmelia("gui"), message="If you unset the time-series variable, you will lose any time-series settings such as lags, leads, or polynomials of time. Unset the time-series variable?",icon="question",type="yesno")
  if (tclvalue(sure) == "no")
    return(NULL)

  tcl(getAmelia("main.tree"), "item", tsvartemp, image = "")
  putAmelia("tsvar", NULL)
  tkentryconfigure(getAmelia("main.menu.options"),0, state="disabled")
  putAmelia("lags",vector("numeric",ncol(getAmelia("amelia.data"))))
  putAmelia("leads",vector("numeric",ncol(getAmelia("amelia.data"))))
  children <- strsplit(tclvalue(tcl(getAmelia("main.tree"),"children","")), " ")[[1]]
  for(i in children) {
    tkset(getAmelia("main.tree"), i, "lag", "")
    tkset(getAmelia("main.tree"), i, "lead", "")
  }
}

unsetCS <- function() {
  csvartemp <- strsplit(tclvalue(tcl(getAmelia("main.tree"),"selection")), " ")[[1]]
  sure<-tkmessageBox(parent=getAmelia("gui"), message="If you unset the cross-section variable, you will lose any cross-section settings. Unset the cross-section variable?",icon="question",type="yesno")
  if (tclvalue(sure) == "no")
    return(NULL)

  tcl(getAmelia("main.tree"), "item", csvartemp, image = "")
  putAmelia("csvar", NULL)
  tkentryconfigure(getAmelia("main.menu.options"),0, state="normal")
  if (is.factor(getAmelia("amelia.data")[,csvartemp]) |
      is.character(getAmelia("amelia.data")[,csvartemp])) {
    tcl(getAmelia("main.tree"), "item", csvartemp, image = redFlagIcon)
  }
}

setCS <- function() {
  csvartemp <- strsplit(tclvalue(tcl(getAmelia("main.tree"),"selection")), " ")[[1]]
  if (length(csvartemp) > 1) {
    tkmessageBox(parent=getAmelia("gui"), message="Only one variable can be set as the cross-section variable.",icon="error",type="ok")
    return(NULL)
  }
  if (!is.null(getAmelia("tsvar"))) {
    if (getAmelia("tsvar") == csvartemp) {
      tkmessageBox(parent=getAmelia("gui"), message="A variable cannot be both the time-series and cross-section index.",icon="error",type="ok")
      return(NULL)
    }
  }

  if (!is.null(getAmelia("csvar"))) {
    if (is.factor(getAmelia("amelia.data")[,getAmelia("csvar")]) |
        is.character(getAmelia("amelia.data")[,getAmelia("csvar")])) {
      tcl(getAmelia("main.tree"), "item", getAmelia("csvar"), image = redFlagIcon)
    } else {
      tcl(getAmelia("main.tree"), "item", getAmelia("csvar"), image = "")
    }
  }

  dropTrans()
  tcl(getAmelia("main.tree"), "item", csvartemp, image = getAmelia("userIcon"))
  putAmelia("csvar", csvartemp)
  tkentryconfigure(getAmelia("main.menu.options"),1,state="normal")
  tkentryconfigure(getAmelia("main.menu.options"), 1, variable = getAmelia("intercs"))

}


fillMainTree <- function() {
  children <- strsplit(tclvalue(tcl(getAmelia("main.tree"),"children","")), " ")[[1]]
  tkdelete(getAmelia("main.tree"), children)
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
    tkinsert(getAmelia("main.tree"),"","end", id = i,tag="normal",text
             = i, values = vals)
  }
  bandTree()
  return()
}

AmeliaView<-function() {

  ##Preamble
  require(tcltk) || stop("The package 'tcltk' is required")
  require(foreign)

  if (.Platform$OS.type != "windows") {
    tcl("ttk::style", "theme", "use", "clam")
    tkfont.configure("TkHeadingFont", weight="normal")
    tkfont.configure("TkCaptionFont", weight="normal")
  }

  ## If the current working directory is not writable, move to a
  ## sensible default locations: the HOME dir
  if (file.access(getwd(), mode = 2) == -1) {
    if (file.access(Sys.getenv("HOME"), mode = 0) == 0 &
        file.access(Sys.getenv("HOME"), mode = 2) == 0) {
      setwd(Sys.getenv("HOME"))
    }

  }
  tclServiceMode(on=FALSE)
  putAmelia("outname",    tclVar("outdata"))
  putAmelia("outnum",     tclVar("5"))
  putAmelia("empri",      tclVar("0"))
  putAmelia("tol",        tclVar("0.0001"))
  putAmelia("amelia.data",NULL)
  putAmelia("am.filename",NULL)
  putAmelia("varnames",   NULL)
  putAmelia("tsvar",      NULL)
  putAmelia("csvar",      NULL)
  putAmelia("varmin",     NULL)
  putAmelia("varmax",     NULL)
  putAmelia("runState", "noimps")
  putAmelia("session.flag", FALSE)
  putAmelia("intercs",tclVar("0"))
  putAmelia("splinestime",tclVar("0"))
  putAmelia("outtype", tclVar("1"))
  putAmelia("max.resample", tclVar("1000"))
  putAmelia("inname",     tclVar(""))
  putAmelia("seed",       tclVar(""))
  putAmelia("output.log", NULL)
  putAmelia("boundMin", tclVar(""))
  putAmelia("boundMax", tclVar(""))
  putAmelia("wdForLastImputation", getwd())

  output.types <- c("(no save)",
                    "CSV",
                    "Tab Delimited",
                    "Stata 6",
                    "Stata 7",
                    "Stata 8/9",
                    "Stata 10+",
                    "Stata 10+ (stacked)",
                    "RData",
                    "Hold in R memory")
  ampath <- .find.package(package = "Amelia")[1]
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

  putAmelia("ameliaPic", tkimage.create("photo", file=ameliaFile))
  putAmelia("action.go.icon", tkimage.create("photo", file = goFile))
  putAmelia("tablePic", tkimage.create("photo", file = tableFile))
  putAmelia("rPic", tkimage.create("photo", file = rFile))
  putAmelia("dtaPic", tkimage.create("photo", file = dtaFile))
  putAmelia("spssPic", tkimage.create("photo", file = spssFile))
  putAmelia("clockIcon", tkimage.create("photo", file = clockFile))
  putAmelia("userIcon", tkimage.create("photo", file = userFile))
  putAmelia("worldIcon", tkimage.create("photo", file = worldFile))
  putAmelia("upArrowIcon", tkimage.create("photo", file = upFile))
  putAmelia("downArrowIcon", tkimage.create("photo", file = downFile))
  putAmelia("histIcon", tkimage.create("photo", file = histFile))
  putAmelia("saveIcon", tkimage.create("photo", file = saveFile))
  putAmelia("pageUpIcon", tkimage.create("photo", file = pageUpFile))
  putAmelia("redFlagIcon", tkimage.create("photo", file =
                                          redFlagFile))
  putAmelia("redStopIcon", tkimage.create("photo", file = redStopFile))
  putAmelia("greenCheckIcon", tkimage.create("photo", file = greenCheckFile))
  putAmelia("pageTextIcon", tkimage.create("photo", file =
                                           pageTextFile))
  putAmelia("pageEditIcon", tkimage.create("photo", file = pageEditFile))
  putAmelia("gui", tktoplevel())
  tkwm.title(getAmelia("gui"), "AmeliaView")
  tkwm.protocol(getAmelia("gui"),"WM_DELETE_WINDOW", function() main.close())
  tkwm.geometry(gui, "800x500")
  ##Menu
  putAmelia("main.menu", tkmenu(getAmelia("gui")))
  putAmelia("main.menu.file", tkmenu(main.menu, tearoff=0))
  putAmelia("main.menu.demo", tkmenu(main.menu, tearoff=0))
  putAmelia("main.menu.import", tkmenu(main.menu, tearoff=0))
  putAmelia("main.menu.options", tkmenu(main.menu, tearoff=0))
  putAmelia("main.menu.splines", tkmenu(main.menu, tearoff=0))
  putAmelia("main.menu.output", tkmenu(main.menu, tearoff=0))
  putAmelia("main.menu.help", tkmenu(main.menu, tearoff=0))
  putAmelia("main.menu.variables", tkmenu(main.menu, tearoff=0,
                                          postcommand = variableOptionsPost))
  putAmelia("main.menu.trans", tkmenu(main.menu, tearoff=0))
  putAmelia("main.menu.outfile", tkmenu(main.menu, tearoff=0))
  tkadd(main.menu.file,"command",label="Load R Data File...",command=function()loadRData(),
        underline = 5)
  tkadd(main.menu.import,"command",label="Import comma-separated value data...",
        command=loadCSV, underline = 7)
  tkadd(main.menu.import,"command",label="Import tab-delimited data...",
        command=loadTAB, underline = 7)
  tkadd(main.menu.import,"command",label="Import Stata dta file...",
        command=loadStata, underline = 13)
  tkadd(main.menu.import,"command",label="Import SPSS data...",
        command=loadSPSS, underline = 7)
  tkadd(main.menu.import,"command",label="Import SAS Transport data...",
        command=loadSAS, underline = 8)
  tkadd(main.menu.file,"cascade",menu=main.menu.import,label="Import Data",
        underline = 0)
  tkadd(main.menu.demo,"command",label="africa", command=function()
        loadDemo(name="africa"), underline = 0)
  tkadd(main.menu.demo,"command",label="freetrade", command=function()
        loadDemo(name="freetrade"), underline = 0)
  tkadd(main.menu.file,"cascade",menu=main.menu.demo,label="Load Package Data",
        underline = 5)
  tkadd(main.menu.file,"command",command =
        setWorkingDir,label="Set Working Directory...", underline = 4)
  tkadd(main.menu.file,"command",label="Edit Data...",
        command=function(){putAmelia("amelia.data",
          edit(getAmelia("amelia.data")));updateTreeStats()},state="disabled",
        underline = 0)
  tkadd(main.menu.file,"separator")
  tkadd(main.menu.file,"command",label="Load Session...",command=function()load.session(),
        underline = 0)
  tkadd(main.menu.file,"command",label="Save Session...",command=function()save.session(),
        state="disabled", underline = 0)
  tkadd(main.menu.file,"separator")
  tkadd(main.menu.file,"command",label="Quit Amelia",command=function()main.close(),
        underline = 0)
  tkadd(main.menu.variables, "command", label =
        "Set as Time-Series Variable", command = setTS, state = "disabled",
        underline = 0)
  tkadd(main.menu.variables, "command", label =
        "Set as Cross-Section Variable", command = setCS, state =
        "disabled", underline = 7)
  tkadd(main.menu.variables, "command", label =
        "Unset as Time-Series Variable", command = unsetTS, state =
        "disabled", underline = 0)
  tkadd(main.menu.variables, "command", label =
        "Unset as Cross-Section Variable", command = unsetCS, state =
        "disabled", underline = 23)
  tkadd(main.menu.variables,"separator")
  tkadd(main.menu.variables, "command", label = "Add Lag", command =
        function() addLag(), state = "disabled", underline = 0)
  tkadd(main.menu.variables, "command", label = "Add Lead", command =
        function() addLead(), state = "disabled", underline = 4)
  tkadd(main.menu.variables, "command", label = "Remove Lag", command
        = function() dropLag(), state = "disabled", underline = 0)
  tkadd(main.menu.variables, "command", label = "Remove Lead", command
        = function() dropLead(), state = "disabled", underline = 1)
  tkadd(main.menu.variables,"separator")

  tkadd(main.menu.variables, "command", label =
        "Plot Histogram(s) of Selected", command = plotHist, state =
        "disabled", underline = 0)
  tkadd(main.menu.trans, "command", label = "Log", command =
        function(x) setTrans("logs"), underline = 0)
  tkadd(main.menu.trans, "command", label = "Square Root", command =
        function(x) setTrans("sqrt"), underline = 0)
  tkadd(main.menu.trans, "command", label = "Logistic", command =
        function(x) setTrans("lgstc"), underline = 1)
  tkadd(main.menu.trans, "command", label = "Nominal", command =
        function(x) setTrans("noms"), underline = 0)
  tkadd(main.menu.trans, "command", label = "Ordinal", command =
        function(x) setTrans("ords"), underline = 0)
  tkadd(main.menu.trans, "command", label = "ID Variable", command =
        function(x) setTrans("idvar"), underline = 0)
  tkadd(main.menu.variables, "cascade", label =
        "Add Transformation...", menu = main.menu.trans, state = "disabled",
        underline = 4)
  tkadd(main.menu.variables, "command", label =
        "Remove Transformations", command = dropTrans, state =
        "disabled", underline = 2)
  tkadd(main.menu.variables,"separator")
  tkadd(main.menu.variables, "command", label =
        "Add or Edit Bounds", command = addBounds, state = "disabled",
        underline = 12)
  for (i in 0:10)
    tkadd(main.menu.splines, "radiobutton", variable =
          getAmelia("splinestime"), label = paste(i,"knots"), value = i,
          underline = 0)
  tkadd(main.menu.options, "cascade", label =
        "Splines of Time with...", menu=main.menu.splines,
        state="disabled", underline = 0)
  tkadd(main.menu.options, "checkbutton", label =
        "Interact Spline With Cross-Section?", variable =
        getAmelia("intercs"), onvalue=1,offvalue=0, state="disabled",
        underline = 0)

  tkadd(main.menu.options,"separator")
  tkadd(main.menu.options,"command", label =
        "Add Observations Priors...", command = gui.pri.setup,
        state="disabled", underline = 17)
  tkadd(main.menu.options, "separator")
  tkadd(main.menu.options, "command", label = "Numerical Options",
        command = buildNumericalOptions, state = "disabled", underline
        = 0)
  tkadd(main.menu.options, "command", label = "Draw Missingness Map",
        command = drawMissMap, state="disabled", underline = 5)
  tkadd(main.menu.options, "command", label = "Output File Options...",
        command = buildOutputOptions, state = "disabled", underline = 0)
  for (i in 1:length(output.types)) {
    tkadd(main.menu.outfile, "radiobutton", variable =
          getAmelia("outtype"), label = output.types[i], value = i-1)
  }
  tkadd(main.menu.options, "cascade", label = "Output File Type...",
        menu = main.menu.outfile, state = "disabled", underline = 7)
  tkadd(main.menu.output,"command", label =
        "Imputation Diagnostics...", command = gui.diag.setup,
        state="disabled", underline = 11)
  tkadd(main.menu.output,"command", label = "Output Log", command =
        show.output.log, state="disabled", underline = 0)
  tkadd(main.menu.output,"command", label =
        "Open Folder Containing Imputated Data", command =
        showImputedFiles, state="disabled", underline = 12)
  tkadd(main.menu.help,"command",label="Amelia Website",command=
        function()browseURL("http://gking.harvard.edu/amelia/"),
        underline = 7)
  tkadd(main.menu.help,"command",label="Documentation",command=
        function() browseURL("http://gking.harvard.edu/amelia/docs/"),
        underline = 0)

  tkadd(main.menu.help,"command",label="About...",command=
        function()buildAboutDialog(), underline = 0)

  tkadd(main.menu,"cascade",label="File",menu=main.menu.file,
        underline = 0)
  tkadd(main.menu,"cascade",label="Variables",menu=main.menu.variables,
        underline = 0)
  tkadd(main.menu,"cascade",label="Options",menu=main.menu.options,
        underline = 0)
  tkadd(main.menu,"cascade",label="Output",menu=main.menu.output,
        underline = 1)
  tkadd(main.menu,"cascade",label="Help",menu=main.menu.help,
        underline = 0)
  tkconfigure(getAmelia("gui"), menu = main.menu)


  ## Welcome Screen
  putAmelia("gui.welcome", ttkframe(gui))
  ameliaPicLabel <- ttklabel(gui.welcome, relief = "groove", image = getAmelia("ameliaPic"))
  loadRButton <- ttkbutton(gui.welcome, text = "Load R Data",
                           image = rPic, compound = "top",
                           command = loadRData)
  loadCSVButton <- ttkbutton(gui.welcome, text = "Import CSV",
                             image = tablePic, compound = "top",
                             command = loadCSV)
  loadStataButton <- ttkbutton(gui.welcome, text = "Import STATA",
                               image = dtaPic, compound = "top",
                               command = loadStata)
  loadSPSSButton <- ttkbutton(gui.welcome, text = "Import SPSS",
                              image = spssPic, compound = "top",
                              command = loadSPSS)
  loadDemoButton <- ttkbutton(gui.welcome, text = "Load Demo",
                              image = tablePic, compound = "top",
                              command = function () loadDemo(name = "africa"))

  tkgrid(ameliaPicLabel, row = 0, column = 0, columnspan = 6, padx =
         10, pady = 10)
  tkgrid(ttklabel(gui.welcome,
                  text=paste("Welcome to AmeliaView ",packageDescription("Amelia",
                    fields="Version"), "!", sep="")),
         row = 1, column = 0, columnspan = 6, padx = 10, pady = 10)
  tkgrid(ttklabel(gui.welcome, text="Please load a dataset:"),
         row = 2, column = 0, columnspan = 6, padx = 10, pady = 10)
  tkgrid(loadRButton, row = 3, column = 0, padx = 10, pady = 10)
  tkgrid(loadCSVButton, row = 3, column = 1, padx = 10, pady = 10)
  tkgrid(loadStataButton, row = 3, column = 2, padx = 10, pady = 10)
  tkgrid(loadSPSSButton, row = 3, column = 3, padx = 10, pady = 10)
  tkgrid(loadDemoButton, row = 3, column = 4, padx = 10, pady = 10)
  tkgrid(gui.welcome, row = 0, column = 0)
  ##Frame
  putAmelia("gui.skel", ttkpanedwindow(getAmelia("gui"), orient = "vertical"))

###############
### Toolbar ###
###############

  toolbar <- ttkframe(gui.skel)
  putAmelia("loadSessionButton",
            ttkbutton(toolbar, text = "Load Session",
                      command = load.session, image = pageUpIcon, compound = "top",
                      style="Toolbutton"))
  putAmelia("saveSessionButton",
            ttkbutton(toolbar, text = "Save Session",
                      command = save.session, image = saveIcon, compound = "top",
                      style="Toolbutton"))
  putAmelia("plotHistButton",
            ttkbutton(toolbar, text = "Plot Histogram", state =
                      "disabled", command = plotHist, image = histIcon, compound = "top",
                      style="Toolbutton"))
  putAmelia("editDataButton",
            ttkbutton(toolbar, text = "Edit Data", state = "disabled",
                      command = function(){putAmelia("amelia.data", edit(getAmelia("amelia.data")));updateTreeStats()}, image =
                      pageEditIcon, compound = "top",
                      style="Toolbutton"))
  putAmelia("missmapButton",
            ttkbutton(toolbar, text = "Missingness Map", state = "disabled",
                      command = drawMissMap, image =
                      worldIcon, compound = "top",
                      style="Toolbutton"))
  putAmelia("output.run",
            ttkbutton(toolbar,text="Impute!", state = "disabled",
                      command = run.amelia, image =
                      action.go.icon, compound = "top",
                      style="Toolbutton"))

  putAmelia("showLogButton",
            ttkbutton(toolbar, text = "Output Log", state = "disabled",
                      command = show.output.log, image =
                      pageTextIcon, compound = "top",
                      style="Toolbutton"))
  tkgrid(loadSessionButton, row =0, column = 0, sticky = "ew")
  tkgrid(saveSessionButton, row =0, column = 1, sticky = "ew")
  tkgrid(ttkseparator(toolbar, orient = "vertical"), row = 0, column =
         2, padx=5, pady=5, sticky="ns")
  tkgrid(plotHistButton, row = 0, column = 3, sticky = "ew")
  tkgrid(editDataButton, row = 0, column = 4, sticky = "ew")
  tkgrid(missmapButton, row = 0, column = 5, sticky="ew")
  tkgrid(ttkseparator(toolbar, orient = "vertical"), row = 0, column =
         6, padx=5, pady=5, sticky="ns")
  tkgrid(output.run, row = 0 , column = 7, sticky = "ew")
  tkgrid(showLogButton, row = 0, column = 8, sticky = "ew")

##########################
### Variable Dashboard ###
##########################

  dashboard   <- ttkframe(gui.skel)

  yscr <- ttkscrollbar(dashboard,  orient = "vertical",
                       command=function(...)tkyview(getAmelia("main.tree"),...))
  xscr <- ttkscrollbar(dashboard, orient = "horizontal",
                       command=function(...)tkxview(getAmelia("main.tree"),...))

  sorts <- rep(FALSE, times = 10)
  names(sorts) <- c("#0","transform","lag", "lead","bounds", "min", "max",
                    "mean", "sd", "miss")
  putAmelia("sortDirs", sorts)
  putAmelia("main.tree", ttktreeview(dashboard, columns =
                                     "transform lag lead bounds  min max mean sd miss",
                                     yscrollcommand=function(...)tkset(yscr,...), xscrollcommand=function(...)tkset(xscr,...),
                                     selectmode = "extended"))

                                        #putAmelia("sum.right.click",tkmenu(getAmelia("main.tree"), tearoff = FALSE) )
                                        #tkadd(getAmelia("sum.right.click"), "command", label = "Plot Histogram of Selected", command = function() sum.plot())
                                        #tkbind(getAmelia("main.tree"), "<Button-3>", RightClick)
                                        #putAmelia("sum.right.dis",tkmenu(getAmelia("main.tree"), tearoff = FALSE) )
                                        #tkadd(getAmelia("sum.right.dis"), "command", label = "Plot Histogram of Selected", state = "disabled")

  tcl(getAmelia("main.tree"), "column", "#0", width = 70, minwidth = 80)
  tcl(getAmelia("main.tree"), "column", 0, width = 78, minwidth = 78, anchor = "center")
  tcl(getAmelia("main.tree"), "column", 1, width = 20, minwidth = 20, anchor = "center")
  tcl(getAmelia("main.tree"), "column", 2, width = 20, minwidth = 20,
      anchor = "center")
  tcl(getAmelia("main.tree"), "column", 3, width = 50, minwidth = 50, anchor = "e")
  tcl(getAmelia("main.tree"), "column", 4, width = 50, minwidth = 50, anchor = "e")
  tcl(getAmelia("main.tree"), "column", 5, width = 50, minwidth = 50, anchor = "e")
  tcl(getAmelia("main.tree"), "column", 6, width = 50, minwidth = 50, anchor = "e")
  tcl(getAmelia("main.tree"), "column", 7, width = 50, minwidth = 50, anchor = "e")
  tcl(getAmelia("main.tree"), "column", 8, width = 50, minwidth = 50, anchor = "e")

  tcl(getAmelia("main.tree"), "heading", "#0", text = "Variable",
      command = function() sortTreeBy("#0"))
  tcl(getAmelia("main.tree"), "heading", 0, text = "Transformation",
      command = function() sortTreeBy("transform"))
  tcl(getAmelia("main.tree"), "heading", 1, text = "Lag",
      command = function() sortTreeBy("lag"))
  tcl(getAmelia("main.tree"), "heading", 2, text = "Lead",
      command = function() sortTreeBy("lead"))
  tcl(getAmelia("main.tree"), "heading", 3, text = "Bounds",
      command = function() sortTreeBy("lower"))
  tcl(getAmelia("main.tree"), "heading", 4, text = "Min",
      command = function() sortTreeBy("min"))
  tcl(getAmelia("main.tree"), "heading", 5, text = "Max",
      command = function() sortTreeBy("max"))
  tcl(getAmelia("main.tree"), "heading", 6, text = "Mean",
      command = function() sortTreeBy("mean"))
  tcl(getAmelia("main.tree"), "heading", 7, text = "SD",
      command = function() sortTreeBy("sd"))
  tcl(getAmelia("main.tree"), "heading", 8, text = "Missing",
      command = function() sortTreeBy("miss"))
  tkbind(getAmelia("main.tree"), "<Button-3>", mainTreeRightClick)

  ## Windows 7 doesn't handle treeview selection correctly
  selectbg <- tcl("ttk::style","configure",".","-selectbackground")
  selectfg <- tcl("ttk::style","configure",".","-selectforeground")
  tktag.configure(getAmelia("main.tree"),"normal", background="white")
  tktag.configure(getAmelia("main.tree"),"selected",
                  background=selectbg, foreground=selectfg)
  tkbind(getAmelia("main.tree"),"<<TreeviewSelect>>",function()
         refreshSelection(getAmelia("main.tree")))
  putAmelia("legendFrame", ttkframe(dashboard))
  tkgrid(ttklabel(legendFrame, text="= Time-Series Variable", image =
                  clockIcon, compound = "left"), row = 0, column = 0,  sticky="w",
         padx = 5)
  tkgrid(ttklabel(legendFrame, text="= Cross-Section Variable", image =
                  userIcon, compound = "left"), row = 0, column = 1,
         sticky="w", padx = 5)
  tkgrid(ttklabel(legendFrame, text="= Unhandled Factor Variable", image =
                  redFlagIcon, compound = "left"), row = 0, column =
         2, sticky="w", padx = 5)

  tkgrid(main.tree, row=0,column=0, sticky="news")
  tkgrid(yscr, row = 0, column = 1, sticky = "ns")
  tkgrid(xscr, row = 1, column = 0, sticky = "ew")
  tkgrid(legendFrame, row = 2, column = 0, sticky = "ew")
  tkgrid.rowconfigure(dashboard, 0, weight = 1)
  tkgrid.columnconfigure(dashboard, 0, weight = 1)
  ##Output Frame
  ##output options, run button, diag

  ##output options


  ##grid the whole thing
  tkadd(gui.skel, toolbar)
  tkadd(gui.skel, dashboard)

  tkgrid(toolbar, row = 0, column = 1, padx = 2, pady=2, sticky = "ew")
  tkgrid(dashboard,row = 1,  column = 1, sticky = "news", padx = 10,
         pady = 5)

  tkgrid.rowconfigure(gui.skel, 1, weight = 1)
  tkgrid.columnconfigure(gui.skel, 1, weight = 1)
                                        #tkgrid(gui.skel,sticky="news")
  tkgrid.rowconfigure(gui, 0, weight = 1)
  tkgrid.columnconfigure(gui, 0, weight = 1)

  ##statusbar at the bottom.
  putAmelia("statusbar", ttkframe(getAmelia("gui"), relief = "groove", borderwidth = 3))
  statusbar.lab1a <- ttklabel(statusbar, text = "Data Loaded:", anchor = "w",
                              padding = c(2,0))
  putAmelia("statusbar.lab1b",
            ttklabel(statusbar, text = "Unspecified", relief = "sunken",
                     anchor = "w", foreground = "red",padding = c(2,0),
                     width = 35))
  statusbar.nlab <- ttklabel(statusbar, text = "Obs:", anchor="e", padding = c(2,0))
  putAmelia("statusbar.n",
            ttklabel(statusbar, text = "----", relief = "sunken",
                     anchor = "w", foreground = "red",padding = c(2,0,0,0),
                     width = 6))

  statusbar.klab <- ttklabel(statusbar, text = "Vars:", anchor="e",
                             padding = c(2,0))

  putAmelia("statusbar.k",
            ttklabel(statusbar, text = "----", relief = "sunken", anchor = "w",
                     foreground = "red", padding = c(2,0,0,0), width = 6))

  putAmelia("runAmeliaProgress",
            ttkprogressbar(statusbar, value = 0, length = 200,
                           mode = "determinate"))
  putAmelia("error.label", ttkbutton(statusbar, text =
                                     "Error! See Output Log.", image =
                                     redStopIcon, compound = "left", style =
                                     "Toolbutton", command = show.output.log))
  putAmelia("allgood.label", ttkbutton(statusbar, text = "Successful Imputation.", image =
                                       greenCheckIcon, compound = "left",
                                       style = "Toolbutton", command = showImputedFiles))
  putAmelia("noimps.label", ttklabel(statusbar, text =
                                     "No imputations run.", justify = "right"))

  tkgrid(statusbar.lab1a,row = 2, column = 1, sticky="w")
  tkgrid(getAmelia("statusbar.lab1b"),row = 2, column = 2, sticky="w")
  tkgrid(statusbar.nlab,row = 2, column = 3, sticky="w")
  tkgrid(getAmelia("statusbar.n"),row = 2, column = 4, sticky="w")
  tkgrid(statusbar.klab,row = 2, column = 5, sticky="w")
  tkgrid(getAmelia("statusbar.k"), row = 2, column = 6, sticky = "w")
  tkgrid(noimps.label, row = 2, column = 7,
         sticky ="e", padx = 10)
  tkgrid.rowconfigure(statusbar, 2, weight = 1)
                                        #tkgrid(statusbar, sticky = "sew")


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
    tkwm.iconbitmap(getAmelia("gui"),file.path(.find.package(package = "Amelia")[1], "gui/amelia.ico"))
  tkraise(getAmelia("gui"))
  tkwm.deiconify(getAmelia("gui"))
  tkfocus(getAmelia("gui"))
  tclServiceMode(on = TRUE)
  tkwait.window(getAmelia("gui"))


}

buildNumericalOptions <- function() {
  onCancel <- function(){
    tcl("set", getAmelia("seed"), temp.seed)
    tcl("set", getAmelia("tol"), temp.tol)
    tkwm.withdraw(numericalWindow)
    tkgrab.release(numericalWindow)
    tkfocus(getAmelia("gui"))
  }

  putAmelia("temp.seed", tclvalue(getAmelia("seed")))
  putAmelia("temp.tol", tclvalue(getAmelia("tol")))

  if (exists("numericalWindow", envir = ameliaEnv())) {
    tkwm.deiconify(getAmelia("numericalWindow"))
    tkraise(getAmelia("numericalWindow"))
    return()
  }

  putAmelia("numericalWindow", tktoplevel())
  tkwm.title(numericalWindow, "Numerical Options")
  numericalBox <- ttkframe(numericalWindow)
  putAmelia("output.seedlab", ttklabel(numericalBox, text="Seed:"))
  putAmelia("output.seed",
            ttkentry(numericalBox, width="7", textvariable=getAmelia("seed")))
  putAmelia("output.tollab", ttklabel(numericalBox, text="Tolerance:"))
  putAmelia("output.tol",
            ttkentry(numericalBox, width="7",
                     textvariable=getAmelia("tol")))
  putAmelia("empri.ent", ttkentry(numericalBox, width=7,textvariable = getAmelia("empri")))
  putAmelia("empri.label", ttklabel(numericalBox,text="Ridge prior:"))
  putAmelia("maxre.ent", ttkentry(numericalBox, width=7,textvariable = getAmelia("max.resample")))
  putAmelia("maxre.label",
            ttklabel(numericalBox,text="Maximum Resample for Bounds:"))

  buttonBox <- ttkframe(numericalBox)
  okButton <- ttkbutton(buttonBox, text = "OK", width = 10, command = function() {tkwm.withdraw(numericalWindow);tkgrab.release(numericalWindow);tkfocus(getAmelia("gui"))})
  cancelButton <- ttkbutton(buttonBox, width = 10, text = "Cancel", command = onCancel)


  tkgrid(getAmelia("output.seedlab"), row = 1, column = 1, sticky = "w", padx = 10, pady = 10)
  tkgrid(getAmelia("output.seed"), row = 1, column = 2, sticky = "w", padx = 10, pady = 10)
  tkgrid(getAmelia("output.tollab"), row = 2, column = 1, sticky =
         "w", padx = 10, pady = 10)
  tkgrid(getAmelia("output.tol"), row = 2, column = 2, sticky = "w",
         padx = 10, pady = 10)
  tkgrid(getAmelia("empri.label"), row = 3, column = 1, sticky = "w", padx = 10, pady = 10)
  tkgrid(getAmelia("empri.ent"), row = 3, column = 2, sticky = "w",
         padx = 10, pady = 10)

  tkgrid(getAmelia("maxre.label"), row = 4, column = 1, sticky = "w", padx = 10, pady = 10)
  tkgrid(getAmelia("maxre.ent"), row = 4, column = 2, sticky = "w",
         padx = 10, pady = 10)
  tkgrid(okButton, row = 0, column = 0, padx = 10, pady = 10)
  tkgrid(cancelButton, row = 0, column = 1, padx = 10, pady = 10)
  tkgrid(buttonBox, row = 5, column = 1, sticky = "e", columnspan = 2)
  tkgrid(numericalBox, sticky = "news")

  tkwm.protocol(numericalWindow, "WM_DELETE_WINDOW", onCancel)

  centerModalDialog(numericalWindow, resize=FALSE)

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
    tcl("set", getAmelia("outname"), temp.name)
    tcl("set", getAmelia("outnum"), temp.num)
    tkwm.withdraw(outputWindow)
    tkgrab.release(outputWindow)
    tkfocus(getAmelia("gui"))
  }

  putAmelia("temp.name", tclvalue(getAmelia("outname")))
  putAmelia("temp.num", tclvalue(getAmelia("outnum")))

  if (exists("outputWindow", envir = ameliaEnv())) {
    tkwm.deiconify(getAmelia("outputWindow"))
    tkraise(getAmelia("outputWindow"))
    return()
  }

  putAmelia("outputWindow", tktoplevel())
  tkwm.title(outputWindow, "Output Options")
  outputBox <- ttkframe(outputWindow)

  putAmelia("output.label", ttklabel(outputBox, text="Name the Imputed Dataset:"))
  putAmelia("output.entry",
            ttkentry(outputBox, width="15",
                     textvariable = getAmelia("outname")))

  putAmelia("output.numlab", ttklabel(outputBox, text = "Number of Imputed Datasets:"))
  putAmelia("output.num",
            ttkentry(outputBox, width = "7",
                     textvariable = getAmelia("outnum")))

  buttonBox <- ttkframe(outputBox)
  okButton <- ttkbutton(buttonBox, text = "OK", width = 10, command = function() {tkwm.withdraw(outputWindow);tkgrab.release(outputWindow);tkfocus(getAmelia("gui"))})
  cancelButton <- ttkbutton(buttonBox, width = 10, text = "Cancel", command = onCancel)


  tkgrid(getAmelia("output.label"), row = 1, column = 1, sticky = "w", padx = 10, pady = 10)
  tkgrid(getAmelia("output.entry"), row = 1, column = 2, sticky = "w", padx = 10, pady = 10)
  tkgrid(getAmelia("output.numlab"), row = 2, column = 1, sticky = "w", padx = 10, pady = 10)
  tkgrid(getAmelia("output.num"), row = 2, column = 2, sticky = "w",
         padx = 10, pady = 10)
  tkgrid(okButton, row = 0, column = 0, padx = 10, pady = 10)
  tkgrid(cancelButton, row = 0, column = 1, padx = 10, pady = 10)
  tkgrid(buttonBox, row = 3, column = 1, sticky = "e", columnspan = 2)
  tkgrid(outputBox, sticky = "news")

  tkwm.protocol(outputWindow, "WM_DELETE_WINDOW", onCancel)

  centerModalDialog(outputWindow, resize=FALSE)

  bindTooltip(widget = "output.entry", tip = "The prefix for the saved imputed datasets. For most saving options they will be in the following format: \n\nmyprefix1.out\nmyprefix2.out\n...\n\nAnd so on, where \"out\" is the file extension.")
  bindTooltip(widget = "output.label", tip = "The prefix for the saved imputed datasets. For most saving options they will be in the following format: \n\nmyprefix1.out\nmyprefix2.out\n...\n\nAnd so on, where \"out\" is the file extension.")
  bindTooltip(widget = "output.num", tip = "Set the number of imputed datasets.\n\nIn many cases, around 5 is sufficient, but if the fraction of missingness is high, you may need more. Use the Summarize Data and Missingness Map above to get a sense for the amount of missingness in your data.")
  bindTooltip(widget = "output.numlab", tip = "Set the number of imputed datasets.\n\nIn many cases, around 5 is sufficient, but if the fraction of missingness is high, you may need more. Use the Summarize Data and Missingness Map above to get a sense for the amount of missingness in your data.")

}



buildAboutDialog <- function() {
  if (exists("aboutWindow", envir = ameliaEnv())) {
    tkwm.deiconify(getAmelia("aboutWindow"))
    tkraise(getAmelia("aboutWindow"))
    return()
  }
  putAmelia("aboutWindow", tktoplevel(parent=getAmelia("gui")))
  tkwm.title(aboutWindow, "About AmeliaView")
  aboutBox <- ttkframe(aboutWindow, height = 150, width = 200)
                                        #ameliaPic <- tkimage.create("photo",file=ameliaFile)
  picLabel <- ttklabel(aboutBox, image=getAmelia("ameliaPic"), relief="groove", borderwidth=2)
  tkgrid(ttkframe(aboutBox,width=100), row=0,column=1)
  tkgrid(ttkframe(aboutBox,height=150,width=0), row=0,column=0,rowspan=3)
  tkgrid(picLabel, row = 1, column=1, pady = 20, padx = 20)
  tkgrid(ttklabel(aboutBox, text=paste("AmeliaView",packageDescription("Amelia", fields="Version")), justify="center"), row = 2, column = 1)
  tkgrid(ttklabel(aboutBox, text="James Honaker, Gary King, Matthew Blackwell", justify="center"), row = 3, column = 1, padx=20)
  tkgrid(ttklabel(aboutBox, text="\uA9 2006-2010", justify="center"), row = 4, column = 1, padx=20)
  buttonBox <- ttkframe(aboutBox)
  closeButton <- ttkbutton(buttonBox, text = "Close", command = function() {tkwm.withdraw(aboutWindow);tkgrab.release(aboutWindow);tkfocus(getAmelia("gui"))}, width = 10)
  websiteButton <- ttkbutton(buttonBox, text = "Website",
                             command = function() browseURL("http://gking.harvard.edu/amelia/"))
  tkgrid(websiteButton, row=0, column = 0, sticky="w", padx=10, pady=10)
  tkgrid(closeButton, row=0, column = 0, sticky="e", padx=10, pady=10)
  tkgrid.columnconfigure(buttonBox, 0, weight=1)
  tkgrid(buttonBox, row=5, column = 1, sticky="ew")
  tkgrid(aboutBox, sticky = "nsew")
  tkwm.protocol(aboutWindow, "WM_DELETE_WINDOW", function() {tkwm.withdraw(aboutWindow);tkgrab.release(aboutWindow);tkfocus(getAmelia("gui"))})

  centerModalDialog(aboutWindow, resize=FALSE)
}


sum.data <-function() {

  RightClick <- function(x, y) { # x and y are the mouse coordinates
    rootx <- as.integer(tkwinfo("rootx", getAmelia("summary.tree")))  # tkwinfo() return several infos
    rooty <- as.integer(tkwinfo("rooty", getAmelia("summary.tree")))
    xTxt <- as.integer(x) + rootx
    yTxt <- as.integer(y) + rooty
                                        # Create a Tcl command in a character string and run it
    var <- tkindex(getAmelia("summary.tree"),tclvalue(tcl(getAmelia("summary.tree"), "selection")))
    var <- as.numeric(var) + 1
    if (is.factor(getAmelia("amelia.data")[,var]) |
        is.character(getAmelia("amelia.data")[,var])) {
      .Tcl(paste("tk_popup", .Tcl.args(getAmelia("sum.right.dis"), xTxt, yTxt)))
    } else {
      .Tcl(paste("tk_popup", .Tcl.args(getAmelia("sum.right.click"), xTxt, yTxt)))
    }
  }
  tt <- tktoplevel()
  tkwm.title(tt, "Data Summary")

  yscr <- ttkscrollbar(tt,  orient = "vertical",
                       command=function(...)tkyview(getAmelia("summary.tree"),...))
  xscr <- ttkscrollbar(tt, orient = "horizontal",
                       command=function(...)tkxview(getAmelia("summary.tree"),...))
  putAmelia("summary.tree", ttktreeview(tt, columns = "Min Max Mean SD Missing",
                                        yscrollcommand=function(...)tkset(yscr,...), xscrollcommand=function(...)tkset(xscr,...),
                                        selectmode = "browse"))

  putAmelia("sum.right.click",tkmenu(getAmelia("summary.tree"), tearoff = FALSE) )
  tkadd(getAmelia("sum.right.click"), "command", label = "Plot Histogram of Selected", command = function() sum.plot())
  tkbind(getAmelia("summary.tree"), "<Button-3>", RightClick)
  putAmelia("sum.right.dis",tkmenu(getAmelia("summary.tree"), tearoff = FALSE) )
  tkadd(getAmelia("sum.right.dis"), "command", label = "Plot Histogram of Selected", state = "disabled")

  tcl(getAmelia("summary.tree"), "column", "#0", width = 80)
  tcl(getAmelia("summary.tree"), "column", 0, width = 80, anchor = "center")
  tcl(getAmelia("summary.tree"), "column", 1, width = 80, anchor = "center")
  tcl(getAmelia("summary.tree"), "column", 2, width = 80, anchor = "center")
  tcl(getAmelia("summary.tree"), "column", 3, width = 80, anchor = "center")
  tcl(getAmelia("summary.tree"), "column", 4, width = 80, anchor = "center")
  tcl(getAmelia("summary.tree"), "heading", "#0", text = "Variable")
  tcl(getAmelia("summary.tree"), "heading", 0, text = "Minimum")
  tcl(getAmelia("summary.tree"), "heading", 1, text = "Maximum")
  tcl(getAmelia("summary.tree"), "heading", 2, text = "Mean")
  tcl(getAmelia("summary.tree"), "heading", 3, text = "SD")
  tcl(getAmelia("summary.tree"), "heading", 4, text = "Missing")
  ## Windows 7 doesn't handle treeview selection correctly
  if (.Platform$OS.type == "windows") {
    tktag.configure(getAmelia("summary.tree"),"normal", background="white")
    tktag.configure(getAmelia("summary.tree"),"selected", background="#107FC9")
    tkbind(getAmelia("summary.tree"),"<<TreeviewSelect>>",function() refreshSelection(getAmelia("summary.tree")))
  }
  for (i in 1:ncol(getAmelia("amelia.data"))) {
    if (is.factor(getAmelia("amelia.data")[,i]) |
        is.character(getAmelia("amelia.data")[,i])) {
      vals <- c("(factor)","...","...","...","...")
    } else {
      vals <- c(min(getAmelia("amelia.data")[,i],na.rm=T), max(getAmelia("amelia.data")[,i],na.rm=T),
                mean(getAmelia("amelia.data")[,i],na.rm=T), sd(getAmelia("amelia.data")[,i],na.rm=T))
      vals <- signif(vals, digits = 4)
      vals <- c(vals,paste(sum(is.na(getAmelia("amelia.data")[,i])),
                           nrow(getAmelia("amelia.data")), sep="/"))
    }
    tkinsert(getAmelia("summary.tree"),"","end", values = vals,tag="normal",
             text = names(getAmelia("amelia.data"))[i])
  }

  tkgrid(getAmelia("summary.tree"), row = 0, column = 0, sticky = "nsew")
  tkgrid(yscr, row = 0, column = 1, sticky = "nse")
  tkgrid(xscr, row = 1, column = 0, sticky = "ews")
                                        #tkgrid(gui.frame, sticky = "nsew")
  tkgrid.rowconfigure(tt, 0, weight = 1)
  tkgrid.columnconfigure(tt, 0, weight = 1)
  centerModalDialog(tt, resize=TRUE)
}




gui.pri.setup <- function() {
  cancelPriors <- function() {
    putAmelia("priorsmat", getAmelia("temp.priorsmat"))
  }
  onOK <- function() {

    nm <- c("dist","range")[getAmeliaInd("addpri.note")+1]
    varBox <- paste("add",nm,"var",sep=".")
    caseBox <- paste("add",nm,"case",sep=".")
    caseSelection <- as.numeric(tcl(getAmelia(caseBox),"current"))
    varSelection  <- as.numeric(tcl(getAmelia(varBox),"current")) +1

    thiscase <- tclvalue(tkget(getAmelia(caseBox)))
    thisvar  <- tclvalue(tkget(getAmelia(varBox)))


    if (caseSelection==0) {
      rowSelection <- 0
      colSelection <- which(anyMissing)[varSelection]
    } else {
      rowSelection  <- missingCases[caseSelection]
      colSelection <- which(is.na(getAmelia("amelia.data")[rowSelection,]))[varSelection]
    }

                                        # fork for range vs. dist
    if (nm == "range") {
      if (tclvalue(getAmelia("priorMin"))=="") {
        tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Please enter a minimum value.",
                     type="ok",icon="error")
        return()
      }
      if (tclvalue(getAmelia("priorMax"))=="") {
        tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Please enter a maximum value.",
                     type="ok",icon="error")
        return()
      }

      if (tclvalue(getAmelia("priorConf"))=="") {
        tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Please enter a confidence value.",
                     type="ok",icon="error")
        return()
      }
      if (isTRUE(as.numeric(tclvalue(getAmelia("priorConf"))) <= 0
                 | as.numeric(tclvalue(getAmelia("priorConf"))) >= 1)) {
        tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Confidence levels must be between 0 and 1.",
                     type="ok",icon="error")
        return()
      }

      prMax <- as.numeric(tclvalue(getAmelia("priorMax")))
      prMin <- as.numeric(tclvalue(getAmelia("priorMin")))
      prCon <- as.numeric(tclvalue(getAmelia("priorConf")))
      if (prMax <= prMin) {
        tkmessageBox(title="Error",
                     message="The max is less than the min.",
                     type="ok",icon="error")
        return()
      }
      prMean<- prMin + ((prMax-prMin)/2)
      prSD  <-(prMax-prMin)/(2*qnorm(1-(1-prCon)/2))


                                        #if dist prior
    } else {
      if (tclvalue(getAmelia("priorMean"))=="") {
        tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Please enter a mean value.",
                     type="ok",icon="error")
        return()
      }
      if (tclvalue(getAmelia("priorSD"))=="") {
        tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Please enter a standard deviation.",
                     type="ok",icon="error")
        return()
      }
      if (isTRUE(as.numeric(tclvalue(getAmelia("priorSD"))) == 0)) {
        tkmessageBox(parent=getAmelia("priorsWindow"), title="Error",
                     message="Standard deviations must be greater than 0.",
                     type="ok",icon="error")
        return()
      }
      prMean <- as.numeric(tclvalue(getAmelia("priorMean")))
      prSD   <- as.numeric(tclvalue(getAmelia("priorSD")))


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
      over <- tkmessageBox(parent=getAmelia("priorsWindow"), title="Overwrite Prior",message=mess,
                           icon="question",type="yesno",default="no")
      if (tclvalue(over)=="no") {
        return()
      } else {
        putAmelia("priorsmat",getAmelia("priorsmat")[-which(matchPrior),])
        tkdelete(getAmelia("priors.tree"), paste(rowSelection,colSelection,sep="-"))
      }
    }

    putAmelia("priorsmat",rbind(getAmelia("priorsmat"),newPrior))

    ## need to change the treeview
                                        #updateTree()
    tkinsert(getAmelia("priors.tree"),"","end", id = paste(rowSelection,colSelection,sep="-"), values = c(thisvar,prMean,prSD),
             text = thiscase,tag="normal")
    resetEntries()
    return()
  }
  validateNumeric <- function(x) {
    if (isTRUE(grep("(^-?[0-9]*\\.?[0-9]*$)",x)==1))
      return(tclVar("TRUE"))
    else
      return(tclVar("FALSE"))
  }
  validateSD <- function(x) {
    if (isTRUE(grep("^[0-9]*\\.?[0-9]*$",x)==1))
      return(tclVar("TRUE"))
    else
      return(tclVar("FALSE"))
  }
  validateConf <- function(x) {
    if (isTRUE(grep("^0*\\.[0-9]*$",x)==1))
      return(tclVar("TRUE"))
    else
      return(tclVar("FALSE"))
  }
  setMissingVars <- function() {

    currentSelection <- as.numeric(tcl(getAmelia("add.dist.case"), "current"))
    currentCase      <- missingCases[currentSelection]
    if (currentSelection==0)
      missVars <- anyMissing
    else
      missVars    <- is.na(getAmelia("amelia.data")[currentCase,])
    missVarNames <- colnames(getAmelia("amelia.data"))[missVars]
    tkconfigure(getAmelia("add.dist.var"),values = missVarNames)
    tcl(getAmelia("add.dist.var"), "current", 0)
  }

  resetEntries <- function() {
    tcl("set", getAmelia("priorMin"),"")
    tcl("set", getAmelia("priorMax"),"")
    tcl("set", getAmelia("priorMean"),"")
    tcl("set", getAmelia("priorSD"),"")
    tcl("set", getAmelia("priorConf"),"")
    return()
  }
  updateTree <- function() {
    allrows <- paste(tcl(getAmelia("priors.tree"),"children",""))
    tkdelete(getAmelia("priors.tree"), allrows)

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
      tkinsert(getAmelia("priors.tree"),"","end", id = paste(getAmelia("priorsmat")[i,1],getAmelia("priorsmat")[i,2],sep="-"), values = c(thisvar,getAmelia("priorsmat")[i,c(3,4)]),
               text = thiscase,tag="normal")
    }
    return()

  }
  dropPriors <- function() {
    sel.pri <- strsplit(tclvalue(tcl(getAmelia("priors.tree"), "selection")), " ")[[1]]
    pri.mat.rows <- c()
    for (i in 1:length(sel.pri)) {
      pri.mat.rows <- c(pri.mat.rows, tclvalue(tkindex(getAmelia("priors.tree"),sel.pri[i])))
    }
    pri.mat.rows <- as.numeric(pri.mat.rows) + 1
    putAmelia("priorsmat", getAmelia("priorsmat")[-pri.mat.rows,, drop = FALSE])
    tkdelete(getAmelia("priors.tree"),paste(tcl(getAmelia("priors.tree"), "selection")))
    if (nrow(getAmelia("priorsmat")) == 0) putAmelia("priorsmat", NULL)
    return(NULL)
  }
  RightClick <- function(x, y) { # x and y are the mouse coordinates
    rootx <- as.integer(tkwinfo("rootx", getAmelia("priors.tree")))  # tkwinfo() return several infos
    rooty <- as.integer(tkwinfo("rooty", getAmelia("priors.tree")))
    xTxt <- as.integer(x) + rootx
    yTxt <- as.integer(y) + rooty
                                        # Create a Tcl command in a character string and run it
    .Tcl(paste("tk_popup", .Tcl.args(getAmelia("pri.right.click"), xTxt, yTxt)))
  }


  putAmelia("temp.priorsmat", getAmelia("priorsmat"))

  if (exists("priorsWindow", envir=ameliaEnv())) {
    updateTree()
    resetEntries()
    tkwm.deiconify(getAmelia("priorsWindow"))
    tkraise(getAmelia("priorsWindow"))
    tkgrab(getAmelia("priorsWindow"))
    return()
  }
  putAmelia("priorsWindow", tktoplevel())
  tkwm.title(priorsWindow,"Observational Priors")

  priorsBox <- ttkframe(priorsWindow)

  prior.frame <- ttkpanedwindow(priorsBox, orient = "horizontal")
  prior.disp  <- ttklabelframe(prior.frame, text = "Observational priors ", height = 200, width = 200)
  prior.add   <- ttklabelframe(prior.frame, text = "Add priors", height = 200, width = 200)
  putAmelia("prior.add.but", ttkbutton(prior.add, text = "Add", command = function() onOK()))

  yscr <- ttkscrollbar(prior.disp,  orient = "vertical",
                       command=function(...)tkyview(getAmelia("priors.tree"),...))
  xscr <- ttkscrollbar(prior.disp, orient = "horizontal",
                       command=function(...)tkxview(getAmelia("priors.tree"),...))
  putAmelia("priors.tree", ttktreeview(prior.disp, columns = "Variable Mean SD",
                                       yscrollcommand=function(...)tkset(yscr,...), xscrollcommand=function(...)tkset(xscr,...)))

  putAmelia("pri.right.click",tkmenu(getAmelia("priors.tree"), tearoff = FALSE) )
  tkadd(getAmelia("pri.right.click"), "command", label = "Remove selected priors", command = function() dropPriors())
  tkbind(getAmelia("priors.tree"), "<Button-3>", RightClick)

  tcl(getAmelia("priors.tree"), "column", "#0", width = 120)
  tcl(getAmelia("priors.tree"), "column", 0, width = 80, anchor = "center")
  tcl(getAmelia("priors.tree"), "column", 1, width = 40, anchor = "center")
  tcl(getAmelia("priors.tree"), "column", 2, width = 40, anchor = "center")
  tcl(getAmelia("priors.tree"), "heading", "#0", text = "Case")
  tcl(getAmelia("priors.tree"), "heading", 0, text = "Variable")
  tcl(getAmelia("priors.tree"), "heading", 1, text = "Mean")
  tcl(getAmelia("priors.tree"), "heading", 2, text = "SD")
  ## Windows 7 doesn't handle treeview selection correctly
  if (.Platform$OS.type == "windows") {
    tktag.configure(getAmelia("priors.tree"),"normal", background="white")
    tktag.configure(getAmelia("priors.tree"),"selected", background="SystemHighlight")
    tkbind(getAmelia("priors.tree"),"<<TreeviewSelect>>",function() refreshSelection(getAmelia("priors.tree")))
  }
  putAmelia("addpri.note", ttknotebook(prior.add))
  add.dist.frame <- ttkframe(getAmelia("addpri.note"))
  add.range.frame <- ttkframe(getAmelia("addpri.note"))



  missingCases <- which(!complete.cases(getAmelia("amelia.data")))
  anyMissing   <- apply(getAmelia("amelia.data"), 2, function(x) any(is.na(x)))

  cases1 <- paste(rownames(getAmelia("amelia.data"))[missingCases], ") ",
                  getAmelia("amelia.data")[missingCases, getAmelia("csvar")]," ",
                  getAmelia("amelia.data")[missingCases, getAmelia("tsvar")], sep="")


  cases <- c("(whole variable)",cases1)

  if (!is.null(getAmelia("priorsmat"))) updateTree()
  vars <- getAmelia("varnames")[anyMissing]


  ## Distribution prior note

  putAmelia("add.dist.case",ttkcombobox(add.dist.frame, values=cases,
                                        state="readonly", width=15))
  putAmelia("add.dist.var",ttkcombobox(add.dist.frame, values=vars,
                                       state="readonly", width=15))
  tkbind(getAmelia("add.dist.case"), "<<ComboboxSelected>>", function(...) setMissingVars())
  tkgrid(ttklabel(add.dist.frame, text="Case:"), column=1, row=1, sticky = "e")
  tkgrid(ttklabel(add.dist.frame, text="Variable:"), column=1, row=2, sticky = "e")
  tcl(getAmelia("add.dist.case"), "current", 0)
  tcl(getAmelia("add.dist.var"), "current", 0)
  tkconfigure(getAmelia("add.dist.var"), postcommand=function(...) setMissingVars())
  tkgrid(getAmelia("add.dist.case"), column=2, row=1, pady=3)
  tkgrid(getAmelia("add.dist.var"),  column=2, row=2, pady=3)



  putAmelia("priorMean", tclVar())
  putAmelia("priorSD", tclVar())


  tkgrid(ttkframe(add.dist.frame, width = 150, height = 0), column = 1, row = 0)
  putAmelia("meanBox", ttkentry(add.dist.frame, textvar=getAmelia("priorMean"), validate="key",
                                validatecommand = function(P) validateNumeric(P)))

  putAmelia("sdBox", ttkentry(add.dist.frame, textvar=getAmelia("priorSD"), validate="key",
                              validatecommand = function(P) validateSD(P)))

  tkgrid(ttklabel(add.dist.frame, text="Mean:"), column=1, row=3, sticky = "e")
  tkgrid(ttklabel(add.dist.frame, text="Standard Deviation:"), column=1,
         row=4, sticky = "e")

  tkgrid(meanBox, column=2, row=3, pady=5, padx=5)
  tkgrid(sdBox, column=2, row=4, pady=5, padx=5)

  ## Range prior note

  putAmelia("add.range.case",ttkcombobox(add.range.frame, values=cases,
                                         state="readonly", width=15))
  putAmelia("add.range.var",ttkcombobox(add.range.frame, values=vars,
                                        state="readonly", width=15))

  tkgrid(ttklabel(add.range.frame, text="Case:"), column=1, row=1, sticky = "e")
  tkgrid(ttklabel(add.range.frame, text="Variable:"), column=1, row=2, sticky = "e")
  tcl(getAmelia("add.range.case"), "current", 0)
  tcl(getAmelia("add.range.var"), "current", 0)
  tkconfigure(getAmelia("add.range.var"), postcommand=function(...) setMissingVars())
  tkgrid(getAmelia("add.range.case"), column=2, row=1, pady=3)
  tkgrid(getAmelia("add.range.var"),  column=2, row=2, pady=3)

  tkgrid(ttkframe(add.range.frame, width = 150, height = 0), column = 1, row = 0)
  putAmelia("priorMax", tclVar())
  putAmelia("priorMin", tclVar())
  putAmelia("priorConf", tclVar())


  putAmelia("minBox", ttkentry(add.range.frame, textvar=getAmelia("priorMin"), validate="key",
                               validatecommand = function(P) validateNumeric(P)))

  putAmelia("maxBox", ttkentry(add.range.frame, textvar=getAmelia("priorMax"), validate="key",
                               validatecommand = function(P) validateNumeric(P)))


  putAmelia("confBox", ttkentry(add.range.frame, textvar=getAmelia("priorConf"), validate="key",
                                validatecommand = function(P) validateNumeric(P)))

  tkgrid(ttklabel(add.range.frame, text="Minimum:"), column=1, row=3, sticky = "e")
  tkgrid(ttklabel(add.range.frame, text="Maximum:"), column=1, row=4, sticky = "e")
  tkgrid(ttklabel(add.range.frame, text="Confidence:"), column=1, row=5, sticky = "e")
                                        #tkgrid(tkframe(add.range.frame, width = 20, height = 0), column = 1, row = 6)

  tkgrid(minBox, column=2, row=3, pady=5, padx=5)
  tkgrid(maxBox, column=2, row=4, pady=5, padx=5)
  tkgrid(confBox, column=2, row=5, pady=5, padx=5)

  tkadd(getAmelia("addpri.note"), add.dist.frame, text = "Add Distribution Prior")
  tkadd(getAmelia("addpri.note"), add.range.frame, text = "Add Range Prior")


  tkgrid(getAmelia("addpri.note"), row = 1, sticky = "nsew")

  tkgrid(prior.add.but, sticky = "se", padx = 10, pady = 10)
  but.frame <- ttkframe(priorsBox)
  putAmelia("pri.ok", ttkbutton(but.frame, text = "OK", command = function(){tkwm.withdraw(priorsWindow);tkgrab.release(priorsWindow);tkfocus(getAmelia("gui"))}, width = 10))
  putAmelia("pri.can", ttkbutton(but.frame, text = "Cancel", width = 10, command = function() {cancelPriors();tkwm.withdraw(priorsWindow);tkgrab.release(priorsWindow);tkfocus(getAmelia("gui"))}))


  tkgrid(getAmelia("priors.tree"), row = 1, column = 1, sticky = "nsew")
  tkgrid(yscr, row = 1, column = 2, sticky = "nsew")
  tkgrid(xscr, row = 2, column = 1, sticky = "nsew")
  tkgrid.rowconfigure(prior.disp, 1, weight = 1)
  tkgrid.columnconfigure(prior.disp, 1, weight = 1)
  tkadd(prior.frame, prior.add)
  tkadd(prior.frame, prior.disp)

  tkgrid(prior.frame, row = 1, column = 0, columnspan = 2, padx = 10, pady = 10, sticky = "news")
  tkgrid(pri.ok, row = 0, column = 1, sticky = "ne", padx = 10, pady = 10)
  tkgrid(pri.can, row = 0, column = 2, sticky = "ne", padx = 10, pady = 10)
  tkgrid(but.frame, row = 2, column = 1, sticky = "ne")
  tkgrid.rowconfigure(priorsBox, 1, weight = 1)
  tkgrid.columnconfigure(priorsBox, 0, weight = 1)
  tkgrid.columnconfigure(priorsBox, 1, weight = 1)
  tkgrid(priorsBox, row = 0, column = 0, sticky = "news")
  tkgrid.rowconfigure(priorsWindow, 0, weight = 1)
  tkgrid.columnconfigure(priorsWindow, 0, weight = 1)

  tkwm.protocol(priorsWindow, "WM_DELETE_WINDOW", function() {tkwm.withdraw(priorsWindow);tkgrab.release(priorsWindow);tkfocus(getAmelia("gui"))})

  centerModalDialog(priorsWindow, resize = TRUE)

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

  if (exists("diagWindow", envir = ameliaEnv())) {
    tkwm.deiconify(getAmelia("diagWindow"))
    tkraise(getAmelia("diagWindow"))
    tkfocus(getAmelia("diagWindow"))
    return()
  }
  putAmelia("diagWindow", tktoplevel())
  tkwm.title(diagWindow, "Diagnostics")

  diagBox <- ttkframe(diagWindow)
  gui.top<-ttkpanedwindow(diagBox, orient = "vertical")
  var.diags <- ttklabelframe(gui.top, text = "Individual Variable Plots", width = 100, height = 100)
  tscs.diags <- ttklabelframe(gui.top, text = "Time-Series Cross-Sectional Plots", width = 100, height = 100)
  disp.diags <- ttklabelframe(gui.top, text = "Overdispersion Plots", width = 100, height = 100)

  tcl("set","indvar","")

  ## get variable names that are actually numeric
  variables <- getAmelia("varnames")
  variables <- variables[sapply(getAmelia("amelia.data"), is.numeric)]

  putAmelia("var.diags.combo", ttkcombobox(var.diags,textvariable="indvar",
                                           values = variables, state = "readonly"))
  indvar.lab <- ttklabel(var.diags, text = "Variable:")
  var.button.frame <- ttkframe(var.diags)
  putAmelia("diag.but.compare",ttkbutton(var.button.frame, text="Compare",
                                         command = function() compare.density(getAmelia("ameliaObject"),
                                           var=tclvalue("indvar"),frontend=TRUE)))

  putAmelia("diag.overimp",ttkbutton(var.button.frame,text="Overimpute",state="normal",
                                     command = function() overimpute(getAmelia("ameliaObject"),
                                       var=tclvalue("indvar"),frontend=TRUE)))
  tcl(getAmelia("var.diags.combo"), "current", 0)
  tkgrid(indvar.lab, row = 0, column = 0, padx = 5)
  tkgrid(getAmelia("var.diags.combo"), row = 0, column = 1, padx = 10, pady = 10)
  tkgrid(getAmelia("diag.but.compare"), row = 0, column = 0, padx = 10, pady = 10)
  tkgrid(getAmelia("diag.overimp"), row = 0, column = 1, padx = 10, pady = 10)
  tkgrid(var.button.frame, row =0, column = 2)
  tkgrid(ttkframe(var.diags, width = 50, height = 0), row = 1)


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
  tcl("set", "casename","")
  tcl("set", "tscsvarname", "")
  putAmelia("tscs.case.combo",
            ttkcombobox(tscs.diags,textvariable="casename", values = cases,
                        state = st))
  putAmelia("tscs.var.combo",
            ttkcombobox(tscs.diags,textvariable="tscsvarname",
                        values = tscsvariables, state = st))
  putAmelia("tscs.plot.but",
            ttkbutton(tscs.diags, text = "TSCS Plot", state = but.st,
                      command = function() tscsPlot(getAmelia("ameliaObject"),
                        cs = tclvalue("casename"),
                        var = tclvalue("tscsvarname"),
                        frontend = TRUE)))
  if (st == "readonly") {
    tcl(getAmelia("tscs.case.combo"), "current", 0)
    tcl(getAmelia("tscs.var.combo"), "current", 0)
  }
  tkgrid(ttklabel(tscs.diags, text = "Case:"), row = 0, column = 0, sticky = "e", padx = 5)
  tkgrid(getAmelia("tscs.case.combo"), row = 0, column = 1, padx = 10, pady = 10)
  tkgrid(ttklabel(tscs.diags, text = "Variable:"), row = 1, column = 0, sticky = "e", padx = 5)
  tkgrid(getAmelia("tscs.var.combo"), row = 1, column = 1, padx = 10, pady = 10)
  tkgrid(getAmelia("tscs.plot.but"), row = 1, column = 2, padx = 10, pady = 10, sticky = "se")
  tkgrid(ttkframe(tscs.diags, width = 50, height = 0), row = 2)

  dimvalue<-tclVar("1")
  putAmelia("onedim", ttkradiobutton(disp.diags, variable=dimvalue, value="1"))
  putAmelia("twodims", ttkradiobutton(disp.diags, variable=dimvalue, value="2"))
  disp.imps.tcl<-tclVar("5")
  putAmelia("disp.imps", ttkentry(disp.diags,width="5",textvariable=disp.imps.tcl))
  putAmelia("disp.but", ttkbutton(disp.diags,text="Overdisperse",state="normal",
                                  command = function() disperse(m=as.numeric(tclvalue(disp.imps.tcl)),
                                    dims=as.numeric(tclvalue(dimvalue)),frontend=TRUE,output=getAmelia("ameliaObject"))))
  tkgrid(ttklabel(disp.diags,text="Number of dispersions:"),row=2,column=1,
         sticky="e")
  tkgrid(ttkframe(disp.diags, width = 50, height = 0), row = 5)
  tkgrid(disp.imps,column=2,row=2,sticky="nw", padx = 10, pady = 10)
  tkgrid(ttklabel(disp.diags,text="One Dimension:"),row=3,column=1, sticky = "e")
  tkgrid(ttklabel(disp.diags,text="Two Dimensions:"),row=4,column=1, sticky = "e")
  tkgrid(onedim,row=3,column=2,padx=10,pady=5)
  tkgrid(twodims,row=4,column=2,padx=10)
  tkgrid(disp.but,row=4,column=3,padx=15, pady=10,sticky="news")

  tkadd(gui.top, var.diags)
  tkadd(gui.top, tscs.diags)
  tkadd(gui.top, disp.diags)
  tkgrid(gui.top, row = 0, padx = 20, pady = 20)
  tkgrid(diagBox, sticky = "news", row = 0, column = 0)
  tkgrid.rowconfigure(diagWindow, 0, weight = 1)
  tkgrid.columnconfigure(diagWindow, 0, weight = 1)

  tkwm.protocol(diagWindow, "WM_DELETE_WINDOW", function() {tkwm.withdraw(diagWindow);tkgrab.release(diagWindow);tkfocus(getAmelia("gui"))})
  centerModalDialog(diagWindow, resize = FALSE)

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
ameliaEnv <- function() {
  pos <-  match("ameliaEnv", search())
  if (is.na(pos)) { # Must create it
    ameliaEnv <- list()
    attach(ameliaEnv, pos = length(search()) - 1)
    rm(ameliaEnv)
    pos <- match("ameliaEnv", search())
  }
  return(pos.to.env(pos))
}

putAmelia <- function(x, value) {
  assign(x, value, envir = ameliaEnv())
}

getAmelia <- function(x, mode="any")
  get(x, envir = ameliaEnv(), mode = mode, inherits = FALSE)
getAmeliaInd <- function(x) {
  as.numeric(tkindex(getAmelia(x), "current"))
}
ameliaTclSet <- function(name, value){
  name <- ls(unclass(getAmelia(name))$env)
  tcl("set", name, value)
}

save.log <- function() {
  file.select <- tclvalue(tkgetSaveFile(parent=getAmelia("gui"), filetypes="{{Text files} {*.txt}} {{All files} *}"))
  cat(getAmelia("output.log"), file = file.select)
}

show.output.log <- function() {

  RightClick <- function(x, y) { # x and y are the mouse coordinates
    rootx <- as.integer(tkwinfo("rootx", getAmelia("log.viewer")))  # tkwinfo() return several infos
    rooty <- as.integer(tkwinfo("rooty", getAmelia("log.viewer")))
    xTxt <- as.integer(x) + rootx
    yTxt <- as.integer(y) + rooty
                                        # Create a Tcl command in a character string and run it
    .Tcl(paste("tk_popup", .Tcl.args(getAmelia("log.right.click"), xTxt, yTxt)))
  }


  if (exists("log.top", envir = ameliaEnv())) {
    tkdelete(getAmelia("log.viewer"), 0, "end")
    tkinsert(getAmelia("log.viewer"), "end",
             paste(getAmelia("output.log"), collapse = ""))
    tkwm.deiconify(getAmelia("log.top"))
    tkraise(getAmelia("log.top"))
    tkfocus(getAmelia("log.top"))
    return()
  }
  putAmelia("log.top", tktoplevel())
  tkwm.title(log.top, "Output Log")
  scr <- ttkscrollbar(log.top,
                      command=function(...)tkyview(getAmelia("log.viewer"),...))

  putAmelia("log.viewer", tktext(log.top, width = 80, height = 25,
                                 yscrollcommand=function(...)tkset(scr,...)))
  tkinsert(getAmelia("log.viewer"), "end", paste(getAmelia("output.log"), collapse = ""))
  tkconfigure(getAmelia("log.viewer"), state = "disabled")
  main.menu      <- tkmenu(log.top)
  main.menu.file <- tkmenu(main.menu, tearoff=0)
  tkadd(main.menu.file,"command",label="Save log file",command=function() save.log())
  tkadd(main.menu.file,"command",label="Close",command=function(){tkwm.withdraw(log.top);tkgrab.release(log.top);tkfocus(getAmelia("gui"))})

  tkadd(main.menu,"cascade",label="File",menu=main.menu.file)
  tkconfigure(log.top,menu=main.menu)


  putAmelia("log.right.click",tkmenu(getAmelia("log.viewer"), tearoff = FALSE) )
  tkadd(getAmelia("log.right.click"), "command", label = "Copy <Ctrl-V>",
        command = function() tkevent.generate(getAmelia("log.viewer"),"<<Copy>>"))
  tkbind(getAmelia("log.viewer"), "<Button-3>", RightClick)

                                        #tkgrid(main.menu, row = 0, sticky = "ew")
  tkgrid(getAmelia("log.viewer"), row = 0, column = 0, sticky = "news")
  tkgrid(scr, row =0, column = 1, sticky = "ns")
                                        #tkgrid.columnconfigure(log.top, 1, weight = 1)
  tkgrid.columnconfigure(log.top, 0, weight = 1)
  tkgrid.rowconfigure(log.top, 0, weight = 1)
  tkwm.protocol(log.top, "WM_DELETE_WINDOW", function() {tkwm.withdraw(log.top);tkgrab.release(log.top);tkfocus(getAmelia("gui"))})
  centerModalDialog(log.top, resize=TRUE)


}

after <- function(ms, func) {
  .Tcl(paste("after", ms, .Tcl.callback(func)))
}

cancel.after <- function(id) {
  invisible(.Tcl(paste("after","cancel", id)))
}

bindTooltip <- function(widget, tip) {
  after.name <- paste(widget, "after", sep = ".")
  tip.name <- paste(widget, "tip", sep = ".")
                                        #  tkbind(getAmelia(widget), "<Any-Enter>", showTooltip(widget, tip))
  tkbind(getAmelia(widget), "<Any-Enter>", function() putAmelia(after.name, after(400, showTooltip(widget, tip))))
  tkbind(getAmelia(widget), "<Any-Leave>", function() {killTooltip(widget)
                                                       cancel.after(getAmelia(after.name))})
  tkbind(getAmelia(widget), "<Any-Button>", function() cancel.after(getAmelia(after.name)))
  tkbind(getAmelia(widget), "<Any-KeyPress>", function() cancel.after(getAmelia(after.name)))

}



showTooltip <- function(widget, text) {

  function() {
    if (getAmelia(widget)$ID != tclvalue(tkwinfo("containing", tkwinfo("pointerx","."),
                           tkwinfo("pointery",".")))) {
      return()
    }
    tip.name <- paste(widget, "tip", sep = ".")
    tiplabel.name <- paste(widget, "tiplabel",sep=".")


    if (exists(tip.name, envir = ameliaEnv())) {
      if (as.logical(tkwinfo("exists",getAmelia(tip.name)))) {
        if (as.logical(tkwinfo("ismapped",getAmelia(tip.name)))) {
          return()
        }
      }
    }


    scrh <- tclvalue(tkwinfo("screenheight", getAmelia(widget)))
    scrw <- tclvalue(tkwinfo("screenwidth", getAmelia(widget)))


    tclServiceMode(on=FALSE)
    if (!exists(tip.name, envir = ameliaEnv())) {
      if (.Platform$OS.type == "windows") {
        borderColor <- "SystemWindowFrame"
        bgColor <- "SystemWindow"
        fgColor <- "SystemWindowText"
      }  else {
        borderColor <- "black"
        bgColor <- "lightyellow"
        fgColor <- "black"
      }
      putAmelia(tip.name, tktoplevel(getAmelia(widget), bd = 1, bg = borderColor, relief = "raised"))
      tkwm.geometry(getAmelia(tip.name), paste("+",scrh,"+",scrw,sep=""))
      tcl("wm","overrideredirect", getAmelia(tip.name), 1)

      putAmelia(tiplabel.name, ttklabel(getAmelia(tip.name), background = bgColor,
                                        foreground = fgColor, text = text, justify = "left",
                                        wraplength=300))
      tkpack(getAmelia(tiplabel.name))

      tkbind(getAmelia(tip.name), "<Any-Enter>",
             function() tkwm.withdraw(getAmelia(tip.name)))
      tkbind(getAmelia(tip.name), "<Any-Leave>",
             function() tkwm.withdraw(getAmelia(tip.name)))
      tkbind(getAmelia(tip.name), "<Any-Button>",
             function() tkwm.withdraw(getAmelia(tip.name)))
    }

    width  <- as.numeric(tclvalue(tkwinfo("reqwidth", getAmelia(tiplabel.name))))
    height <- as.numeric(tclvalue(tkwinfo("reqheight",getAmelia(tiplabel.name))))

    posX <- as.numeric(tclvalue(tkwinfo("pointerx",".")))
    posY <- as.numeric(tclvalue(tkwinfo("pointery","."))) + 25
    screen <- as.numeric(tclvalue(tkwinfo("screenwidth",".")))

                                        # a.) Ad-hockery: Set positionX so the entire tooltip widget will be displayed.
    if  ((posX + width) > screen) {
      posX <-  posX - ((posX + width) - screen) - 3
    }
    tclServiceMode(on = TRUE)
    tkwm.geometry(getAmelia(tip.name),
                  paste("+",posX,"+",posY,sep = ""))

    tkwm.deiconify(getAmelia(tip.name))
    tkraise(getAmelia(tip.name))



  }
}

killTooltip <- function(widget) {
  tip.name <- paste(widget,"tip", sep = ".")
  if (exists(tip.name, envir = ameliaEnv())) {
    tkwm.withdraw(getAmelia(tip.name))
  }
}


refreshSelection <- function(tree) {
  all <- strsplit(tclvalue(tcl(tree,"children","")), " ")[[1]]
  sel <- strsplit(tclvalue(tcl(tree, "selection")), " ")[[1]]
  bandTree()
  for (i in sel) {
    tcl(tree, "item", i, tags = "selected")
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
  sel <- strsplit(tclvalue(tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  states <- variableOptionStatus(sel)
  for (i in 0:14) {
    if (tclvalue(tktype(getAmelia("main.menu.variables"), i)) != "separator")
      tkentryconfigure(getAmelia("main.menu.variables"),i, state = states[i+1])
  }
  return(NULL)
}

mainTreeRightClick <- function(x, y) { # x and y are the mouse coordinates
  rootx <- as.integer(tkwinfo("rootx", getAmelia("main.tree")))  # tkwinfo() return several infos
  rooty <- as.integer(tkwinfo("rooty", getAmelia("main.tree")))
  xTxt <- as.integer(x) + rootx
  yTxt <- as.integer(y) + rooty
                                        # Create a Tcl command in a character string and run it
  sel <- strsplit(tclvalue(tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  states <- variableOptionStatus(sel)
  main.tree.right.click <- tkmenu(main.tree, tearoff = FALSE)
  main.tree.trans <- tkmenu(main.tree, tearoff = FALSE)
  tkadd(main.tree.right.click, "command", label = "Set as Time-Series Variable", command = setTS, state = states[1])
  tkadd(main.tree.right.click, "command", label = "Set as Cross-Section Variable", command = setCS, state = states[2])
  tkadd(main.tree.right.click, "command", label = "Unset as Time-Series Variable", command = unsetTS, state = states[3])
  tkadd(main.tree.right.click, "command", label = "Unset as Cross-Section Variable", command = unsetCS, state = states[4])
  tkadd(main.tree.right.click,"separator")
  tkadd(main.tree.right.click, "command", label = "Add Lag", command = function() addLag(), state = states[6])
  tkadd(main.tree.right.click, "command", label = "Add Lead", command = function() addLead(), state = states[7])
  tkadd(main.tree.right.click, "command", label = "Remove Lag", command = function() dropLag(), state = states[8])
  tkadd(main.tree.right.click, "command", label = "Remove Lead", command = function() dropLead(), state = states[9])
  tkadd(main.tree.right.click,"separator")

  tkadd(main.tree.right.click, "command", label =
        "Plot Histogram(s) of Selected", command = plotHist, state = states[10])
  if (.Platform$OS.type == "windows") {
    tkadd(main.tree.trans, "command", label = "Log", command = function(x) setTrans("logs"))
    tkadd(main.tree.trans, "command", label = "Square Root", command = function(x) setTrans("sqrt"))
    tkadd(main.tree.trans, "command", label = "Logistic", command = function(x) setTrans("lgstc"))
    tkadd(main.tree.trans, "command", label = "Nominal", command = function(x) setTrans("noms"))
    tkadd(main.tree.trans, "command", label = "Ordinal", command = function(x) setTrans("ords"))
    tkadd(main.tree.trans, "command", label = "ID Variable", command = function(x) setTrans("idvar"))
    tkadd(main.tree.right.click, "cascade", label = "Add Transformation...", menu = main.tree.trans, state = states[12])
  } else {
    tkadd(main.tree.right.click, "command", label = "Mark as Log", command = function(x) setTrans("logs"), state = states[12])
    tkadd(main.tree.right.click, "command", label = "Mark as Square Root", command = function(x) setTrans("sqrt"), state = states[12])
    tkadd(main.tree.right.click, "command", label = "Mark as Logistic", command = function(x) setTrans("lgstc"), state = states[12])
    tkadd(main.tree.right.click, "command", label = "Mark as Nominal", command = function(x) setTrans("noms"), state = states[12])
    tkadd(main.tree.right.click, "command", label = "Mark as Ordinal", command = function(x) setTrans("ords"), state = states[12])
    tkadd(main.tree.right.click, "command", label = "Mark as ID Variable", command = function(x) setTrans("idvar"), state = states[12])
  }
  tkadd(main.tree.right.click, "command", label =
        "Remove Transformations", command = dropTrans, state = states[13])
  tkadd(main.tree.right.click,"separator")
  tkadd(main.tree.right.click, "command", label =
        "Add or Edit Bounds", command = addBounds, state = states[15])
  tkpopup(main.tree.right.click, xTxt, yTxt)
}

addLag <- function() {
  sel <- strsplit(tclvalue(tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  tmp <- getAmelia("lags")
  tmp[sel] <- 1
  putAmelia("lags", tmp)
  for (i in sel)
    tkset(getAmelia("main.tree"), i, "lag", "X")
  return()
}
addLead <- function() {
  sel <- strsplit(tclvalue(tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  tmp <- getAmelia("leads")
  tmp[sel] <- 1
  putAmelia("leads", tmp)
  for (i in sel)
    tkset(getAmelia("main.tree"), i, "lead", "X")
  return()
}
dropLag <- function() {
  sel <- strsplit(tclvalue(tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  tmp <- getAmelia("lags")
  tmp[sel] <- 0
  putAmelia("lags", tmp)
  for (i in sel)
    tkset(getAmelia("main.tree"), i, "lag", "")
  return()
}
dropLead <- function() {
  sel <- strsplit(tclvalue(tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  tmp <- getAmelia("leads")
  tmp[sel] <- 0
  putAmelia("leads", tmp)
  for (i in sel)
    tkset(getAmelia("main.tree"), i, "lead", "")
  return()
}

setTrans <- function(trans) {
  all.trans <- c(logs = "Log",sqrt = "Square Root",
                 lgstc = "Logistic", noms = "Nominal", ords = "Ordinal", idvar = "ID")
  sel <- strsplit(tclvalue(tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  tmp <- getAmelia(trans)
  tmp[sel] <- 1
  putAmelia(trans, tmp)
  for (j in sel) {
    tkset(getAmelia("main.tree"), j,"transform", all.trans[trans])
    tcl(getAmelia("main.tree"), "item", j, image = "")
  }
  return()
}

dropTrans <- function() {
  all.trans <- c("logs","sqrt","lgstc","noms","ords","idvar")
  sel <- strsplit(tclvalue(tcl(getAmelia("main.tree"), "selection")), " ")[[1]]
  for (j in sel)
    tkset(getAmelia("main.tree"), j,"transform", "")
  if (is.factor(getAmelia("amelia.data")[,j]) |
      is.character(getAmelia("amelia.data")[,j])) {
    tcl(getAmelia("main.tree"), "item", j, image = redFlagIcon)
  }

  for (i in all.trans) {
    tmp <- getAmelia(i)
    tmp[sel] <- 0
    putAmelia(i, tmp)
  }
}

addBounds <- function() {
  onOK <- function(sel) {

    bdMax <- as.numeric(tclvalue(getAmelia("boundMax")))
    bdMin <- as.numeric(tclvalue(getAmelia("boundMin")))

    if (is.na(bdMax) & !is.na(bdMin)) {
      tkmessageBox(parent=getAmelia("addBoundsWindow"), title="Error",
                   message="Please enter a minimum and a maximum value or neither to clear the bounds.",
                   type="ok",icon="error")
      return()
    }
    if  (!is.na(bdMax) & is.na(bdMin)) {
      tkmessageBox(parent=getAmelia("addBoundsWindow"), title="Error",
                   message="Please enter a minimum and a maximum value or neither to clear the bounds.",
                   type="ok",icon="error")
      return()
    }

    if (!is.na(bdMax) & !is.na(bdMin)) {
      if (bdMax <= bdMin) {
        tkmessageBox(parent=getAmelia("addBoundsWindow"), title="Error",
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
    tkset(getAmelia("main.tree"), sel, "bounds", treeBounds)
    tkwm.withdraw(addBoundsWindow)
    tkgrab.release(addBoundsWindow)
    tkfocus(getAmelia("gui"))
    return()
  }
  validateNumeric <- function(x) {
    if (isTRUE(grep("(^-?[0-9]*\\.?[0-9]*$)",x)==1))
      return(tclVar("TRUE"))
    else
      return(tclVar("FALSE"))
  }


  sel <- strsplit(tclvalue(tcl(getAmelia("main.tree"), "selection")),
                  " ")[[1]]
  if (sum(is.na(getAmelia("amelia.data")[,sel])) == 0) {
    tkmessageBox(parent = getAmelia("gui"), message =
                 "No missing data on the selected variable.", type = "ok")
    return()

  }
  currMin <- getAmelia("boundsmat")[sel,2]
  currMax <- getAmelia("boundsmat")[sel,3]

  putAmelia("boundMin", tclVar(ifelse(is.na(currMin), "",
                                      currMin)))
  putAmelia("boundMax", tclVar(ifelse(is.na(currMax), "",
                                      currMax)))

  if (exists("addBoundsWindow", envir = ameliaEnv())) {
    tkconfigure(getAmelia("maxBox"), textvar = getAmelia("boundMax"))
    tkconfigure(getAmelia("minBox"), textvar = getAmelia("boundMin"))
    tkconfigure(getAmelia("bd.ok"), command = function() onOK(sel))
    tkwm.deiconify(getAmelia("addBoundsWindow"))
    tkraise(getAmelia("addBoundsWindow"))
    return()
  }
  putAmelia("addBoundsWindow", tktoplevel())
  tkwm.title(addBoundsWindow, "Add or Edit Bounds")
  bounds.add   <- ttkframe(addBoundsWindow)


  putAmelia("minBox", ttkentry(bounds.add, textvar=getAmelia("boundMin"), validate="key",
                               validatecommand = function(P) validateNumeric(P)))

  putAmelia("maxBox", ttkentry(bounds.add, textvar=getAmelia("boundMax"), validate="key",
                               validatecommand = function(P) validateNumeric(P)))

  tkgrid(ttklabel(bounds.add, text="Minimum:"), column=1, row=2,
         sticky = "e", padx = 10, pady = 10)
  tkgrid(ttklabel(bounds.add, text="Maximum:"), column=1, row=3,
         sticky = "e", padx = 10, pady = 10)

  tkgrid(minBox, column=2, row=2, pady=5, padx=5)
  tkgrid(maxBox, column=2, row=3, pady=5, padx=5)
  but.frame <- ttkframe(bounds.add)
  putAmelia("bd.ok", ttkbutton(but.frame, text = "OK", command = function() onOK(sel)))
  putAmelia("bd.can", ttkbutton(but.frame, text = "Cancel", width =
                                10, command = function() {tkwm.withdraw(addBoundsWindow);tkgrab.release(addBoundsWindow);tkfocus(getAmelia("gui"))}))
  tkgrid(bd.ok, row = 0, column = 1, sticky = "ne", padx = 10, pady = 10)
  tkgrid(bd.can, row = 0, column = 2, sticky = "ne", padx = 10, pady = 10)
  tkgrid(but.frame, row = 4, column = 1, columnspan = 2, sticky =
         "ne")
  tkgrid(bounds.add, sticky = "news")

  tkwm.protocol(addBoundsWindow, "WM_DELETE_WINDOW", function() {tkwm.withdraw(addBoundsWindow);tkgrab.release(addBoundsWindow);tkfocus(getAmelia("gui"))})

  centerModalDialog(addBoundsWindow, resize=FALSE)
}


plotHist <- function() {
  sel <- strsplit(tclvalue(tcl(getAmelia("main.tree"), "selection")),
                  " ")[[1]]
  if (length(sel)==0) {
    tkmessageBox(parent = getAmelia("gui"), type = "ok", message =
                 "No variable selected.")
    return(NULL)
  }
  sel <- sel[which(sapply(getAmelia("amelia.data")[sel], is.numeric))]
  if (length(sel)==0) {
    tkmessageBox(parent = getAmelia("gui"), type = "ok", message =
                 "Cannot plot non-numeric variables.")
    return(NULL)
  }
  if (.Platform$OS.type == "windows")
    windows()
  else
    x11()

  mfrow <- set.mfrow(nvars = length(sel))
  on.exit(par(NULL))
  layout <- par(mfrow = mfrow)
  j <- 0
  for (i in sel) {
    j <- j + 1
    if (j > 9) {
      j <- 1
      if (.Platform$OS.type == "windows")
        windows()
      else
        x11()
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
  children <- strsplit(tclvalue(tcl(getAmelia("main.tree"),
                                    "children","")), " ")[[1]]

  if (col == "#0") {
    coldata <- children
  } else {
    for (i in children) {
      coldata <- c(coldata, tclvalue(tkset(getAmelia("main.tree"), i, col)))
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
    tkmove(getAmelia("main.tree"), sorted[i],"", i-1)
  }
  drawArrow(col, sortDir)
  refreshSelection(getAmelia("main.tree"))
  dirs[col] <- !sortDir
  putAmelia("sortDirs", dirs)
}

drawArrow <- function(col, down) {
  treecols <- names(getAmelia("sortDirs"))
  for (i in treecols) {
    tcl(getAmelia("main.tree"), "heading", i, image = "")
  }
  if (down) {
    tcl(getAmelia("main.tree"), "heading", col,
        image = upArrowIcon)
  } else {
    tcl(getAmelia("main.tree"), "heading", col,
        image = downArrowIcon)
  }
  return(NULL)
}

bandTree <- function() {
  children <- strsplit(tclvalue(tcl(getAmelia("main.tree"),
                                    "children","")), " ")[[1]]
  j <- 0
  tktag.configure(getAmelia("main.tree"),"white", background="white")
  tktag.configure(getAmelia("main.tree"),"gray", background="gray92")
  for (i in children) {
    j <- j+1
    if ((j %% 2) == 0) {
      tcl(getAmelia("main.tree"), "item", i, tag = "white")
    } else {
      tcl(getAmelia("main.tree"), "item", i, tag = "gray")
    }
  }
}

updateTreeStats <- function(){
  children <- strsplit(tclvalue(tcl(getAmelia("main.tree"),"children","")), " ")[[1]]
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
    tkset(getAmelia("main.tree"), i, "min", vals[1])
    tkset(getAmelia("main.tree"), i, "max", vals[2])
    tkset(getAmelia("main.tree"), i, "mean", vals[3])
    tkset(getAmelia("main.tree"), i, "sd", vals[4])
    tkset(getAmelia("main.tree"), i, "miss", vals[5])
  }


}

centerModalDialog <- function(window, resize=TRUE) {
  xpos <- as.numeric(tkwinfo("rootx",getAmelia("gui")))
  ypos <- as.numeric(tkwinfo("rootx",getAmelia("gui")))
  rwidth <- as.numeric(tkwinfo("width",getAmelia("gui")))
  rheight <- as.numeric(tkwinfo("height", getAmelia("gui")))
  width <- as.numeric(tkwinfo("reqwidth",window))
  height <- as.numeric(tkwinfo("reqheight",window))
  newxpos <- xpos + .5*rwidth - .5*width
  newypos <- ypos + .5*rheight - .5*height
  if (.Platform$OS.type == "windows")
    tkwm.geometry(window, paste("+",round(newxpos),"+",round(newypos),sep=""))
  tkfocus(window)
  tkgrab.set(window)
  if (!resize) {
    tkwm.resizable(window, 0,0)
  }
  tkwm.transient(window, getAmelia("gui"))
  tcl("update","idletasks")
}


showImputedFiles <- function() {
  if (Sys.info()['sysname'] %in% c("Windows", "Darwin"))
    system(paste("open", shQuote(getAmelia("wdForLastImputation"))))
  else
    system(paste("xdg-open", shQuote(getAmelia("wdForLastImputation"))))
  return(NULL)
}

if (getRversion() >= "2.15.1") {
  globalVariables(c('gui', 'main.menu', 'main.menu.file',
                    'main.menu.import', 'main.menu.demo',
                    'main.menu.variables','main.menu.trans',
                    'main.menu.splines', 'main.menu.options',
                    'main.menu.outfile', 'main.menu.output',
                    'main.menu.help', 'gui.welcome', 'rPic',
                    'tablePic', 'dtaPic', 'spssPic', 'gui.skel',
                    'pageUpIcon', 'saveIcon', 'histIcon',
                    'pageEditIcon', 'worldIcon', 'action.go.icon',
                    'pageTextIcon', 'loadSessionButton',
                    'saveSessionButton', 'plotHistButton',
                    'editDataButton', 'missmapButton', 'output.run',
                    'showLogButton', 'legendFrame', 'clockIcon',
                    'userIcon', 'redFlagIcon', 'main.tree',
                    'statusbar', 'redStopIcon', 'greenCheckIcon',
                    'noimps.label', 'addBoundsWindow', 'minBox',
                    'maxBox', 'bd.ok', 'bd.can', 'aboutWindow',
                    'temp.seed', 'temp.tol', 'numericalWindow',
                    'temp.name', 'temp.num', 'outputWindow',
                    'upArrowIcon', 'downArrowIcon', 'diagWindow',
                    'disp.imps', 'onedim', 'twodims', 'disp.but',
                    'priorsWindow', 'meanBox', 'sdBox', 'confBox',
                    'prior.add.but', 'pri.ok', 'pri.can', 'clockIcon',
                    'varnames', 'userIcon', 'amelia.data',
                    'main.tree', 'log.top', 'sum.plot'))
}
