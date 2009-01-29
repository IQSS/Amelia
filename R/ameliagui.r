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

AmeliaView<-function() {

main.close<-function() {
	qvalue<-tkmessageBox(message="Are you sure you want to exit Amelia?",
		icon="question",
		type="okcancel",
		default="cancel")
	if (tclvalue(qvalue)=="ok") {
		tkdestroy(gui)
                detach("ameliaEnv")
	}
}


file.type<-function(...) {
  putAmelia("drop.select",as.numeric(tkcurselection(input.drop.box)))
  
  filetype<-c("{{Comma-delimited files} {.csv}} {{All files} *} ",
              "{{Tab Delimited} {.txt}} {{All files} *}",
              "{{Stata files} {.dta}} {{All files} *}",
              "{{SPSS} {.sav}} {{All files} *}",
              "{{All files} *}",
              "{{RData} {.RData}} {{All files} *}")
  
  putAmelia("file.kind",filetype[getAmelia("drop.select")+1])
}

get.filename<-function(entry) {
  putAmelia("am.filename", tclvalue(tkgetOpenFile(filetypes=getAmelia("file.kind"))))
  tkdelete(input.entry,0,"end")
  tkinsert(input.entry,"end",getAmelia("am.filename"))
}

load.data <- function(session=FALSE) {
  if (!session) {
    if (!is.null(getAmelia("amelia.data"))) {
      sure<-tkmessageBox(message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")    
      if (tclvalue(sure) == "no") 
        return(NULL)
    }
  }
  
  if (is.null(getAmelia("am.filename")))
    putAmelia("am.filename",tclvalue(getAmelia("inname")))
  if (!file.exists(getAmelia("am.filename"))) {
    tkmessageBox(message="The file doesn't exist.  Try Again.",icon="error",type="ok")
    return(NULL)
  }
  ds <- getAmelia("drop.select")
  if (ds == 0)
    putAmelia("amelia.data",try(read.csv(getAmelia("am.filename"),header=TRUE)))
  if (ds == 1)
    putAmelia("amelia.data",try(read.table(getAmelia("am.filename"),header=TRUE)))
  if (ds == 2)
    putAmelia("amelia.data",try(read.dta(getAmelia("am.filename"),convert.factors=FALSE)))
  if (ds == 3)
    putAmelia("amelia.data",try(read.spss(getAmelia("am.filename"),use.value.labels=FALSE,to.data.frame=TRUE)))
  if (ds == 4)
    putAmelia("amelia.data",try(read.xport(getAmelia("am.filename"))))
  if (ds == 5)
    putAmelia("amelia.data",try(load(getAmelia("am.filename"))))
  
  putAmelia("filetype.sess",ds)
  if (inherits(getAmelia("amelia.data"), "try-error")) {
    tkmessageBox(message="Failure in loading the data.  Try again.",icon="error",type="ok")
    return(NULL)
  }
  temp.list<- strsplit(getAmelia("am.filename"),"/")[[1]]
  putAmelia("am.directory",
            paste(temp.list[-length(temp.list)],"",sep="/",collapse=""))                         
  setwd(getAmelia("am.directory"))
  putAmelia("varnames" , names(getAmelia("amelia.data")))
  tcl("set","varnames",c("(none)",getAmelia("varnames")))
  tkconfigure(options.var, state = "normal")
  tkconfigure(options.tsvar, state = "normal",listvar="varnames")
  tkconfigure(options.csvar, state = "normal",listvar="varnames")
  tkconfigure(options.priors, state = "active")
  tkconfigure(statusbar.lab1b, text = getAmelia("am.filename"), foreground = "blue")
  tkconfigure(statusbar.n, text = paste(nrow(getAmelia("amelia.data"))), foreground = "blue")
  tkconfigure(statusbar.k, text = paste(ncol(getAmelia("amelia.data"))), foreground = "blue")
  tkconfigure(output.run, state = "active")
  tkconfigure(input.see, state = "active")
  putAmelia("tsvar",0)
  putAmelia("csvar",0)
  tkselect(options.tsvar,getAmelia("tsvar"))
  tkselect(options.csvar,getAmelia("csvar"))
  tkconfigure(options.tsvar,command=function(...)set.tsvar())
  tkconfigure(options.csvar,command=function(...)set.csvar())
  putAmelia("transmat",vector("numeric",ncol(getAmelia("amelia.data"))))
  putAmelia("amelia.lags",vector("numeric",ncol(getAmelia("amelia.data"))))
  putAmelia("future.lags",vector("numeric",ncol(getAmelia("amelia.data"))))
  putAmelia("num.poly",tclVar("0"))
  putAmelia("intercs",tclVar("0"))
#  putAmelia("store.pri",  NULL)
  putAmelia("obs.prior",  NULL)
  putAmelia("r",NULL)
  putAmelia("priorsmat",  NULL)
  putAmelia("outname",  tclVar("outdata"))
  putAmelia("outnum",  tclVar("5"))
  putAmelia("empri",  tclVar("0"))
  tkconfigure(options.tscs, state = "disabled")
  putAmelia("call.flag",NULL)
  putAmelia("varmin",NULL)
  putAmelia("varmax",NULL)
  putAmelia("tclvarmin",list())
  putAmelia("tclvarmax",list())
  tkconfigure(output.entry, textvariable=getAmelia("outname"))
  tkconfigure(output.num, textvariable=getAmelia("outnum"))
  #tkconfigure(input.load,state="disabled",text="Data Loaded")
  
}

set.tsvar<-function() {
  putAmelia("tsvar",as.numeric(tkcurselection(options.tsvar)))
  if (getAmelia("tsvar") != 0)
    tkconfigure(options.tscs,state="active")
  putAmelia("call.flag",NULL)
}

set.csvar<-function() {
  putAmelia("csvar",as.numeric(tkcurselection(options.csvar)))
  putAmelia("call.flag",NULL)
}


#Returns the data matrix without the rows with missing values
# x: data matrix
packr<-function(x) {
	cols<-ncol(x)
	r<-is.na(x)
	sumr<-apply(r,1,sum)
	x2<-x[sumr==0,]
	return(x2)
}


save.session <-function() {
  if (is.null(getAmelia("amelia.data"))) {
    tkmessageBox(message="You must load a dataset before you can save a session.", icon="error", type="ok")  
    return(NULL)
  }  
  file.select <- tclvalue(tkgetSaveFile(filetypes="{{RData files} {.RData}} {{All files} *}"))
#  if (exists("amelia.list")) {
#    amelia.list$amelia.args$am.filename <- am.filename
#    amelia.list$amelia.args$varmin <- varmin
#    amelia.list$amelia.args$varmax <- varmax
#    amelia.list$amelia.args$output.select <- output.select
#    dump("amelia.list", file.select)
#    return(NULL)
#  }
  nn <- ncol(getAmelia("amelia.data"))
  
  amelia.list<-list()
  outname1 <- tclvalue(getAmelia("outname"))
  outnum1 <- as.numeric(tclvalue(getAmelia("outnum")))
  empri1 <- as.numeric(tclvalue(getAmelia("empri")))
  amelia.list$amelia.args<-list()
  amelia.list$amelia.args$outname <- outname1
  amelia.list$amelia.args$m <- outnum1
  amelia.list$amelia.args$empri <- empri1
  amelia.list$amelia.args$ts <- getAmelia("tsvar")
  amelia.list$amelia.args$cs <- getAmelia("csvar")
  amelia.list$amelia.args$am.filename <- getAmelia("am.filename")
  amelia.list$amelia.args$file.type <- getAmelia("filetype.sess")
  amelia.list$amelia.args$idvars <- (1:nn)[getAmelia("transmat") == 6]
  amelia.list$amelia.args$ords <- (1:nn)[getAmelia("transmat") == 1]
  amelia.list$amelia.args$noms <- (1:nn)[getAmelia("transmat") == 2]
  amelia.list$amelia.args$logs <- (1:nn)[getAmelia("transmat") == 3]
  amelia.list$amelia.args$sqrts <- (1:nn)[getAmelia("transmat") == 4]
  amelia.list$amelia.args$lgstc <- (1:nn)[getAmelia("transmat") == 5]
  amelia.list$amelia.args$lags <- (1:nn)[getAmelia("amelia.lags") == 1]
  amelia.list$amelia.args$leads <-(1:nn)[getAmelia("future.lags") == 1]
 
#  amelia.list$amelia.args$casepri <- getAmelia("store.pri")
  amelia.list$amelia.args$polytime <- as.numeric(tclvalue(getAmelia("num.poly")))
  amelia.list$amelia.args$intercs <- as.numeric(tclvalue(getAmelia("intercs")))
  amelia.list$amelia.args$output.select <- getAmelia("output.select")
  amelia.list$amelia.args$priors <- getAmelia("priorsmat")
  amelia.list$amelia.args$seed <- as.numeric(tclvalue(getAmelia("seed")))
  amelia.list$amelia.args$tol  <- as.numeric(tclvalue(getAmelia("tol")))
  
  save("amelia.list", file=file.select)
  return(NULL)
}

load.session <- function() {

  ## diaglog to get RData file
  file.select <- tclvalue(tkgetOpenFile(filetypes=
          "{{RData files} {.RData}} {{All files} *}"))
	if (nchar(file.select) <= 0)
    return(NULL)

  ## try loading the RData file and stop if it doesn't work
  tryloadsess <- try(load(file=file.select, envir=ameliaEnv()), silent=TRUE)
  
  if (inherits(tryloadsess,"try-error")) {
    tkmessageBox(message="Error loading session.  This is not a valid session file.",icon="error",type="ok")
    return(NULL)
  }

  ## make sure that the RData file loaded the right list
  if (!("amelia.list" %in% ls(ameliaEnv()))) {
    tkmessageBox(message="Not an Amelia session file.  Try again.",icon="error",type="ok")
    return(NULL)
  }
  
  
  ## check that the data is where it's supposed to be.
  if (!file.exists(getAmelia("amelia.list")$amelia.args$am.filename)) {
    tkmessageBox(message=paste("Dataset file not found at:",getAmelia("amelia.list")$amelia.args$am.filename,"Cannot load session.",sep="\n"),icon="error",type="ok")
    return(NULL)
  }
  putAmelia("drop.select", getAmelia("amelia.list")$amelia.args$file.type)
  putAmelia("inname",      tclVar(getAmelia("amelia.list")$amelia.args$am.filename))
  putAmelia("am.filename", getAmelia("amelia.list")$amelia.args$am.filename)
  load.data(session=TRUE)
  putAmelia("tsvar", getAmelia("amelia.list")$amelia.args$ts)
  putAmelia("csvar", getAmelia("amelia.list")$amelia.args$cs)

  nn <- ncol(getAmelia("amelia.data"))
  transhold <- rep(0,nn)
  lagshold <- rep(0,nn)
  leadhold <- rep(0,nn)
  transhold[c(1:nn) %in% getAmelia("amelia.list")$amelia.args$idvars]<-6
  transhold[c(1:nn) %in% getAmelia("amelia.list")$amelia.args$ords]<-1
  transhold[c(1:nn) %in% getAmelia("amelia.list")$amelia.args$noms]<-2
  transhold[c(1:nn) %in% getAmelia("amelia.list")$amelia.args$logs]<-3
  transhold[c(1:nn) %in% getAmelia("amelia.list")$amelia.args$sqrts]<-4
  transhold[c(1:nn) %in% getAmelia("amelia.list")$amelia.args$lgstc]<-5
  lagshold[c(1:nn) %in% getAmelia("amelia.list")$amelia.args$lags]<-1
  leadhold[c(1:nn) %in% getAmelia("amelia.list")$amelia.args$leads]<-1

  putAmelia("transmat",transhold)
  putAmelia("amelia.lags",lagshold)
  putAmelia("future.lags",leadhold)

  putAmelia("num.poly",tclVar(getAmelia("amelia.list")$amelia.args$polytime))
  putAmelia("intercs",tclVar(getAmelia("amelia.list")$amelia.args$intercs))
      
  putAmelia("empri",tclVar(getAmelia("amelia.list")$amelia.args$empri))
#  putAmelia("store.pri",getAmelia("amelia.list")$amelia.args$casepri)

  putAmelia("priorsmat",getAmelia("amelia.list")$amelia.args$priors)
  if ((getAmelia("tsvar") != 0) && (getAmelia("csvar") != 0)) {
    tkconfigure(options.tscs,state="active")
  }
  tkselect(options.tsvar,getAmelia("tsvar"))
  tkselect(options.csvar,getAmelia("csvar"))
  set.tsvar()
  set.csvar()
##   if (!is.null(getAmelia("amelia.list")$amelia.args$casepri))
##     tkconfigure(options.csvar, state="disabled")
  putAmelia("output.select",getAmelia("amelia.list")$amelia.args$output.select)
  tkselect(output.drop.box, getAmelia("output.select"))
  putAmelia("outname",tclVar(getAmelia("amelia.list")$amelia.args$outname))
  putAmelia("outnum",tclVar(getAmelia("amelia.list")$amelia.args$m))
  putAmelia("seed", tclVar(getAmelia("amelia.list")$amelia.args$seed))
  putAmelia("tol", tclVar(getAmelia("amelia.list")$amelia.args$tol))
  tkdelete(output.entry,0,"end")
  tkinsert(output.entry,"end",tclvalue(getAmelia("outname")))
  tkdelete(output.num,0,"end")
  tkinsert(output.num,"end", tclvalue(getAmelia("outnum")))
  tkdelete(output.seed,0,"end")
  tkinsert(output.seed,"end", tclvalue(getAmelia("seed")))
  if (names(getAmelia("amelia.list"))[1] == "m1")
    tkconfigure(output.diag, state = "active")
    
  return(NULL)
}


run.amelia <- function() {
  id <- c()
  ord <- c()
  nom <- c()
  logs <- c()
  lgstc <- c()
  sqrts <- c()
  fact <- c()
  amlags <- c()
  amfut <- c()
  ts <- getAmelia("tsvar")
  cs <- getAmelia("csvar")
  nn <- ncol(getAmelia("amelia.data"))

  am.intercs  <- as.logical(as.numeric(tclvalue(getAmelia("intercs"))))
  am.num.poly <- as.numeric(tclvalue(getAmelia("num.poly")))
  
 if (am.num.poly == 0)
    if (am.intercs == FALSE)
      am.num.poly<-NULL
 if (ts == 0) {
    am.num.poly <- NULL
    ts          <- NULL
  }
  if (cs == 0) {
    cs      <- NULL
    am.intercs <- FALSE
  }

  id    <- (1:nn)[getAmelia("transmat")==6]
  ord   <- (1:nn)[getAmelia("transmat")==1]
  nom   <- (1:nn)[getAmelia("transmat")==2]
  logs  <- (1:nn)[getAmelia("transmat")==3]
  sqrts <- (1:nn)[getAmelia("transmat")==4]
  lgstc <- (1:nn)[getAmelia("transmat")==5]
  amlags<- (1:nn)[getAmelia("amelia.lags")==1]
  amfut <- (1:nn)[getAmelia("future.lags")==1]

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

  if (!is.na(as.numeric(tclvalue(getAmelia("seed")))))
    set.seed(as.numeric(tclvalue(getAmelia("seed"))))
    
  ## run amelia! or at least try, and put the output in a list
  ## the name of the list will be the output name set by user
  putAmelia(tclvalue(getAmelia("outname")),
            try(amelia(data     = getAmelia("amelia.data"),
                       m        = as.numeric(tclvalue(getAmelia("outnum"))),
                       p2s      = FALSE,
                       idvars   = id,
                     #  casepri  = getAmelia("store.pri"),
                       ts       = ts,
                       cs       = cs,
                       priors   = pmat,
                       lags     = amlags,
                       empri    = as.numeric(tclvalue(getAmelia("empri"))),
                       intercs  = am.intercs,
                       leads    = amfut, 
                       polytime = am.num.poly,
                       frontend = TRUE,
                       logs     = logs,
                       sqrts    = sqrts,
                       lgstc    = lgstc,
                       ords     = ord,
                       noms     = nom,
                       write.out= FALSE,
                       tolerance= as.numeric(tclvalue(getAmelia("tol")))),
                silent=TRUE))

  ## check for errors in the process.
  if (inherits(getAmelia(tclvalue(getAmelia("outname"))),"try-error")) {
    tkinsert(getAmelia("run.text"),"end","\nThere was an unexpected error in the execution of Amelia.  \nDouble check all inputs for errors and take note of the error message:\n\n")
    tkinsert(getAmelia("run.text"),"end",paste(getAmelia(tclvalue(getAmelia("outname")))))
    return(NULL)
  }

  ##
  if (all(getAmelia(tclvalue(getAmelia("outname")))$code!=c(1,2))) {
    tkinsert(getAmelia("run.text"),"end","\n")
    tkinsert(getAmelia("run.text"),"end",paste("Amelia Error Code:",
                                  getAmelia(tclvalue(getAmelia("outname")))[[1]],"\n",
                                  getAmelia(tclvalue(getAmelia("outname")))[[2]]))
    tkinsert(getAmelia("run.text"),"end","\n\nYou have recieved an error.  You can close this window and reset\n various options to correct the error.")
  }
  else {
    tkinsert(getAmelia("run.text"),"end","\nAmelia has run successfully.  You can close this window and \nuse the diagnostics button to see how your imputations ran.")
    tkconfigure(output.diag, state = "active")
    tksee(getAmelia("run.text"),"end")
    amelia.save(getAmelia(tclvalue(getAmelia("outname"))),tclvalue(getAmelia("outname")),as.numeric(tclvalue(getAmelia("outnum"))))
  }
}

amelia.save <- function(out,outname,m)  {
  if (getAmelia("output.select") == 1)
    for (i in 1:m) 
      write.csv(out[[i]],file=paste(getAmelia("am.directory"),"/",outname,i,".csv",sep=""),row.names=FALSE)
  if (getAmelia("output.select") == 2)
    for (i in 1:m) 
      write.table(out[[i]],file=paste(getAmelia("am.directory"),"/",outname,i,".txt",sep=""),sep="\t",row.names=FALSE)
  if (getAmelia("output.select") == 3)
    for (i in 1:m) 
      write.dta(out[[i]],file=paste(getAmelia("am.directory"),"/",outname,i,".dta",sep=""),version=6)
  if (getAmelia("output.select") == 4)
    for (i in 1:m) 
      write.dta(out[[i]],file=paste(getAmelia("am.directory"),"/",outname,i,".dta",sep=""),version=7)
  if (getAmelia("output.select") == 5)
    save(list=tclvalue(getAmelia("outname")), envir=ameliaEnv(),
         file=paste(getAmelia("am.directory"),"/",outname,".RData",sep=""))
  if (getAmelia("output.select") == 6)
    assign(x=tclvalue(getAmelia("outname")), value=out,envir=.GlobalEnv)
}

set.out<-function(...) {
  putAmelia("output.select",as.numeric(tkcurselection(output.drop.box)))
}

#Preamble
require(tcltk) || stop("The package 'tcltk' is required")
require(foreign)
libdir<-file.path(.find.package(package = "Amelia")[1], "tklibs")
addTclPath(libdir)
tclRequire("combobox")
.Tcl("catch {namespace import ::combobox::*}")
tclRequire("BWidget")

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
putAmelia("inname",     tclVar(""))
putAmelia("seed",       tclVar(""))

gui<-tktoplevel()
tkwm.title(gui, "AmeliaView")
                         
##Menu
main.menu      <- tkmenu(gui)
main.menu.file <- tkmenu(main.menu, tearoff=0)
main.menu.help <- tkmenu(main.menu, tearoff=0)
tkadd(main.menu.file,"command",label="Load Session",command=function()load.session())
tkadd(main.menu.file,"command",label="Save Session",command=function()save.session())
tkadd(main.menu.file,"command",label="ExitAmelia",command=function()main.close())
tkadd(main.menu.help,"command",label="Amelia Website",command=
      function()browseURL("http://gking.harvard.edu/amelia/"))
tkadd(main.menu.help,"command",label="Online Documentation",command=
      function()browseURL("http://gking.harvard.edu/amelia/docs/"))

tkadd(main.menu.help,"command",label="About",command=
      function()tkmessageBox(title="AmeliaView",message="James Honaker, Gary King, & Matthew Blackwell 2006-8",icon="info",type="ok"))
tkadd(main.menu,"cascade",label="File",menu=main.menu.file)
tkadd(main.menu,"cascade",label="Help",menu=main.menu.help)
tkconfigure(gui,menu=main.menu)

##Frame
gui.skel   <- tkframe(gui)
main.title <- tklabel(gui, text="AmeliaView",font=c("Arial",22))

tkgrid(main.title,sticky = "ew")

gui.input   <- tkframe(gui.skel, relief = "groove", borderwidth = 2)
gui.options <- tkframe(gui.skel, relief = "groove", borderwidth = 2)
gui.output  <- tkframe(gui.skel, relief = "groove", borderwidth = 2)

if (.Platform$OS.type == "windows") {
  padding<-5
  putAmelia("helpfont",c("arial",8))
} else {
  putAmelia("helpfont",c("arial",10))
  padding<-3
}

##Input Frame
##Data management,loading, etc.

#Data type combobox
putAmelia("drop.select", 0)
putAmelia("file.kind", "{{Comma-delimited files} {.csv}} {{All files} *} ")
file.types<-c("CSV","Tab Delimited","Stata","SPSS","SAS Transport") #,"RData")
tcl("set", "filetypes",file.types)
input.drop.label<-tklabel(gui.input,text="Input Data Format:")

input.drop.box<-tkwidget(gui.input,"combobox", borderwidth=1,
    editable="FALSE",listvar= "filetypes",width=15)
tkselect(input.drop.box,getAmelia("drop.select"))
tkconfigure(input.drop.box,command=function(...)file.type())


#Data entry and Load
input.entry <- tkentry(gui.input, width="45", textvariable = getAmelia("inname"))
input.browse <- tkbutton(gui.input, text="Browse...", command=function()get.filename())
input.label <- tklabel(gui.input, text="Input Data File:                     ")
input.load <- tkbutton(gui.input, text="Load Data", width = 10, command = function() load.data())
input.see <- tkbutton(gui.input, text="Summarize Data", state = "disabled", width = 13,
  command = function() sum.data())
input.help <-tkbutton(gui.input, text = "?", command = function()browseURL("http://gking.harvard.edu/amelia/docs/Step_1.html"))

#Arranging the frame
tkgrid(tklabel(gui.input, text = "Step 1 - Input", font = c("Arial", "16")), row = 1,
  column = 1, columnspan = 2, sticky = "w")
tkgrid(input.help, row = 1, column = 4, sticky = "ne", padx= 2, pady = 2)  
tkgrid(input.drop.label, row = 2, column = 1, sticky = "w", padx = padding, pady = padding)
tkgrid(input.drop.box, row = 2, column = 2, columnspan = 2, sticky = "w", padx = padding, pady = padding)
tkgrid(input.label, row = 3, column = 1, sticky = "w", padx = padding, pady = padding)
tkgrid(input.entry, row = 3, column = 2, columnspan = 2, sticky = "w", padx = padding, pady = padding)
tkgrid(input.browse, row = 3, column = 4, sticky = "w", padx = padding, pady = padding)
tkgrid(input.load, row = 4, column = 1, columnspan = 1,  sticky = "e", padx = padding, pady = padding)
tkgrid(input.see, row = 4, column = 3, sticky = "w", padx = padding, pady = padding)
tkgrid(tklabel(gui.input,text="        "), row = 4, column = 2)
tkgrid.rowconfigure(gui.input, 1, weight = 1)
tkgrid.rowconfigure(gui.input, 2, weight = 1)
tkgrid.rowconfigure(gui.input, 3, weight = 1)
tkgrid.rowconfigure(gui.input, 4, weight = 1)
tkgrid.columnconfigure(gui.input, 1, weight = 0)
tkgrid.columnconfigure(gui.input, 2, weight = 0)
tkgrid.columnconfigure(gui.input, 3, weight = 0)
tkgrid.columnconfigure(gui.input, 4, weight = 1)


##Options Frame
##TS/CS, Option buttons

#tscs comboboxes
if (is.null(getAmelia("varnames"))) {
  putAmelia("varnames","  .     .       .       .   .     .   .  ..   .  . . . . . . ")
  tcl("set","varnames",getAmelia("varnames"))
}
options.tsvar<-tkwidget(gui.options,"combobox",listvar="varnames",editable="FALSE")
options.csvar<-tkwidget(gui.options,"combobox",listvar="varnames",editable="FALSE")
options.tsvar.lab<-tklabel(gui.options,text="Time Series Index:")
options.csvar.lab<-tklabel(gui.options,text="Cross-Sectional Index:          ")
tkconfigure(options.tsvar,state="disabled")
tkconfigure(options.csvar,state="disabled")


#option buttons
options.var<-tkbutton(gui.options, text = "Variables", state = "disabled", width = 10, 
  command = function() gui.var.setup())
options.tscs<-tkbutton(gui.options, text = "TSCS", state = "disabled", width = 10,
  command = function() gui.tscs.setup())
options.priors<-tkbutton(gui.options, text = "Priors", state = "disabled", width = 10,
  command = function() gui.pri.setup())
options.help <-tkbutton(gui.options, text = "?", 
  command = function()browseURL("http://gking.harvard.edu/amelia/docs/Step_2.html"))

#option button labels
options.var.lab<-tklabel(gui.options, text = "Set options for individual variables")
options.tscs.lab<-tklabel(gui.options, text = "Time series and cross-sectional options")
options.priors.lab<-tklabel(gui.options, text = "Set prior beliefs about the data")

#arranging the frame
tkgrid(tklabel(gui.options, text = "Step 2 - Options", font = c("Arial", "16")),
  row = 1, column = 1, columnspan = 2, sticky = "nw")
tkgrid(options.help, row = 1, column = 3, sticky = "ne", padx= 2, pady = 2)
tkgrid(options.tsvar.lab, row = 2, column = 1, sticky = "w", padx = padding, pady = padding)
tkgrid(options.tsvar, row = 2, column = 2, sticky = "w", padx = padding, pady = padding)
tkgrid(options.csvar.lab, row = 3, column = 1, sticky = "w", padx = padding, pady = padding)
tkgrid(options.csvar, row = 3, column = 2, sticky = "w", padx = padding, pady = padding)
tkgrid(options.var, row = 4, column = 1,padx = padding, pady = padding, sticky = "e")
tkgrid(options.tscs, row = 5, column = 1,padx = padding, pady = padding, sticky = "e")
tkgrid(options.priors, row = 6, column = 1,padx = padding, pady = padding, sticky = "e")
tkgrid(options.var.lab, row = 4, column = 2, sticky = "w",padx = padding, pady = padding)
tkgrid(options.tscs.lab, row = 5, column = 2, sticky = "w",padx = padding, pady = padding)
tkgrid(options.priors.lab, row = 6, column = 2, sticky = "w",padx = padding, pady = padding)
tkgrid.rowconfigure(gui.options, 1, weight = 1)
tkgrid.rowconfigure(gui.options, 2, weight = 1)
tkgrid.rowconfigure(gui.options, 3, weight = 1)
tkgrid.rowconfigure(gui.options, 4, weight = 1)
tkgrid.rowconfigure(gui.options, 5, weight = 1)
tkgrid.rowconfigure(gui.options, 6, weight = 1)
tkgrid.columnconfigure(gui.options, 1, weight = 0)
tkgrid.columnconfigure(gui.options, 2, weight = 0)
tkgrid.columnconfigure(gui.options, 3, weight = 1)


##Output Frame
##output options, run button, diag

putAmelia("output.select",1)
output.types<-c("(no save)","CSV","Tab Delimited","Stata 6","Stata 7/8",
                "RData","Hold in R memory")
tcl("set", "outtypes",output.types)
output.drop.label<-tklabel(gui.output,text="Output Data Format:")

output.drop.box<-tkwidget(gui.output,"combobox",borderwidth=1,
    editable="FALSE",listvar= "outtypes",width=13)
tkselect(output.drop.box,getAmelia("output.select"))
tkconfigure(output.drop.box,command = function(...)set.out())


#output options
output.label  <- tklabel(gui.output, text="Name the Imputed Dataset:")
output.entry  <- tkentry(gui.output, width="15",textvariable = getAmelia("outname"))
output.numlab <- tklabel(gui.output, text = "Number of Imputed Datasets:")
output.num    <- tkentry(gui.output, width = "5", textvariable = getAmelia("outnum"))
output.seedlab<- tklabel(gui.output, text="Seed:")
output.seed   <- tkentry(gui.output, width="5", textvariable=getAmelia("seed"))


#output buttons
output.run<-tkbutton(gui.output,text="Run Amelia", state = "disabled", width = 10,
  command=function()run.amelia())
output.diag<-tkbutton(gui.output, text="Diagnostics", state = "disabled", width = 13,
  command = function() gui.diag.setup())
output.help <-tkbutton(gui.output, text = "?", 
  command = function()browseURL("http://gking.harvard.edu/amelia/docs/Step_3.html"))


#arranging the frame
tkgrid(tklabel(gui.output, text = "Step 3 - Output", font = c("Arial", "16")),
  row = 1, column = 1, columnspan = 2, sticky = "w")
tkgrid(output.help, row = 1, column = 4, sticky = "ne", padx= 2, pady = 2)
tkgrid(output.label, row = 3, column = 1, sticky = "w", padx = padding, pady = padding)
tkgrid(output.entry, row = 3, column = 2, sticky = "w", padx = padding, pady = padding)
tkgrid(output.drop.label, row = 2, column = 1, sticky = "w", padx = padding, pady = padding)
tkgrid(output.drop.box, row = 2, column = 2, sticky = "w", padx = padding, pady = padding)
tkgrid(output.numlab, row = 4, column = 1, sticky = "w", padx = padding, pady = padding)
tkgrid(output.num, row = 4, column = 2, sticky = "w", padx = padding, pady =
       padding)
tkgrid(output.seedlab, row = 5, column = 1, sticky = "w", padx = padding, pady = padding)
tkgrid(output.seed, row = 5, column = 2, sticky = "w", padx = padding, pady = padding)
tkgrid(output.run, row = 6, column = 1, sticky = "e",  padx = padding, pady = padding)
tkgrid(output.diag, row = 6, sticky = "w", column = 3, padx = padding, pady = padding)
tkgrid.rowconfigure(gui.output, 1, weight = 1)
tkgrid.rowconfigure(gui.output, 2, weight = 1)
tkgrid.rowconfigure(gui.output, 3, weight = 1)
tkgrid.rowconfigure(gui.output, 4, weight = 1)
tkgrid.rowconfigure(gui.output, 5, weight = 1)
tkgrid.rowconfigure(gui.output, 6, weight = 1)
tkgrid.columnconfigure(gui.output, 4, weight = 1)


#grid the whole thing
tkgrid(gui.input,row = 1,  column = 1, sticky = "news", pady = 10, padx = 5)
tkgrid(gui.options, row = 2, column = 1, sticky = "news", pady = 10, padx = 5)
tkgrid(gui.output, row = 3, column = 1, sticky = "news", pady = 10, padx = 5)

tkgrid(gui.skel,sticky="w")


#statusbar at the bottom.
statusbar<-tkframe(gui, relief = "groove", borderwidth = 3)
statusbar.lab1a<-tklabel(statusbar, text = "Data Loaded:", anchor = "w", padx = 2)
statusbar.lab1b <- tklabel(statusbar, text = "Unspecified",
	relief = "sunken", anchor = "w", foreground = "red",
	padx = 2, width = 35)
statusbar.nlab<-tklabel(statusbar, text = "Obs:", anchor="e", padx = 2)
statusbar.n <- tklabel(statusbar, text = "----",relief = "sunken", anchor = "w", foreground = "red",padx = 2, width = 6)
statusbar.klab<-tklabel(statusbar, text = "Vars:", anchor="e", padx = 2)
statusbar.k <- tklabel(statusbar, text = "----",
	relief = "sunken", anchor = "w", foreground = "red",
	padx = 2, width = 6)


tcl("set", "helpvar", "")
help.lab<-tklabel(gui,textvariable="helpvar", font=getAmelia("helpfont"))
tkgrid(tkframe(statusbar, width=500,height=0), row = 1, column = 1, columnspan = 6)
tkgrid(help.lab, sticky = "w")
tkgrid(statusbar.lab1a,row = 2, column = 1, sticky="w")
tkgrid(statusbar.lab1b,row = 2, column = 2, sticky="w")
tkgrid(statusbar.nlab,row = 2, column = 3, sticky="w")
tkgrid(statusbar.n,row = 2, column = 4, sticky="w")
tkgrid(statusbar.klab,row = 2, column = 5, sticky="w")
tkgrid(statusbar.k, row = 2, column = 6, sticky = "w")
tkgrid.rowconfigure(statusbar, 2, weight = 1)
tkgrid(statusbar, sticky = "sew")
tkfocus(gui)

tkbind(input.drop.label, "<Motion>","set helpvar \"File format of the input data you are using.\"")
tkbind(input.drop.label, "<Leave>","set helpvar \"\"")
tkbind(input.drop.box, "<Motion>","set helpvar \"File format of the input data you are using.\"")
tkbind(input.drop.box, "<Leave>","set helpvar \"\"")
tkbind(input.label, "<Motion>","set helpvar \"Location of the input data file you are using.\"")
tkbind(input.label, "<Leave>","set helpvar \"\"")
tkbind(input.entry, "<Motion>","set helpvar \"Location of the input data file you are using.\"")
tkbind(input.entry, "<Leave>","set helpvar \"\"")
tkbind(input.browse, "<Motion>","set helpvar \"Locate the input data file you are using.\"")
tkbind(input.browse, "<Leave>","set helpvar \"\"")
tkbind(input.load, "<Motion>","set helpvar \"Load the data into Amelia.  You must do this to move past Step 1.\"")
tkbind(input.load, "<Leave>","set helpvar \"\"")
tkbind(input.see, "<Motion>","set helpvar \"Review summary statistics about the data and plot variable historgrams.\"")
tkbind(input.see, "<Leave>","set helpvar \"\"")
tkbind(options.tsvar.lab, "<Motion>","set helpvar \"Declare the time variable in your dataset.  You must set this to set TSCS options.\"")
tkbind(options.tsvar.lab, "<Leave>","set helpvar \"\"")
tkbind(options.tsvar, "<Motion>","set helpvar \"Declare the time variable in your dataset.  You must set this to set TSCS options.\"")
tkbind(options.tsvar, "<Leave>","set helpvar \"\"")
tkbind(options.csvar.lab, "<Motion>","set helpvar \"Declare the cross-section or index variable.  Required to access the TSCS or Case priors options.\"")
tkbind(options.csvar.lab, "<Leave>","set helpvar \"\"")
tkbind(options.csvar, "<Motion>","set helpvar \"Declare the cross-section or index variable.  Required to access TSCS or Case priors options.\"")
tkbind(options.csvar, "<Leave>","set helpvar \"\"")
tkbind(options.var, "<Motion>","set helpvar \"Select transformations for the individual variables.\"")
tkbind(options.var, "<Leave>","set helpvar \"\"")
tkbind(options.var.lab, "<Motion>","set helpvar \"Select transformations for the individual variables.\"")
tkbind(options.var.lab, "<Leave>","set helpvar \"\"")
tkbind(options.tscs, "<Motion>","set helpvar \"Control the methods Amelia uses to handle Time-Series Cross-Sectional data.\"")
tkbind(options.tscs, "<Leave>","set helpvar \"\"")
tkbind(options.tscs.lab, "<Motion>","set helpvar \"Control the methods Amelia uses to handle Time-Series Cross-Sectional data.\"")
tkbind(options.tscs.lab, "<Leave>","set helpvar \"\"")
tkbind(options.priors, "<Motion>","set helpvar \"Input beliefs about missing values in the data, either at the observation, case, or parameter level.\"")
tkbind(options.priors, "<Leave>","set helpvar \"\"")
tkbind(options.priors.lab, "<Motion>","set helpvar \"Input beliefs about missing values in the data, either at the observation, case, or parameter level.\"")
tkbind(options.priors.lab, "<Leave>","set helpvar \"\"")
tkbind(output.entry, "<Motion>","set helpvar \"Set the prefix for the imputed datasets.\"")
tkbind(output.entry, "<Leave>","set helpvar \"\"")
tkbind(output.label, "<Motion>","set helpvar \"Set the prefix for the imputed datasets.\"")
tkbind(output.label, "<Leave>","set helpvar \"\"")
tkbind(output.num, "<Motion>","set helpvar \"Set the number of imputed datasets.\"")
tkbind(output.num, "<Leave>","set helpvar \"\"")
tkbind(output.numlab, "<Motion>","set helpvar \"Set the number of imputed datasets.\"")
tkbind(output.numlab, "<Leave>","set helpvar \"\"")
tkbind(output.run, "<Motion>","set helpvar \"Run Amelia on your input dataset.\"")
tkbind(output.run, "<Leave>","set helpvar \"\"")
tkbind(output.diag, "<Motion>","set helpvar \"Post-imputation checks for problems in the imputation.\"")
tkbind(output.diag, "<Leave>","set helpvar \"\"")
tkbind(output.help, "<Motion>","set helpvar \"Link to a manual page about Step 3.\"")
tkbind(output.help, "<Leave>","set helpvar \"\"")
tkbind(options.help, "<Motion>","set helpvar \"Link to a manual page about Step 2.\"")
tkbind(options.help, "<Leave>","set helpvar \"\"")
tkbind(input.help, "<Motion>","set helpvar \"Link to a manual page about Step 1.\"")
tkbind(input.help, "<Leave>","set helpvar \"\"")
output.drop.label
tkbind(output.drop.label, "<Motion>","set helpvar \"Set the file format for saving the imputed datasets, if you want to save them.\"")
tkbind(output.drop.label, "<Leave>","set helpvar \"\"")
tkbind(output.drop.box, "<Motion>","set helpvar \"Set the file format for saving the imputed datasets, if you want to save them.\"")
tkbind(output.drop.box, "<Leave>","set helpvar \"\"")

gui.var.setup<-function() {
  tt<-tktoplevel()
  tkwm.title(tt, "Variable Options")
  gui.frame<-tkframe(tt)
  var.save <- function() {
    putAmelia("transmat",getAmelia("temp.trans"))
    putAmelia("tol",tclVar(tclvalue(getAmelia("temptol"))))
    tkdestroy(tt)
  }
  set.trans <- function(index, ind) {
    transind <- which(index==ind,arr.ind=TRUE)
    tmp <- getAmelia("temp.trans")
    tmp[transind[1]] <- transind[2]-1
    putAmelia("temp.trans",tmp)
  }
    


  putAmelia("temp.trans", getAmelia("transmat"))

  var.link <-tkbutton(tt, text = "?", 
    command = function()browseURL("http://gking.harvard.edu/amelia/docs/Variables_Dialog.html"))
  nc <- ncol(getAmelia("amelia.data"))
  #ScrollableFrame for variable options
  sw <- tkwidget(tt,"ScrolledWindow",relief="sunken",borderwidth=2)
  sf <- tkwidget(sw,"ScrollableFrame")
  tcl(sw,"setwidget",sf)
  subfID <- tclvalue(tcl(sf,"getframe"))
  tcltrans <- list()
  trans.radio.buts <- list()
  indexmat<-matrix(1:(nc*7),nc,7)
  transnames<-c("No Transformations","Ordinal","Nominal","Log-Linear","Square Root","Logistic","ID Variable")
  tcl("set","varhelp","")
  for (i in 0:(nc)) {    
    if (i == 0) {
      tkgrid(tcl("label",paste(subfID,".top",2,sep=""),text="No Transformation"),row=1,column=2)
      tkgrid(tcl("label",paste(subfID,".top",3,sep=""),text="Ordinal"),row=1,column=3)
      tkgrid(tcl("label",paste(subfID,".top",4,sep=""),text="Nominal"),row=1,column=4)
      tkgrid(tcl("label",paste(subfID,".top",5,sep=""),text="Log-Linear"),row=1,column=5)
      tkgrid(tcl("label",paste(subfID,".top",6,sep=""),text="Square Root"),row=1,column=6)
      tkgrid(tcl("label",paste(subfID,".top",7,sep=""),text="Logistic"),row=1,column=7)
      tkgrid(tcl("label",paste(subfID,".top",8,sep=""),text="ID Variable"),row=1,column=8)
    } else {
      tcltrans[[i]]<-tclVar(init=paste(getAmelia("temp.trans")[[i]]))
      for (j in 0:7) {
        if (j == 0) {
          tkgrid(tcl("label",paste(subfID,".left",i,sep=""),text=names(getAmelia("amelia.data"))[i]),row=i+1,column=1)
        } else {
          trans.radio.buts[[indexmat[i,j]]]<-tcl("radiobutton",
            paste(subfID,".but",indexmat[i,j],sep=""), variable=tcltrans[[i]],
            value=paste(j-1))
          tkgrid(trans.radio.buts[[indexmat[i,j]]],row=i+1,column=j+1)
          tkbind(paste(subfID,".but",indexmat[i,j],sep=""),"<Motion>",paste("set varhelp \"", transnames[j],"\"",sep=""))
          tkbind(paste(subfID,".but",indexmat[i,j],sep=""),"<Leave>","set varhelp \" \"")
          local({
            tempindexmat<-indexmat
            tempind<-indexmat[i,j]
            tkconfigure(paste(subfID,".but",indexmat[i,j],sep=""),command=function()set.trans(tempindexmat,tempind))})

        }
        
      }
    }
  }
  tkconfigure(sf,width=450)
  tkbind(sf,"<MouseWheel>", function(D){ if (as.numeric(D) > 0 ) {offset <- -1} else {offset <- 1}; tkyview(sf,"scroll",paste(offset),"units")})  #        tkyview(sf,"scroll", ,"units"
  putAmelia("temptol",tclVar(tclvalue(getAmelia("tol"))))
  tolerance.box<-tkentry(tt, width= "7", textvariable=getAmelia("temptol"))
  var.status<-tkframe(tt, relief = "groove", borderwidth = 3)
  var.help<-tklabel(var.status, textvariable="varhelp", font=getAmelia("helpfont"))



  var.ok<-tkbutton(tt,text="OK", command = function() var.save())
  var.cancel<-tkbutton(tt,text="Cancel", command = function() tkdestroy(tt))

  tkpack(var.help, anchor = "w")
  tkgrid(tklabel(tt, text="Variables Options", font="Arial 16 bold"),pady=5, row = 1, column = 1, columnspan = 2,sticky="w")
  tkgrid(sw, sticky="news",columnspan = 4,row=2,column=1)
  
  tkgrid(var.link, row = 1, column = 4, sticky = "ne", padx = 2, pady = 2)
  tkgrid(var.status, sticky="sew", row = 4,columnspan = 5)
  tkgrid(var.ok, row = 3, column = 3, sticky = "sew", padx = 10, pady = 10)
  tkgrid(var.cancel, row = 3, column = 4, sticky = "sew", padx = 10, pady = 10)
  tkgrid(tklabel(tt,text="Tolerance:"), row=3, column=1,sticky="e")
  tkgrid(tolerance.box, row=3, column= 2,sticky="w")
  tkgrid.rowconfigure(tt,2,weight = 1)
  tkgrid.columnconfigure(tt,1,weight=1)
  tkgrid.columnconfigure(tt,3,weight=1)
  tkfocus(sf)
  tkbind(tt,"<Destroy>", function() {tkgrab.release(sf);tkfocus(gui)})
}


sum.data <-function() {
  tt<-tktoplevel()
  tkwm.title(tt, "Data Summary")
  gui.frame<-tkframe(tt)
  select.var<-function(varnum) {
    varnum2<-tknearest(var.list, varnum)
    putAmelia("sum.var",as.numeric(varnum2) + 1)
    putAmelia("sum.var.name",getAmelia("varnames")[getAmelia("sum.var")])
    tkconfigure(var.sum.name,text=getAmelia("sum.var.name"))
		
    if (any(is.factor(getAmelia("amelia.data")[,getAmelia("sum.var")]),
            is.character(getAmelia("amelia.data")[,getAmelia("sum.var")]))) {
      tkconfigure(var.but.plot, state="disabled")
      tkconfigure(var.info.min, text = "Min: (is factor)")
      tkconfigure(var.info.max, text = "Max: ...")
      tkconfigure(var.info.mean,text = "Mean: ...")
      tkconfigure(var.info.sd, text = "SD: ...")
      tkconfigure(var.info.miss,text = "Missing: ...")
    }
    else {
      tkconfigure(var.info.mean,text=paste("Mean:",
                  signif(mean(as.numeric(getAmelia("amelia.data")[,getAmelia("sum.var")]),na.rm=TRUE),digits=4)))
      tkconfigure(var.info.sd,text=paste("SD: ",
                  signif(sd(as.numeric(getAmelia("amelia.data")[,getAmelia("sum.var")]),na.rm=TRUE),digits=4)))
		  tkconfigure(var.info.miss,text=paste("Missing: ",nrow(getAmelia("amelia.data"))-nrow(as.matrix(packr(as.matrix(getAmelia("amelia.data")[,getAmelia("sum.var")])))),"/",nrow(getAmelia("amelia.data")),sep=""))
      tkconfigure(var.but.plot, state="active")
      tkconfigure(var.info.min, text = paste("Min:", 
        signif(min(getAmelia("amelia.data")[,getAmelia("sum.var")],na.rm=TRUE),digits = 4)))
      tkconfigure(var.info.max, text = paste("Max:", 
        signif(max(getAmelia("amelia.data")[,getAmelia("sum.var")],na.rm=TRUE),digits = 4)))
    }
  }
  draw.miss<-function() {
    mm <- tktoplevel()
    tkwm.title(mm, "Missingness")
    sw <- tkwidget(mm,"ScrolledWindow",relief="sunken",borderwidth=2)
    sf <- tkwidget(sw,"ScrollableFrame")
    tcl(sw,"setwidget",sf)
    subfID <- tclvalue(tcl(sf,"getframe"))
    count<-0
    for (i in 1:nrow(getAmelia("amelia.data"))) {
      for (j in 1:ncol(getAmelia("amelia.data"))) {
        count<-count+1
        if (is.na(getAmelia("amelia.data")[i,j]))
          tkgrid(tcl("label",paste(subfID,".",count,sep=""),text="   ",
            bg="white",font="arial 1"),row = i, column = j)
        else  
          tkgrid(tcl("label",paste(subfID,".",count,sep=""),text="   ",
            bg="black",font="arial 1"),row = i, column = j)
      }
    }
    tkgrid(sw,sticky = "news")
    tkfocus(mm)
    tkgrab.set(mm)
    tkbind(mm,"<Destroy>",function(){tkgrab.release(mm);tkfocus(tt)})
 }
  sum.plot <- function() {
    if (.Platform$OS.type == "windows")
      windows()
    else
      x11()
    hist(getAmelia("amelia.data")[,getAmelia("sum.var")],
         main = paste("Histogram of",getAmelia("sum.var.name")), ylab = "Frequnecy",
         xlab ="", col="wheat")
  }
  gui.sum.left <-tkframe(gui.frame, relief = "groove", borderwidth = 2)
  gui.sum.right<-tkframe(gui.frame, relief = "groove", borderwidth = 2)
  
  
  sum.link <-tkbutton(gui.frame, text = "?", 
    command = function()browseURL("http://gking.harvard.edu/amelia/docs/"))

  
  if (!exists("sum.var")) {
    putAmelia("sum.var",1)
    putAmelia("sum.var.name",getAmelia("varnames")[getAmelia("sum.var")])
  }
  scr <- tkscrollbar(gui.sum.left, repeatinterval=5,
				   command=function(...)tkyview(var.list,...))
  #Listbox for variables.
  var.list.label<-tklabel(gui.sum.left,text="Select a variable:")
  var.list<-tklistbox(gui.sum.left,height=10,selectmode="single",
    yscrollcommand=function(...)tkset(scr,...))
  for (i in 1:ncol(getAmelia("amelia.data")))
    tkinsert(var.list,"end",getAmelia("varnames")[i])
  tkselection.set(var.list,(getAmelia("sum.var")-1))
  tkbind(var.list,"<Button-1>",function(y)select.var(y))
  tkbind(var.list,"<Up>",function()select.var(getAmelia("sum.var")))
  tkbind(var.list,"<Down>",function()select.var(getAmelia("sum.var")-2))
	
  var.sum.name <- tklabel(gui.sum.right, text = getAmelia("sum.var.name"),
    font = c("arial", 10, "bold"))
  
  if (!any(is.factor(getAmelia("amelia.data")[,getAmelia("sum.var")]),
           is.character(getAmelia("amelia.data")[,getAmelia("sum.var")]))) {
    var.info.mean<-tklabel(gui.sum.right,text=paste("Mean:",
	  	signif(mean(as.numeric(getAmelia("amelia.data")[,getAmelia("sum.var")]),na.rm=TRUE),digits=4)))
    var.info.sd<-tklabel(gui.sum.right,text=paste("SD:",
      signif(sd(as.numeric(getAmelia("amelia.data")[,getAmelia("sum.var")]),na.rm=TRUE),digits=4)))
    var.info.miss<-tklabel(gui.sum.right,text=paste("Missing: ",
      nrow(getAmelia("amelia.data"))-nrow(as.matrix(packr(as.matrix(getAmelia("amelia.data")[,getAmelia("sum.var")])))),"/",nrow(getAmelia("amelia.data")),sep=""))
    var.info.min<-tklabel(gui.sum.right, text = paste("Min:", 
      signif(min(getAmelia("amelia.data")[,getAmelia("sum.var")]),digits = 4)))
    var.info.max<-tklabel(gui.sum.right, text = paste("Max:", 
      signif(max(getAmelia("amelia.data")[,getAmelia("sum.var")]),digits = 4)))
  }
  else {
    var.info.mean<-tklabel(gui.sum.right,text="Mean: (is factor)")
    var.info.sd<-tklabel(gui.sum.right,text="SD: ...")
    var.info.miss<-tklabel(gui.sum.right,text="Missing: ...")
    var.info.min<-tklabel(gui.sum.right, text = "Min: ...")
    var.info.max<-tklabel(gui.sum.right, text = "Max: ...")
  }
  sum.close<-tkbutton(gui.frame,text="Close",command=function()tkdestroy(tt))

  test<-tkbutton(gui.sum.right, text="miss",command = function()draw.miss())
  var.but.plot <- tkbutton(gui.sum.right, text = "Plot Variable",
                           command = function()  sum.plot())
  if (is.factor(getAmelia("amelia.data")[,getAmelia("sum.var")]))
    tkconfigure(var.but.plot, state="disabled")

  tkgrid(var.list.label, row = 1, column = 1, columnspan = 2)
  tkgrid(var.list,row = 2, column = 1)
  tkgrid(scr, row = 2, column = 2)
  tkgrid.configure(scr,rowspan=4,sticky="nsw")
  tkgrid(tkframe(gui.sum.right, height = 0, width = 150), row = 1, columnspan = 2)
  tkgrid(var.sum.name, row = 2, padx = 5)
  tkgrid(var.info.mean, row = 3, padx=5, sticky = "w")
  tkgrid(var.info.sd, row = 4, padx=5, sticky = "w")
  tkgrid(var.info.miss, row = 5, padx=5, sticky = "w")
  tkgrid(var.info.min, row = 6, padx=5, sticky = "w")
  tkgrid(var.info.max, row = 7, padx=5, sticky = "w")
  tkgrid(var.but.plot, row = 8, padx = 5)
                                        #tkgrid(test, row = 9, padx = 5)
  tkfocus(tt)
  tkgrab.set(tt)
  tkbind(tt,"<Destroy>", function() {tkgrab.release(tt);tkfocus(gui)})
 	tkgrid(tklabel(gui.frame, text="Data Summary", font="Arial 16 bold"),pady=5, row = 1, column = 1,sticky="w")
	tkgrid(sum.link, row = 1, column = 3, sticky = "ne", padx = 2, pady = 2)
	tkgrid(gui.sum.left, row = 2, column = 1, padx = 10, pady = 10, sticky = "nsew")
	tkgrid(gui.sum.right, row = 2, column = 2, columnspan = 2, padx = 10, pady = 10, sticky = "nsew")
	tkgrid(sum.close, row = 3, column = 3, sticky="news", padx = 10, pady=10)
  tkpack(gui.frame)
}


gui.tscs.setup<-function() {

  ## this function is called as we select a new variable.
  ## it grabs the value
  select.var<-function(varnum) {
    tmp1 <- getAmelia("temp.lags")
    tmp2 <- getAmelia("temp.leads")
    tmp1[getAmelia("tscs.var")] <- as.numeric(tclvalue(getAmelia("lag.but.var")))
    tmp2[getAmelia("tscs.var")] <- as.numeric(tclvalue(getAmelia("lead.but.var")))
    putAmelia("temp.lags",tmp1)
    putAmelia("temp.leads",tmp2)
    varnum2<-tknearest(var.list, varnum)
    putAmelia("tscs.var",as.numeric(varnum2) + 1)
    putAmelia("tscs.var.name",getAmelia("varnames")[getAmelia("tscs.var")])
    putAmelia("lag.but.var", tclVar(getAmelia("temp.lags")[getAmelia("tscs.var")]))
    putAmelia("lead.but.var", tclVar(getAmelia("temp.leads")[getAmelia("tscs.var")]))
    tkconfigure(var.tscs.name,text= getAmelia("tscs.var.name"))
    tkconfigure(tscs.lag, variable = getAmelia("lag.but.var"))
    tkconfigure(tscs.fut, variable = getAmelia("lead.but.var"))
  }
  tscs.save<-function() {
    tmp1 <- getAmelia("temp.lags")
    tmp2 <- getAmelia("temp.leads")
    tmp1[getAmelia("tscs.var")] <- as.numeric(tclvalue(getAmelia("lag.but.var")))
    tmp2[getAmelia("tscs.var")] <- as.numeric(tclvalue(getAmelia("lead.but.var")))
    putAmelia("temp.lags",tmp1)
    putAmelia("temp.leads",tmp2)
    putAmelia("num.poly",getAmelia("temp.num"))
    putAmelia("intercs",getAmelia("temp.inter"))
    putAmelia("amelia.lags",getAmelia("temp.lags"))
    putAmelia("future.lags",getAmelia("temp.leads"))
    tkdestroy(tt)
  }
  
  ## set up some holders for all the options we mess with these and
  ## then throw them away if "cancel" is hit and saved to their
  ## true values if "ok" is hit
  putAmelia("temp.num",tclVar(tclvalue(getAmelia("num.poly"))))
  putAmelia("temp.inter",tclVar(tclvalue(getAmelia("intercs"))))
  putAmelia("temp.lags",getAmelia("amelia.lags"))
  putAmelia("temp.leads",getAmelia("future.lags"))
  
  if (!exists("tscs.var")) {
    putAmelia("tscs.var",1)
    putAmelia("tscs.var.name",getAmelia("varnames")[getAmelia("tscs.var")]) 
  }


  tt<-tktoplevel()
  tkwm.title(tt, "TSCS Options")
  gui.top<-tkframe(tt, relief = "groove", borderwidth = 2)
  gui.bottom1<-tkframe(tt, relief = "groove", borderwidth = 2)
  gui.bottom2<-tkframe(tt, relief = "groove", borderwidth = 2)
  
  putAmelia("lag.but.var", tclVar(getAmelia("temp.lags")[getAmelia("tscs.var")]))
  putAmelia("lead.but.var", tclVar(getAmelia("temp.leads")[getAmelia("tscs.var")]))
  tscs.link <-tkbutton(tt, text = "?", 
    command = function()browseURL("http://gking.harvard.edu/amelia/docs/Time_Series_Cross.html"))
  
  
  poly.time<-tklabel(gui.top, text="Polynomials of time?")
  poly.time0<-tkradiobutton(gui.top, text = "0", variable = getAmelia("temp.num"), 
    value = "0")
  poly.time1<-tkradiobutton(gui.top, text = "1", variable = getAmelia("temp.num"), 
    value = "1")
  poly.time2<-tkradiobutton(gui.top, text = "2", variable = getAmelia("temp.num"), 
    value = "2")
  poly.time3<-tkradiobutton(gui.top, text = "3", variable = getAmelia("temp.num"), 
    value = "3")
  inter.cs<-tkcheckbutton(gui.top, text = "Interact with Cross-Section",
    variable = getAmelia("temp.inter"))
  if (getAmelia("csvar") == 0)
    tkconfigure(inter.cs, state = "disabled")
 
  #Listbox for variables.
  scr <- tkscrollbar(gui.bottom1, repeatinterval=5,
                     command=function(...)tkyview(var.list,...))
  var.list.label<-tklabel(gui.bottom1,text="Select a variable:")
  var.list<-tklistbox(gui.bottom1,height=10,selectmode="single",
                      yscrollcommand=function(...)tkset(scr,...))
  for (i in 1:ncol(getAmelia("amelia.data")))
    tkinsert(var.list,"end",getAmelia("varnames")[i])
  tkselection.set(var.list,(getAmelia("tscs.var")-1))
  tkbind(var.list,"<Button-1>",function(y)select.var(y))
	
  var.tscs.label<-tklabel(gui.bottom2,text="Set variable options for:")
  var.tscs.name <- tklabel(gui.bottom2, text = getAmelia("tscs.var.name"),
                           font = c("arial", 10, "bold"))
  tscs.lag <- tkcheckbutton(gui.bottom2, text = "Include Lag",
                            variable = getAmelia("lag.but.var"))
  tscs.fut <- tkcheckbutton(gui.bottom2, text = "Include Lead",
                            variable = getAmelia("lead.but.var"))
  
  tcl("set","tscshelp","")
  tscs.status<-tkframe(tt, relief = "groove", borderwidth = 3, width = 500)
  tscs.help<-tklabel(tscs.status, textvariable="tscshelp", font=getAmelia("helpfont"))

  tscs.ok<-tkbutton(tt,text="   OK   ", command = function() tscs.save())
  tscs.cancel<-tkbutton(tt,text="Cancel", command = function()tkdestroy(tt))
  tkpack(tkframe(tscs.status, height = 0, width = 400))
  tkpack(tscs.help, anchor = "w")
  tkgrid(poly.time,poly.time0,poly.time1,poly.time2,poly.time3, sticky = "nw")
  tkgrid(inter.cs, sticky = "nw")
  tkgrid(var.list.label,row = 1, column = 1)
  tkgrid(var.list, row = 2, column = 1)
  tkgrid(scr, row = 2, column = 2, sticky="nsw")
  tkgrid(var.tscs.label, row = 2, column = 1)
  tkgrid(var.tscs.name, row = 3, column = 1)
  tkgrid(tscs.lag, row = 4, column = 1)
  tkgrid(tscs.fut, row = 5, column = 1)
  tkgrid(tklabel(tt, text="TSCS Options", font="Arial 16 bold"),pady=5, row = 1, column = 0,sticky="w")
  tkgrid(tscs.link, row = 1, column = 2, sticky = "ne", padx = 2, pady = 2)
  tkgrid(gui.top, row = 2, column = 0, columnspan = 3, padx = 5, pady = 5, sticky = "nsew")
  tkgrid(gui.bottom1, row = 3, column = 0, padx = 5, pady = 5,sticky="nsew")
  tkgrid(gui.bottom2, row = 3, column = 1, columnspan = 2, padx = 5, pady = 5,sticky="nsew")
  tkgrid(tscs.status, row = 5, columnspan = 3, sticky = "sew")
  tkgrid(tscs.ok, row = 4, column = 1, sticky = "sew", padx = 10, pady = 10)
  tkgrid(tscs.cancel, row = 4, column = 2, sticky = "sew", padx = 10, pady = 10)
  
  tkfocus(tt)
  tkgrab.set(tt)
  tkbind(tt,"<Destroy>", function() {tkgrab.release(tt);tkfocus(gui)})
  
 	tkbind(poly.time, "<Motion>","set tscshelp \"Use trends of time in imputation.\"")
  tkbind(poly.time, "<Leave>","set tscshelp \"\"")
 	tkbind(inter.cs, "<Motion>","set tscshelp \"Use different trends of time for each case.\"")
  tkbind(inter.cs, "<Leave>","set tscshelp \"\"")
 	tkbind(poly.time0, "<Motion>","set tscshelp \"Add to the imputation model: Constant (Fixed Effects if Interacted)\"")
  tkbind(poly.time0, "<Leave>","set tscshelp \"\"")
 	tkbind(poly.time1, "<Motion>","set tscshelp \"Add to the imputation model: Constant + Time terms\"")
  tkbind(poly.time1, "<Leave>","set tscshelp \"\"")
 	tkbind(poly.time2, "<Motion>","set tscshelp \"Add to the imputation model: Constant + Time + Time^2 terms\"")
  tkbind(poly.time2, "<Leave>","set tscshelp \"\"")
 	tkbind(poly.time3, "<Motion>","set tscshelp \"Add to the imputation model: Constant + Time + Time^2 + Time^3 terms\"")
  tkbind(poly.time3, "<Leave>","set tscshelp \"\"")
 	tkbind(tscs.lag, "<Motion>","set tscshelp \"Include a lag of this variable in the imputation model\"")
  tkbind(tscs.lag, "<Leave>","set tscshelp \"\"")
 	tkbind(tscs.fut, "<Motion>","set tscshelp \"Include a future lag of this variable in the imputation model\"")
  tkbind(tscs.fut, "<Leave>","set tscshelp \"\"")
}


gui.pri.setup<-function() {

##   case.priors <- function() {
##     reset.case <- function() {
##       putAmelia("store.pri",NULL)
##       tkdestroy(ss)
##       case.priors()
##       return(NULL)
##     }
##     pri.toggle <- function(ii,jj) {
##       ind <- ii + sum(seq(from = 1, to = (jj - 1)))
##       pre.state <- getAmelia("hold.pri")[ii,jj]
##       if (pre.state == 3)
##         new.state <- 0
##       else
##         new.state <- pre.state + 1
##       tkconfigure(button.list[[ind]], text = new.state,
##             background = bground[new.state+1],activebackground=bground[new.state+1])
##       hold.pri[ii,jj] <<- new.state
##     }
##     case.save<-function() {
##       putAmelia("store.pri",getAmelia("hold.pri"))
##       tkdestroy(ss)
##     }
##     ss <- tktoplevel()
##     tkwm.title(ss, "Case Priors")
##     sw <- tkwidget(ss,"ScrolledWindow",relief="sunken",borderwidth=2)
##     sf <- tkwidget(sw,"ScrollableFrame")
##     tcl(sw,"setwidget",sf)
##     subfID <- tclvalue(tcl(sf,"getframe"))
##     reset.c<-tkbutton(ss, text = "Reset All", command = function() reset.case(), width = 10)
##     case.ok<-tkbutton(ss, text = "OK", command = function() case.save(), width = 10)
##     case.can<-tkbutton(ss, text = "Cancel", command = function() tkdestroy(ss), width = 10)
##     legend<-tkframe(ss, relief = "groove", borderwidth = 2)
##     grey.zero<-tklabel(legend, text = "0 - No relationship", bg = "grey")
##     green.one<-tklabel(legend, text = "1 - Slightly similar", bg = "green")
##     yellow.two<-tklabel(legend, text = "2 - Midly similar", bg = "yellow")
##     red.three<-tklabel(legend, text = "3 - Highly similar", bg = "red")
##     tkgrid(grey.zero, green.one, yellow.two, red.three, padx = 5, pady = 5)
##     caselist <- unique(getAmelia("amelia.data")[,getAmelia("csvar")])
##     casenum <- nrow(as.matrix(caselist))
##     if (is.null(getAmelia("store.pri")))
##       putAmelia("store.pri",matrix(0,nrow = casenum, ncol = casenum))
##     putAmelia("hold.pri",store.pri)
##     bground <- c("grey","green", "yellow", "red")
##     leg <- matrix(0,nrow = casenum, ncol= casenum)
##     button.list <- list()
##     tkgrid(legend, row = 1, columnspan = 5, padx = 3, pady = 3) 
##     tkgrid(case.ok, padx = 3, pady = 3, column = 1, sticky = "es", row = 3)
##     tkgrid(case.can, padx = 3, pady = 3, column = 2, sticky = "es", row = 3)
##     tkgrid(reset.c, padx = 3, pady = 3, column = 3, sticky = "es", row = 3)
##     tkconfigure(options.csvar, state="disabled")
##     for (i in 1:casenum) {
##       if (i !=1)
##         if (storage.mode(caselist) == "character")  {
##           tkgrid(tcl("label",paste(subfID,".top",i,sep=""), 
##             text = substr(as.list(caselist)[[i]],1,3), font=c("Courier",10)), row = 1, column = (i + 1))
##         }
##         else {
##           tkgrid(tcl("label",paste(subfID,".top",i,sep=""), 
##             text = substr(as.character(as.list(caselist)[[i]]),1,3), font=c("Courier",10)), row = 1, column = (i + 1))
##         }
##         tkgrid(tcl("label",paste(subfID,".left",i,sep=""), 
##           text = as.list(caselist)[[i]], font = c("Courier",10)), row = (i+1), column = 1,
##           columnspan=(i+1),sticky="e")
##     }
##     for (i in 1:casenum) {
##       for (j in 1:casenum) {
##         if (i > j)
##           next
##         if ((i == 1) && (j == 1))  {
##           ind <- 1
##           leg[i,j] <- 1
##         }
##         else {
##           ind <- i + sum(seq(from = 1, to = (j - 1)))
##           leg[i,j] <- i + sum(seq(from = 1, to = (j - 1)))
##         }
##         button.list[[ind]] <- tcl("button",paste(subfID,".but",ind,sep=""), 
##           text = paste(getAmelia("hold.pri")[i,j]), background = bground[hold.pri[i,j]+1], activebackground=bground[hold.pri[i,j]+1])
##         if (i != j)
##           tkgrid(button.list[[ind]], row = (i + 1) , column = (j + 1), pady = 2)
##         local ({
##           temp <- ind
##           ii <- i
##           jj <- j
##           tkconfigure(button.list[[temp]], command = function() pri.toggle(ii,jj))
##         })
##       }
##     }
##     tkgrid(sw,sticky = "news", columnspan = 5, row = 2)
##     tkgrid.rowconfigure(ss,2,weight = 1)
##     tkgrid.columnconfigure(ss,1,weight=1)
##     tkfocus(ss)
##     tkgrab.set(ss)
##     tkbind(ss,"<Destroy>",function(){tkgrab.release(ss);tkfocus(tt)})
##   }

  obs.priors2 <- function() {
    removePriors <- function() {
      if (is.null(getAmelia("priorsmat"))) {
        tkmessageBox(title="No priors",
                     message="There are no priors to remove.",
                     icon="error", type="ok")
      }
      selectedPriors <- as.numeric(lapply(priorsChecked,
                                          function(x) as.numeric(tclvalue(x))))
      rowSelected <- which(selectedPriors==1)

      mess <- "Are you sure you want to remove the selected priors?"
      sure <- tkmessageBox(title="Remove Prior",message=mess,
                           icon="question", type="yesno", default="no")
      if (tclvalue(sure)=="no")
        return()
      putAmelia("priorsmat", getAmelia("priorsmat")[-rowSelected,, drop=FALSE])
      if (nrow(getAmelia("priorsmat"))==0)
        putAmelia("priorsmat",NULL)

      tkdestroy(pp)
      obs.priors2()
      return()
    }
    addPrior <- function(type) {
      onOK <- function() {
        caseSelection <- as.numeric(tkcurselection(casesBox))
        varSelection  <- as.numeric(tkcurselection(varsBox)) +1
        

        if (caseSelection==0) {
          rowSelection <- 0
          colSelection <- which(anyMissing)[varSelection]
        } else {
          rowSelection  <- missingCases[caseSelection]
          colSelection <- which(is.na(getAmelia("amelia.data")[rowSelection,]))[varSelection]
        }
        
        # fork for range vs. dist
        if (type=="range") {
          if (tclvalue(priorMin)=="") {
            tkmessageBox(title="Error",
                         message="Please enter a mean value.",
                         type="ok",icon="error")
            return()
          }
          if (tclvalue(priorMax)=="") {
            tkmessageBox(title="Error",
                         message="Please enter a mean value.",
                         type="ok",icon="error")
            return()
          }          

          if (tclvalue(priorConf)=="") {
            tkmessageBox(title="Error",
                         message="Please enter a mean value.",
                         type="ok",icon="error")
            return()
          }          
          if (isTRUE(as.numeric(tclvalue(priorConf)) == 0)) {
            tkmessageBox(title="Error",
                         message="Confidence levels must be between 0 and 1.",
                         type="ok",icon="error")
            return()
          }
          
          prMax <- as.numeric(tclvalue(priorMax))
          prMin <- as.numeric(tclvalue(priorMin))
          prCon <- as.numeric(tclvalue(priorConf))
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
          if (tclvalue(priorMean)=="") {
            tkmessageBox(title="Error",
                         message="Please enter a mean value.",
                         type="ok",icon="error")
            return()
          }
          if (tclvalue(priorSD)=="") {
            tkmessageBox(title="Error",
                         message="Please enter a SD value.",
                         type="ok",icon="error")
            return()
          }
          if (isTRUE(as.numeric(tclvalue(priorSD)) == 0)) {
            tkmessageBox(title="Error",
                         message="Standard deviations must be greater than 0.",
                         type="ok",icon="error")
            return()
          }
          prMean <- as.numeric(tclvalue(priorMean))
          prSD   <- as.numeric(tclvalue(priorSD))
          
          
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
          over <- tkmessageBox(title="Overwrite Prior",message=mess,
                               icon="question",type="yesno",default="no")
          if (tclvalue(over)=="no")
            return()
          else
            putAmelia("priorsmat",getAmelia("priorsmat")[-which(matchPrior),])
        }
          
        putAmelia("priorsmat",rbind(getAmelia("priorsmat"),newPrior))
        tkdestroy(addPrior.diag)
        tkdestroy(pp)
        obs.priors2()
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
        if (isTRUE(grep("^0*\\.?[0-9]*$",x)==1))
          return(tclVar("TRUE"))
        else
          return(tclVar("FALSE"))
      }
      setMissingVars <- function() {
        
        currentSelection <- as.numeric(tkcurselection(casesBox))
        currentCase      <- missingCases[currentSelection]
        if (currentSelection==0)
          missVars <- anyMissing
        else
          missVars    <- is.na(getAmelia("amelia.data")[currentCase,])
        missVarNames<- colnames(getAmelia("amelia.data"))[missVars]
        tcl("set","priVarNames",missVarNames)
        tkselect(varsBox,0)
      }
        
      
      addPrior.diag <- tktoplevel()
      tkwm.title(addPrior.diag, "Add Prior")
      missingCases <- which(!complete.cases(getAmelia("amelia.data")))
      anyMissing   <- apply(getAmelia("amelia.data"), 2, function(x) any(is.na(x)))
#      if (all(csvar>0,tsvar>0)) {
#        cases <- apply(amelia.data[missingCases,c(csvar,tsvar),drop=F],1,
#                       function(x) {
#                         if (length(x)==2)
#                           return(paste(x[1],x[2]))
#                         else
#                           paste(x)})
#      } else if (all(csvar==0,tsvar==0)) {
#        cases <- rownames(amelia.data)[missingCases]
#      } else {
#        cases <- paste(amelia.data[missingCases,c(csvar,tsvar)])
#      }

      cases <- paste(rownames(getAmelia("amelia.data"))[missingCases], ") ",
                     getAmelia("amelia.data")[missingCases,getAmelia("csvar")]," ",
                     getAmelia("amelia.data")[missingCases,getAmelia("tsvar")], sep="")
      
      cases <- c("(whole variable)",cases)
      vars <- getAmelia("varnames")[anyMissing]
      tcl("set","priVarNames",vars)
      tcl("set","caseNames",cases)
      casesBox <-tkwidget(addPrior.diag,"combobox",borderwidth=1,
                           editable="FALSE",listvar= "caseNames",width=18)
      varsBox <- tkwidget(addPrior.diag,"combobox",borderwidth=1,
                           editable="FALSE",listvar= "priVarNames",width=18)
      tkgrid(tklabel(addPrior.diag, text="Case:"), column=1, row=1)
      tkgrid(tklabel(addPrior.diag, text="Variable:"), column=1, row=2)
      tkselect(casesBox, 0)
      tkselect(varsBox, 0)
      tkconfigure(casesBox, command=function(...) setMissingVars())
      tkgrid(casesBox, column=2, row=1, pady=3)
      tkgrid(varsBox,  column=2, row=2, pady=3)
      
      if (type=="dist") {
        priorMean <- tclVar()
        priorSD   <- tclVar()
        
        meanBox <- tkentry(addPrior.diag, textvar=priorMean, validate="key",
                           vcmd = function(P) validateNumeric(P))

        sdBox <- tkentry(addPrior.diag, textvar=priorSD, validate="key",
                         vcmd = function(P) validateSD(P))

        tkgrid(tklabel(addPrior.diag, text="Mean:"), column=1, row=3)
        tkgrid(tklabel(addPrior.diag, text="Standard Deviation:"), column=1,
               row=4)
        tkgrid(meanBox, column=2, row=3, pady=5, padx=5)
        tkgrid(sdBox, column=2, row=4, pady=5, padx=5)        
        
      } else {
        priorMin  <- tclVar()
        priorMax  <- tclVar()
        priorConf <- tclVar()
        
        minBox  <- tkentry(addPrior.diag, textvar=priorMin, validate="key",
                           vcmd = function(P) validateNumeric(P))

        maxBox  <- tkentry(addPrior.diag, textvar=priorMax, validate="key",
                           vcmd = function(P) validateNumeric(P))

        
        confBox <- tkentry(addPrior.diag, textvar=priorConf, validate="key",
                           vcmd = function(P) validateNumeric(P))

        tkgrid(tklabel(addPrior.diag, text="Minimum:"), column=1, row=3)
        tkgrid(tklabel(addPrior.diag, text="Maximum:"), column=1, row=4)
        tkgrid(tklabel(addPrior.diag, text="Confidence:"), column=1, row=5)
       
        tkgrid(minBox, column=2, row=3, pady=5, padx=5)
        tkgrid(maxBox, column=2, row=4, pady=5, padx=5)
        tkgrid(confBox, column=2, row=5, pady=5, padx=5)
        
      }
      
      addPriorOK <- tkbutton(addPrior.diag, text="   OK   ", command=function() onOK())
      addPriorCancel <- tkbutton(addPrior.diag, text="Cancel",
                                 command=function() tkdestroy(addPrior.diag))

      tkgrid(addPriorOK, column=1,row=6, padx=5, pady=5)
      tkgrid(addPriorCancel, column=2, row=6, padx=5, pady=5)

      tkgrab.set(addPrior.diag)
      tkfocus(addPrior.diag)
      tkbind(addPrior.diag,"<Destroy>",
             function(){tkgrab.release(addPrior.diag);tkfocus(pp);tkgrab.set(pp)})
    }
    

    pp <- tktoplevel()
    
    tkwm.title(pp,"Observational Priors")
    current.priors <- tkframe(pp)
    currentPriorsSW <- tkwidget(pp, "ScrolledWindow", relief="sunken", borderwidth=2)
    priorsChecked <- list()
    currentPriorsSF <- tkwidget(currentPriorsSW, "ScrollableFrame")
    subfID <- tclvalue(tcl(currentPriorsSF,"getframe"))
    tcl(currentPriorsSW, "setwidget", currentPriorsSF)
    subfID <- tclvalue(tcl(currentPriorsSF, "getframe"))
    addDistPriorButton <- tkbutton(pp, text = "Add Distribution Prior",
                               command = function() addPrior(type="dist"),
                                   width=20)
    addRangePriorButton <- tkbutton(pp, text = "Add Range Prior",
                               command = function() addPrior(type="range"),
                                    width=20)
    removePriorButton <- tkbutton(pp, text = "Remove Selected Priors", command =
                                  function() removePriors(),
                                  width=20)
    obsOK <- tkbutton(pp, text="OK", command=function() {tkdestroy(pp)},width=10)
    
    if (is.null(getAmelia("priorsmat")))
      localPriors <- matrix(NA,1,1)
    else
      localPriors <- getAmelia("priorsmat")
    tkgrid(tcl("label",paste(subfID,".1h",sep=""), text="case",
               width=20),row=0,column=2)
    tkgrid(tcl("label",paste(subfID,".1k",sep=""), text="variable",
               width=20),row=0,column=3)
    tkgrid(tcl("label",paste(subfID,".1l",sep=""), text="mean",
               width=20),row=0,column=4)
    tkgrid(tcl("label",paste(subfID,".1m",sep=""), text="std dev",
               width=20),row=0,column=5)
    
    for (i in 1:nrow(localPriors)) {
      if (ncol(localPriors) == 1)
        next()
      priorsChecked[[i]]<-tclVar("0")
      tkgrid(tcl("checkbutton",paste(subfID,".",i,sep=""),
                 variable = paste(priorsChecked[[i]])),
             pady=5, padx=5, row=i, column=1)

      if ((localPriors[i,1]==0))
        case <- "(whole variable)"
      else
        case <- paste(as.character(getAmelia("amelia.data")[localPriors[i,1],getAmelia("csvar")]),
                      as.character(getAmelia("amelia.data")[localPriors[i,1],getAmelia("tsvar")]))
      
      tkgrid(tcl("label",paste(subfID,".",i,"i",sep=""),text=case,
                 relief="sunken", width=20),row=i,column=2)

      tkgrid(tcl("label",paste(subfID,".",i,"i2",sep=""),text=
             names(getAmelia("amelia.data"))[localPriors[i,2]], relief="sunken",
                 width=20),row=i,column=3)

      
      tkgrid(tcl("label",paste(subfID,".",i,"i3",sep=""),text=
             as.character(round(localPriors[i,3],5)), relief="sunken", width=20),
             row=i,column=4)

      
      tkgrid(tcl("label",paste(subfID,".",i,"i4",sep=""),text=
             as.character(round(localPriors[i,4],5)), relief="sunken", width=20),
             row=i,column=5)
    }
    if (.Platform$OS.type == "windows")
      box.width <- 550
    else
      box.width <- 625
    tkconfigure(currentPriorsSF,width=box.width, height=100)
    tkgrid(currentPriorsSW, row=1, sticky="news", column=1,columnspan=3)
    tkgrid(addDistPriorButton, row=2,sticky="nsew", column=1,padx=5,pady=5)
    tkgrid(addRangePriorButton, row=2,sticky="nsew", column=2,padx=5,pady=5)
    tkgrid(removePriorButton,row=2,sticky="nsew", column=3, pady=5, padx=5)
    tkgrid(obsOK, row=4, column=3, sticky="nse", padx=5, pady=5)
    tkfocus(pp)
    tkgrab.set(pp)
    tkbind(pp,"<Destroy>",function(){tkgrab.release(pp);tkfocus(tt);tkgrab.set(tt)})
  }
    
                                        
  pri.save <- function() {
    putAmelia("empri",tclVar(as.numeric(tclvalue(temp.empri))))
    tkdestroy(tt)
  }


  tt<-tktoplevel()
  tkwm.title(tt, "Priors Settings")
  gui.frame<-tkframe(tt)
  priors <- tkframe(tt, relief = "groove", borderwidth = 2)
	
  
  temp.empri<-tclVar(as.numeric(tclvalue(getAmelia("empri"))))
  empri.ent<-tkentry(priors,width=4,textvariable = temp.empri)
  empri.label<-tklabel(priors,text="Empirical prior:")
##   case.but<-tkbutton(gui.frame,text="Set case priors",
##                      command = function() case.priors())
  
##   if (getAmelia("csvar") == 0)
##     tkconfigure(case.but, state="disabled")

  data.but<-tkbutton(gui.frame,text="Set observation priors")
  tkconfigure(data.but, command = function() obs.priors2())	
  tcl("set","prihelp","")
  
  pri.link <-tkbutton(tt, text = "?", 
    command = function()browseURL("http://gking.harvard.edu/amelia/docs/Priors_Dialog.html"))
  
  
  pri.status <- tkframe(tt, relief = "groove", borderwidth = 2)
  prihelp<-tklabel(pri.status, textvariable="prihelp", font=getAmelia("helpfont"))
  
  pri.ok<-tkbutton(tt,text="   OK   ", command = function() pri.save())
  pri.cancel<-tkbutton(tt,text="Cancel", command = function()tkdestroy(tt))
  
  tkgrid(prihelp, row = 1, sticky = "nw")
  tkgrid(tkframe(pri.status, height = 0, width = 450), row = 2)
  tkgrid(empri.label, empri.ent,row = 1, padx = 5, pady = 5) 
  tkgrid(tklabel(priors, text = paste("(N for current dataset is ",
                           nrow(getAmelia("amelia.data")),")",sep="")), row = 2)
  tkgrid(data.but, row = 1, padx = 5, pady = 5, sticky = "ew")
	
	
  
  
  
  tkgrid(pri.link, sticky = "ne", row = 1, column = 2, padx = 2, pady = 2)
  tkgrid(priors, sticky = "nsew", row = 2, column = 0, columnspan = 3, padx = 10, pady = 10)
  tkgrid(gui.frame, sticky = "nw", row = 4, column = 0, padx = 5, pady = 5)
  tkgrid(pri.ok, row = 5, column = 1, sticky = "ew", padx = 5, pady = 5)
  tkgrid(pri.cancel, row = 5, column = 2, sticky = "ew", padx = 5, pady = 5)
  tkgrid(pri.status, sticky="sew", row = 6,  columnspan = 3)
  tkgrid(tklabel(tt, text="Priors Options", font="Arial 16 bold"),pady=5, row = 1, column = 0, columnspan = 2,sticky="w")
  tkfocus(tt)
  tkgrab.set(tt)
  tkbind(tt,"<Destroy>",function(){tkgrab.release(tt);tkfocus(gui)})
 	tkbind(empri.ent, "<Motion>","set prihelp \"Ridge prior that shrinks the covariances. 5 percent of the number of obs. is a useful default.\"")
  tkbind(empri.ent, "<Leave>","set prihelp \"\"")
 	tkbind(empri.label, "<Motion>","set prihelp \"Ridge prior that shrinks the covariances. 5 percent of the number of obs. is a useful default.\"")
  tkbind(empri.label, "<Leave>","set prihelp \"\"")
 ## 	tkbind(case.but, "<Motion>","set prihelp \"Set prior beliefs about the similarity between different cross-sectional units.\"")
##   tkbind(case.but, "<Leave>","set prihelp \"\"")
 	tkbind(data.but, "<Motion>","set prihelp \"Set priors about individual observations or whole variables.\"")
  tkbind(data.but, "<Leave>","set prihelp \"\"")                
}

gui.diag.setup <- function() {

  select.var <- function(varnum) {
    varnum2<-tknearest(diag.list, varnum)
    putAmelia("diag.sel.var",as.numeric(varnum2) + 1)
    putAmelia("diag.var.name",getAmelia("varnames")[getAmelia("diag.sel.var")])
    if (!is.numeric(getAmelia("amelia.data")[,getAmelia("diag.sel.var")]))
      tkconfigure(diag.but.compare, state="disabled")
    else 
      tkconfigure(diag.but.compare, state="active")
  }
  
  tt<-tktoplevel()
  tkwm.title(tt, "Diagnostics")
  diag.var<-tkframe(tt, relief = "groove", borderwidth = 2)

  if(!exists("diag.sel.var")) {
    putAmelia("diag.sel.var",1)
    putAmelia("diag.var.name",getAmelia("varnames")[getAmelia("diag.sel.var")])
  }
	#Listbox for variables.
  scr<-tkscrollbar(diag.var, repeatinterval = 5,
                   command = function(...) tkyview(diag.list,...))
  diag.list<-tklistbox(diag.var,height=10,selectmode="single",
                       yscrollcommand = function(...) tkset(scr,...))
  for (i in 1:ncol(getAmelia("amelia.data")))
    tkinsert(diag.list,"end",names(getAmelia("amelia.data"))[i])
  tkselection.set(diag.list,(getAmelia("diag.sel.var")-1))
  tkbind(diag.list,"<Button-1>",function(y)select.var(y))
  diag.but.compare <- tkbutton(diag.var, text="Compare",
    command = function() compare.density(data=getAmelia("amelia.data"),
                                         output=getAmelia(tclvalue(getAmelia("outname"))),
                                         var=getAmelia("diag.sel.var"),frontend=TRUE))
  if (is.factor(getAmelia("amelia.data")[,getAmelia("diag.sel.var")]))
    tkconfigure(diag.but.compare, state="disabled")
  diag.overimp <- tkbutton(diag.var,text="Overimpute",state="normal",
    command = function() overimpute(data=getAmelia("amelia.data"),
                                    output=getAmelia(tclvalue(getAmelia("outname"))),
                                    var=getAmelia("diag.sel.var"),frontend=TRUE))
  diag.disp<-tkframe(tt, relief = "groove", borderwidth = 2)
  dimvalue<-tclVar("1")
  onedim<-tkradiobutton(diag.disp, variable=dimvalue, value="1")
  twodims<-tkradiobutton(diag.disp, variable=dimvalue, value="2")
  disp.imps.tcl<-tclVar("5")
  disp.imps<-tkentry(diag.disp,width="5",textvariable=disp.imps.tcl)
  disp.but<-tkbutton(diag.disp,text="Overdisperse",state="normal",
    command = function() disperse(data=getAmelia("amelia.data"),m=as.numeric(tclvalue(disp.imps.tcl)),
    dims=as.numeric(tclvalue(dimvalue)),frontend=TRUE,output=getAmelia(tclvalue(getAmelia("outname")))))
  tkgrid(tklabel(diag.disp, text="Overdispersed Starting Values", 
    font="Arial 12 bold"),row=1, column=1, columnspan=2,padx=3,pady=5)
  tkgrid(tklabel(diag.disp,text="Number of dispersions: "),row=2,column=1,
    sticky="nw")
  tkgrid(disp.imps,column=2,row=2,sticky="nw")
  tkgrid(tklabel(diag.disp,text="One Dimension"),row=3,column=1)
  tkgrid(tklabel(diag.disp,text="Two Dimensions"),row=4,column=1)
  tkgrid(onedim,row=3,column=2)
  tkgrid(twodims,row=4,column=2)
  tkgrid(disp.but,row=5,column=2,padx=2,pady=3,sticky="news")
  
  
  
  diag.link <- tkbutton(tt, text = "?", 
    command = function()browseURL("http://gking.harvard.edu/amelia/docs/Diagnostics_Dialog.html"))
  diag.close<-tkbutton(tt, text="Close",command=function()tkdestroy(tt))
  
  tcl("set","diaghelp","")
  diag.status <- tkframe(tt, relief = "groove", borderwidth = 2)
  diaghelp<-tklabel(diag.status, textvariable="diaghelp", font=getAmelia("helpfont"))

  tkgrid(diaghelp, row = 1, sticky = "nw")
  tkgrid(tkframe(diag.status, height = 0, width = 320), row = 2)
  tkgrid(diag.list, row = 1, column = 1, rowspan = 2,pady=10)
  tkgrid(scr, row = 1, column = 2,pady=10)
  tkgrid.configure(scr,rowspan=4,sticky="nsw")
  tkgrid(diag.but.compare, row = 1, column = 3, padx = 10, pady = 5,sticky="ew" )
  tkgrid(diag.overimp, row=2,column=3,padx=10,pady=5,sticky="ew")
  tkgrid(diag.var, row = 2,column=1, padx = 5, pady = 5, columnspan = 3,sticky="news")
  tkgrid(diag.disp, row = 2, column=4, padx = 5, pady = 5, columnspan = 3,sticky="news")
  tkgrid(diag.close,row = 4, column = 6, sticky = "news", padx = 5, pady = 5)
  tkgrid(diag.status, row = 5,column=1, columnspan = 6,sticky="sew")
  tkgrid(diag.link, row = 1, column=6,sticky="e")
  tkgrid(tklabel(tt, text="Diagnostics", font="Arial 16 bold"),pady=5, row = 1, column = 1, columnspan = 3,sticky="w")
  tkfocus(tt)
  tkgrab.set(tt)
  tkbind(tt,"<Destroy>", function() {tkgrab.release(tt);tkfocus(gui)})
 	tkbind(diag.but.compare, "<Motion>","set diaghelp \"Compare densities of the imputed values vs. observed values.\"")
  tkbind(diag.but.compare, "<Leave>","set diaghelp \"\"")
 	tkbind(diag.overimp, "<Motion>","set diaghelp \"Overimpute and graph confidence intervals.\"")
  tkbind(diag.overimp, "<Leave>","set diaghelp \"\"")
}
#tkwm.iconbitmap(gui,"~/amelia/setup/files/amelia.ico")
tkwm.deiconify(gui)
tkwait.window(gui)

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

putAmelia <- function(x, value)
    assign(x, value, envir = ameliaEnv())

getAmelia <- function(x, mode="any")
    get(x, envir = ameliaEnv(), mode = mode, inherits = FALSE)
    
ameliaTclSet <- function(name, value){
    name <- ls(unclass(getAmelia(name))$env)
    tcl("set", name, value)
    }
