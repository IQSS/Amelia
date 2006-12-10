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
##  1/12/06 mb - fixed session loading for 2.4.0, can't compare non-numerics


AmeliaView<-function() {

main.close<-function() {
	qvalue<-tkmessageBox(message="Are you sure you want to exit Amelia?",
		icon="question",
		type="okcancel",
		default="cancel")
	if (tclvalue(qvalue)=="ok") {
		tkdestroy(gui)
	}
}


file.type<-function(...) {
  drop.select<<-as.numeric(tkcurselection(input.drop.box))
  if (as.numeric(drop.select)==0)
    filetype<-"{{Comma-delimited files} {.csv}} {{All files} *} "
  if (as.numeric(drop.select)==1)
    filetype<-"{{Tab Delimited} {.txt}} {{All files} *}"
  if (as.numeric(drop.select)==2)
    filetype<-"{{Stata files} {.dta}} {{All files} *}"
  if (as.numeric(drop.select)==3)
    filetype<-"{{SPSS} {.sav}} {{All files} *}"
  if (as.numeric(drop.select)==4)
    filetype<-"{{All files} *}"
  if (as.numeric(drop.select)==5)
    filetype<-"{{RData} {.RData}} {{All files} *}"
  file.kind<<-filetype
}

get.filename<-function(entry) {
  am.filename<<-tclvalue(tkgetOpenFile(filetypes =file.kind))
  tkdelete(input.entry,0,"end")
  tkinsert(input.entry,"end",am.filename)
}

load.data <- function(session=F) {
  if (!session) {
    if (!is.null(amelia.data)) {
      sure<-tkmessageBox(message="If you load another dataset, your current settings will be erased.  Are you sure you want to load the new data?",icon="question",type="yesno")    
      if (tclvalue(sure) == "no") 
        return(NULL)
    }
  } 
  if (is.null(am.filename))
    am.filename <<- tclvalue(inname)
  if (!file.exists(am.filename)) {
    tkmessageBox(message="The file doesn't exist.  Try Again.",icon="error",type="ok")
    return(NULL)
  }
  if (drop.select == 0)
    amelia.data<<-try(read.csv(am.filename,header=T))
  if (drop.select == 1)
    amelia.data<<-try(read.table(am.filename,header=T))
  if (drop.select == 2)
    amelia.data<<-try(read.dta(am.filename,convert.factors=F))
  if (drop.select == 3)
    amelia.data<<-try(read.spss(am.filename,use.value.labels=F,to.data.frame=TRUE))
  if (drop.select == 4)
    amelia.data<<-try(read.xport(am.filename))
  if (drop.select == 5)
    amelia.data<<-try(load(am.filename))
  filetype.sess <<- drop.select
  if (inherits(amelia.data, "try-error")) {
    tkmessageBox(message="Failure in loading the data.  Try again.",icon="error",type="ok")
    return(NULL)
  }
  temp.list<- strsplit(am.filename,"/")
  am.directory <<- temp.list[[1]][1]
  for (i in 2:(length(temp.list[[1]])-1))
    am.directory <<- paste(am.directory, temp.list[[1]][i], sep = "/")
  setwd(am.directory)
  varnames<<-(c("(none)",names(amelia.data)))
  tcl("set","varnames",varnames)
  tkconfigure(options.var, state = "normal")
  tkconfigure(options.tsvar, state = "normal",listvar="varnames")
  tkconfigure(options.csvar, state = "normal",listvar="varnames")
  tkconfigure(options.priors, state = "active")
  tkconfigure(statusbar.lab1b, text = am.filename, foreground = "blue")
  tkconfigure(statusbar.n, text = paste(nrow(amelia.data)), foreground = "blue")
  tkconfigure(statusbar.k, text = paste(ncol(amelia.data)), foreground = "blue")
  tkconfigure(output.run, state = "active")
  tkconfigure(input.see, state = "active")
  tsvar<<-0
  csvar<<-0
  tkselect(options.tsvar,tsvar)
  tkselect(options.csvar,csvar)
  tkconfigure(options.tsvar,command=function(...)set.tsvar())
  tkconfigure(options.csvar,command=function(...)set.csvar())
  transmat<<-matrix(0,nrow=ncol(amelia.data),ncol=1)
  amelia.lags<<-matrix(0,nrow=ncol(amelia.data),ncol=1)
  future.lags<<-matrix(0,nrow=ncol(amelia.data),ncol=1)
  num.poly<<-tclVar("0")
  intercs<<-tclVar("0")
  store.pri <<- NULL
  obs.prior <<- NULL
  r<<-NULL
  priorsmat <<- NULL
  outname <<- tclVar("outdata")
  outnum <<- tclVar("5")
  empri <<- tclVar("0")
  tkconfigure(options.tscs, state = "disabled")
  call.flag<<-NULL
  varmin<<-NULL
  varmax<<-NULL
  tclvarmin<<-list()
  tclvarmax<<-list()
  tkconfigure(output.entry, textvariable=outname)
  tkconfigure(output.num, textvariable=outnum)
  #tkconfigure(input.load,state="disabled",text="Data Loaded")
  
}

set.tsvar<-function() {
  tsvar<<-as.numeric(tkcurselection(options.tsvar))
  if (tsvar != 0)
    tkconfigure(options.tscs,state="active")
  call.flag<<-NULL
}

set.csvar<-function() {
  csvar<<-as.numeric(tkcurselection(options.csvar))
  call.flag<<-NULL
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
  if (is.null(amelia.data)) {
    tkmessageBox(message="You must load a dataset before you can save a session.", icon="error", type="ok")  
    return(NULL)
  }  
  file.select <- tclvalue(tkgetSaveFile(filetypes="{{R files} {.R}} {{All files} *}"))
#  if (exists("amelia.list")) {
#    amelia.list$amelia.args$am.filename <- am.filename
#    amelia.list$amelia.args$varmin <- varmin
#    amelia.list$amelia.args$varmax <- varmax
#    amelia.list$amelia.args$output.select <- output.select
#    dump("amelia.list", file.select)
#    return(NULL)
#  } 
  amelia.list<-list()
	outname1 <- tclvalue(outname)
	outnum1 <- as.numeric(tclvalue(outnum))
	empri1 <- as.numeric(tclvalue(empri))
	amelia.list$amelia.args<-list()
	amelia.list$amelia.args$outname <- outname1
	amelia.list$amelia.args$m <- outnum1
	amelia.list$amelia.args$empri <- empri1
	amelia.list$amelia.args$ts <- tsvar
	amelia.list$amelia.args$cs <- csvar
	amelia.list$amelia.args$am.filename <- am.filename
	amelia.list$amelia.args$file.type <- filetype.sess
  amelia.list$amelia.args$idvars <- c()
	amelia.list$amelia.args$ords <- c()
	amelia.list$amelia.args$noms <- c()
	amelia.list$amelia.args$logs <- c()
	amelia.list$amelia.args$sqrts <- c()
	amelia.list$amelia.args$lgstc <- c()
	amelia.list$amelia.args$lags <- c()
	amelia.list$amelia.args$leads <- c()
  
  for (i in 1:length(transmat)) {
    if (transmat[i] == 6)
      amelia.list$amelia.args$idvars <- c(amelia.list$amelia.args$idvars,i)
    if (transmat[i] == 1)
      amelia.list$amelia.args$ords <- c(amelia.list$amelia.args$ords,i)
    if (transmat[i] == 2)
      amelia.list$amelia.args$noms <- c(amelia.list$amelia.args$noms,i)
    if (transmat[i] == 3)
      amelia.list$amelia.args$logs <- c(amelia.list$amelia.args$logs,i)
    if (transmat[i] == 4)
      amelia.list$amelia.args$sqrts <- c(amelia.list$amelia.args$sqrts,i)
    if (transmat[i] == 5)
      amelia.list$amelia.args$lgstc <- c(amelia.list$amelia.args$lgstc,i)      
    if (amelia.lags[[i]] == 1)
      amelia.list$amelia.args$lags<-c(amelia.list$amelia.args$lags,i)
    if (future.lags[[i]] == 1)
      amelia.list$amelia.args$leads <- c(amelia.list$amelia.args$leads, i)
  }
  
  amelia.list$amelia.args$casepri <- store.pri
	amelia.list$amelia.args$polytime <- as.numeric(tclvalue(num.poly))
	amelia.list$amelia.args$intercs <- as.numeric(tclvalue(intercs))

	amelia.list$amelia.args$output.select <- output.select
  amelia.list$amelia.args$varmin <- varmin
  amelia.list$amelia.args$varmax <- varmax
  amelia.list$amelia.args$mins <- matrix(NA, nrow = nrow(amelia.data), ncol = ncol(amelia.data))
  amelia.list$amelia.args$maxs <- matrix(NA, nrow = nrow(amelia.data), ncol = ncol(amelia.data))
  amelia.list$amelia.args$conf <- matrix(NA, nrow = nrow(amelia.data), ncol = ncol(amelia.data))
  amelia.list$amelia.args$means <- matrix(NA, nrow = nrow(amelia.data), ncol = ncol(amelia.data))
  amelia.list$amelia.args$sds <- matrix(NA, nrow = nrow(amelia.data), ncol = ncol(amelia.data))
  
  if (all(!is.null(varmin),!is.null(varmax))) {
    for (i in 1:ncol(amelia.data)) {
      if (all(!is.na(varmin[i]),!is.na(varmax[i]))) {
        amelia.list$amelia.args$mins[,i]<-varmin[i]
        amelia.list$amelia.args$maxs[,i]<-varmax[i]
        amelia.list$amelia.args$conf[,i]<-.95
      }
    }
  }
  if (!is.null(r)) {  
    for (i in 1:nrow(amelia.data)) {
      for (j in 1:ncol(amelia.data))  {
        if (all(r[i,j] != "MISS",r[i,j] != "--",j!=tsvar,j!=csvar)) {
          numholder <- strsplit(r[i,j],split = ",")
          if (length(numholder[[1]]) == 3) {
            amelia.list$amelia.args$mins[i,j] <- numholder[[1]][1]
            amelia.list$amelia.args$maxs[i,j] <- numholder[[1]][2]
            amelia.list$amelia.args$conf[i,j] <- numholder[[1]][3]
          } else {
            if (length(numholder[[1]]) == 2) {
              amelia.list$amelia.args$means[i,j] <- numholder[[1]][1]
              amelia.list$amelia.args$sds[i,j] <- numholder[[1]][2]
            } else {
              obserror <- tkmessageBox(message="There is an error in the observational priors.  Would like to save without them?.",icon="error",type="yesno")
              if (tclvalue(obserror) == "no") {
                tkmessageBox(message="Your session has not been saved.", icon="error", type="ok")
                return(NULL)
              }
            }
          }
        }
      }
    }
    storage.mode(amelia.list$amelia.args$mins) <- "numeric"
    storage.mode(amelia.list$amelia.args$maxs) <- "numeric"
    storage.mode(amelia.list$amelia.args$conf) <- "numeric"
    storage.mode(amelia.list$amelia.args$means) <- "numeric"
    storage.mode(amelia.list$amelia.args$sds) <- "numeric"
  }
  
  if (all(is.na(amelia.list$amelia.args$mins)))
    amelia.list$amelia.args$mins<-NULL
  if (all(is.na(amelia.list$amelia.args$maxs)))
    amelia.list$amelia.args$maxs<-NULL
  if (all(is.na(amelia.list$amelia.args$conf)))
    amelia.list$amelia.args$conf<-NULL
  if (all(is.na(amelia.list$amelia.args$means)))
    amelia.list$amelia.args$means<-NULL
  if (all(is.na(amelia.list$amelia.args$sds)))
    amelia.list$amelia.args$sds<-NULL
  
	dump("amelia.list", file.select)
	return(NULL)
}

load.session <- function() { 
  file.select <- tclvalue(tkgetOpenFile(filetypes=
          "{{R files} {.R}} {{All files} *}"))
	if (nchar(file.select) <= 0)
    return(NULL)
  session.test <- file(file.select)
  first.line <- readLines(session.test)[1]
  close(session.test)
  
	if (first.line != "\"amelia.list\" <-" && first.line != "`amelia.list` <-") {
    tkmessageBox(message="Not an Amelia session file.  Try again.",icon="error",type="ok")
    return(NULL)
  }
  trysession<-try(source(file = file.select),silent=T)
 	
  if (inherits(trysession,"try-error")) {
    tkmessageBox(message="Error loading session.  This is not a valid session file.",icon="error",type="ok")
    return(NULL)
  }
  if (!file.exists(amelia.list$amelia.args$am.filename)) {
    tkmessageBox(message=paste("Dataset file not found at:",amelia.list$amelia.args$am.filename,"Cannot load session.",sep="\n"),icon="error",type="ok")
    return(NULL)
  }
  drop.select <<- amelia.list$amelia.args$file.type 
  tclvalue(inname)<<-amelia.list$amelia.args$am.filename
  am.filename <<- amelia.list$amelia.args$am.filename  
  load.data(session=T)
  tsvar <<- amelia.list$amelia.args$ts
  csvar <<- amelia.list$amelia.args$cs
  for (i in amelia.list$amelia.args$idvars)
    transmat[i]<<-6
  for (i in amelia.list$amelia.args$logs)
    transmat[i]<<-3  
  for (i in amelia.list$amelia.args$ords)
    transmat[i]<<-1
  for (i in amelia.list$amelia.args$noms)
    transmat[i]<<-2
  for (i in amelia.list$amelia.args$sqrts)
    transmat[i]<<-4
  for (i in amelia.list$amelia.args$lgstc)
    transmat[i]<<-5
  for (i in amelia.list$amelia.args$lags)
    amelia.lags[[i]] <<- 1
  for (i in amelia.list$amelia.args$leads)
    future.lags[[i]] <<- 1

  num.poly <<- tclVar(amelia.list$amelia.args$polytime)
  intercs <<- tclVar(amelia.list$amelia.args$intercs)
      
  empri <<- tclVar(amelia.list$amelia.args$empri)
  store.pri <<- amelia.list$amelia.args$casepri
  r <<- matrix(NA,nrow=nrow(amelia.data),ncol=ncol(amelia.data))
  for (i in 1:nrow(amelia.data)) {
    for (j in 1:ncol(amelia.data)) {
      if (!is.na(amelia.data[i,j])) {
        r[i,j] <<- "--"
      } else {
        r[i,j] <<- "MISS"
        if (!is.null(amelia.list$amelia.args$means))
          if (!is.na(amelia.list$amelia.args$means[i,j]))
            r[i,j] <<- paste(amelia.list$amelia.args$means[i,j],amelia.list$amelia.args$sds[i,j],sep=",")
        if (!is.null(amelia.list$amelia.args$mins))
          if (!is.na(amelia.list$amelia.args$mins[i,j]))
            r[i,j] <<- paste(amelia.list$amelia.args$mins[i,j],amelia.list$amelia.args$maxs[i,j],amelia.list$amelia.args$conf[i,j],sep=",") 
      }
    }
  }
  colnames(r)<<-colnames(amelia.data)
  rownames(r)<<-rownames(amelia.data)
  if ((tsvar != 0) && (csvar != 0)) {
    tkconfigure(options.tscs,state="active")
  }
  tkselect(options.tsvar,tsvar)
  tkselect(options.csvar,csvar)
  set.tsvar()
  set.csvar()
  if (!is.null(amelia.list$amelia.args$casepri))
    tkconfigure(options.csvar, state="disabled")
  output.select <<- amelia.list$amelia.args$output.select
  tkselect(output.drop.box, output.select)
  outname<<-tclVar(amelia.list$amelia.args$outname)
  outnum<<-tclVar(amelia.list$amelia.args$m)
  tkdelete(output.entry,0,"end")
  tkinsert(output.entry,"end",tclvalue(outname))
  tkdelete(output.num,0,"end")
  tkinsert(output.num,"end", tclvalue(outnum))
  if (names(amelia.list)[1] == "m1")
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

  intercs<-as.logical(as.numeric(tclvalue(intercs)))
  num.poly<-as.numeric(tclvalue(num.poly))
  
 if (num.poly == 0)
    if (intercs == FALSE)
      num.poly<-NULL
 if (tsvar == 0) {
    num.poly<-NULL
    tsvar <-NULL
  }
  if (csvar == 0) {
    csvar <-NULL
    intercs<-FALSE
  }
 
  for (i in 1:ncol(amelia.data))
    if (transmat[i] == 6)
      id <- c(id,i)
  for (i in 1:ncol(amelia.data))
    if (transmat[i] == 1)
      ord <- c(ord,i)
  for (i in 1:ncol(amelia.data))
    if (transmat[i] == 2)
      nom <- c(nom,i)
  for (i in 1:ncol(amelia.data))
    if (transmat[i] == 3)
      logs <- c(logs,i)
  for (i in 1:ncol(amelia.data))
    if (transmat[i] == 4)
      sqrts <- c(sqrts,i)
  for (i in 1:ncol(amelia.data))
    if (transmat[i] == 5)
      lgstc <- c(lgstc,i)      

  


  for (i in 1:ncol(amelia.data)) {
    if (amelia.lags[[i]] == 1)
      amlags<-c(amlags,i)
    if (future.lags[[i]] == 1)
      amfut <- c(amfut, i)
  }


  colnames(priorsmat)<<-NULL
  rownames(priorsmat)<<-NULL

  amelia.list <<- try(amelia(amelia.data, m = as.numeric(tclvalue(outnum)),
        p2s = FALSE, idvars = id, casepri = store.pri, ts = tsvar, cs = csvar,
        priors=priorsmat, lags = amlags,
        empri = as.numeric(tclvalue(empri)), intercs = intercs, leads = amfut, 
        polytime = num.poly, frontend=TRUE, logs=logs, sqrts=sqrts, lgstc=lgstc,
        ords=ord,noms=nom,write.out=F,tolerance=as.numeric(tclvalue(tol))),silent=T)

  
  if (inherits(amelia.list,"try-error")) {
    tkinsert(run.text,"end","\nThere was an unexpected error in the execution of Amelia.  \nDouble check all inputs for errors and take note of the error message:\n\n")
    tkinsert(run.text,"end",paste(amelia.list))
    return(NULL)
  }
  
  if (amelia.list$code!=1) {
    tkinsert(run.text,"end","\n")
    tkinsert(run.text,"end",paste("Amelia Error Code:",amelia.list[[1]],"\n",amelia.list[[2]]))
    tkinsert(run.text,"end","\n\nYou have recieved an error.  You can close this window and reset\n various options to correct the error.")
  }
  else {
    tkinsert(run.text,"end","\nAmelia has run successfully.  You can close this window and \nuse the diagnostics button to see how your imputations ran.")
    tkconfigure(output.diag, state = "active")
    tksee(run.text,"end")
    amelia.save(amelia.list,tclvalue(outname),as.numeric(tclvalue(outnum)))
  }
}

amelia.save <- function(out,outname,m)  {
  if (output.select == 1)
    for (i in 1:m) 
      write.csv(out[[i]],file=paste(am.directory,"/",outname,i,".csv",sep=""),row.names=FALSE)
  if (output.select == 2)
    for (i in 1:m) 
      write.table(out[[i]],file=paste(am.directory,"/",outname,i,".txt",sep=""),sep="\t",row.names=FALSE)
  if (output.select == 3)
    for (i in 1:m) 
      write.dta(out[[i]],file=paste(am.directory,"/",outname,i,".dta",sep=""),version=6)
  if (output.select == 3)
    for (i in 1:m) 
      write.dta(out[[i]],file=paste(am.directory,"/",outname,i,".dta",sep=""),version=7)
}

set.out<-function(...) {
  output.select<<-as.numeric(tkcurselection(output.drop.box))
}

#Preamble
require(tcltk) || stop("The package 'tcltk' is required")
require(foreign)
libdir<-file.path(.find.package(package = "Amelia")[1], "tklibs")
addTclPath(libdir)
tclRequire("combobox")
.Tcl("catch {namespace import ::combobox::*}")
tclRequire("BWidget")
if (.Platform$OS.type == "windows") 
  tclRequire("Tktable")

outname <<- tclVar("outdata")
outnum <<- tclVar("5")
empri <<- tclVar("0")
tol<<-tclVar("0.0001")
amelia.data <<- NULL
am.filename <<- NULL
varnames <<- NULL
tsvar <<- NULL
csvar <<- NULL
varmin <<- NULL
varmax <<- NULL
inname <<- tclVar("")


gui<-tktoplevel()
tkwm.title(gui, "AmeliaView")
                         
##Menu
main.menu<-tkmenu(gui)
main.menu.file<-tkmenu(main.menu, tearoff=0)
main.menu.help<-tkmenu(main.menu, tearoff=0)
tkadd(main.menu.file,"command",label="Load Session",command=function()load.session())
tkadd(main.menu.file,"command",label="Save Session",command=function()save.session())
tkadd(main.menu.file,"command",label="ExitAmelia",command=function()main.close())
tkadd(main.menu.help,"command",label="Amelia Website",command=
      function()browseURL("http://gking.harvard.edu/amelia/"))
tkadd(main.menu.help,"command",label="Online Documentation",command=
      function()browseURL("http://gking.harvard.edu/amelia/docs/"))

tkadd(main.menu.help,"command",label="About",command=
      function()tkmessageBox(title="AmeliaView",message="James Honaker, Gary King, & Matthew Blackwell 2006",icon="info",type="ok"))
tkadd(main.menu,"cascade",label="File",menu=main.menu.file)
tkadd(main.menu,"cascade",label="Help",menu=main.menu.help)
tkconfigure(gui,menu=main.menu)

##Frame
gui.skel<-tkframe(gui)
main.title<-tklabel(gui, text="AmeliaView",font=c("Arial",22))

tkgrid(main.title,sticky = "ew")

gui.input<-tkframe(gui.skel, relief = "groove", borderwidth = 2)
gui.options<-tkframe(gui.skel, relief = "groove", borderwidth = 2)
gui.output<-tkframe(gui.skel, relief = "groove", borderwidth = 2)

if (.Platform$OS.type == "windows") {
  padding<-5
  helpfont<<-c("arial",8)
} else {
  helpfont<<-c("arial",10)
  padding<-3
}

##Input Frame
##Data management,loading, etc.

#Data type combobox
drop.select<<-0
file.kind<<-"{{Comma-delimited files} {.csv}} {{All files} *} "
file.types<-c("CSV","Tab Delimited","Stata","SPSS","SAS Transport") #,"RData")
tcl("set", "filetypes",file.types)
input.drop.label<-tklabel(gui.input,text="Input Data Format:")

input.drop.box<-tkwidget(gui.input,"combobox",borderwidth=1,
    editable="FALSE",listvar= "filetypes",width=15)
tkselect(input.drop.box,drop.select)
tkconfigure(input.drop.box,command=function(...)file.type())


#Data entry and Load
input.entry <- tkentry(gui.input, width="45", textvariable = inname)
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
if (is.null(varnames)) {
  varnames<<-"  .     .       .       .   .     .   .  ..   .  . . . . . . "
  tcl("set","varnames",varnames)
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
options.tscs<<-tkbutton(gui.options, text = "TSCS", state = "disabled", width = 10,
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

output.select<<-1
output.types<-c("(no save)","CSV","Tab Delimited","Stata 6","Stata 7/8")
tcl("set", "outtypes",output.types)
output.drop.label<-tklabel(gui.output,text="Output Data Format:")

output.drop.box<-tkwidget(gui.output,"combobox",borderwidth=1,
    editable="FALSE",listvar= "outtypes",width=13)
tkselect(output.drop.box,output.select)
tkconfigure(output.drop.box,command = function(...)set.out())


#output options
output.label <- tklabel(gui.output,text="Name the Imputed Dataset")
output.entry <- tkentry(gui.output,width="15",textvariable=outname)
output.numlab <- tklabel(gui.output, text = "Number of Imputed Datasets:")
output.num <- tkentry(gui.output, width = "5", textvariable = outnum)

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
tkgrid(output.num, row = 4, column = 2, sticky = "w", padx = padding, pady = padding)
tkgrid(output.run, row = 5, column = 1, sticky = "e",  padx = padding, pady = padding)
tkgrid(output.diag, row = 5, sticky = "w", column = 3, padx = padding, pady = padding)
tkgrid.rowconfigure(gui.output, 1, weight = 1)
tkgrid.rowconfigure(gui.output, 2, weight = 1)
tkgrid.rowconfigure(gui.output, 3, weight = 1)
tkgrid.rowconfigure(gui.output, 4, weight = 1)
tkgrid.rowconfigure(gui.output, 5, weight = 1)
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
help.lab<-tklabel(gui,textvariable="helpvar", font=helpfont)
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
	select.var<-function(varnum) {
		temp.trans[[selected.var]]<<-as.numeric(tclvalue(trans))
		temp.id[[selected.var]]<<-as.numeric(tclvalue(idvar))
		varnum2<-tknearest(var.list, varnum)
		selected.var<<-as.numeric(varnum2) + 1
		selected.var.name<<-names(amelia.data)[selected.var]
		idvar<<-tclVar(temp.id[[selected.var]])
		trans<<-tclVar(temp.trans[[selected.var]])
		tkconfigure(var.opt.name,text=selected.var.name)
		tkconfigure(var.opt.ord, variable = trans)
		tkconfigure(var.opt.nom, variable = trans)
		tkconfigure(var.opt.none, variable = trans)
		tkconfigure(var.opt.lgln, variable = trans)
		tkconfigure(var.opt.sqrt, variable = trans)
		tkconfigure(var.opt.lgstc, variable = trans)
		tkconfigure(var.opt.id, variable = idvar)
	}
  var.save <- function() {
    transmat<<-temp.trans
    tol<<-tclVar(tclvalue(temptol))
    tkdestroy(tt)
  }
  set.trans <- function(index, ind) {
    transind <- which(index==ind,arr.ind=TRUE)
    temp.trans[transind[1],] <<- transind[2]-1
  }
    


  temp.trans<<-transmat

  var.link <-tkbutton(tt, text = "?", 
    command = function()browseURL("http://gking.harvard.edu/amelia/docs/Variables_Dialog.html"))
	
  #ScrollableFrame for variable options
  sw <- tkwidget(tt,"ScrolledWindow",relief="sunken",borderwidth=2)
  sf <- tkwidget(sw,"ScrollableFrame")
  tcl(sw,"setwidget",sf)
  subfID <- tclvalue(tcl(sf,"getframe"))
  tcltrans <<- list()
  trans.radio.buts <<- list()
  indexmat<-matrix(1:((ncol(amelia.data))*7),ncol(amelia.data),7)
  transnames<-c("No Transformations","Ordinal","Nominal","Log-Linear","Square Root","Logistic","ID Variable")
  tcl("set","varhelp","")
  for (i in 0:(ncol(amelia.data))) {    
    if (i == 0) {
      tkgrid(tcl("label",paste(subfID,".top",2,sep=""),text="No Transformation"),row=1,column=2)
      tkgrid(tcl("label",paste(subfID,".top",3,sep=""),text="Ordinal"),row=1,column=3)
      tkgrid(tcl("label",paste(subfID,".top",4,sep=""),text="Nominal"),row=1,column=4)
      tkgrid(tcl("label",paste(subfID,".top",5,sep=""),text="Log-Linear"),row=1,column=5)
      tkgrid(tcl("label",paste(subfID,".top",6,sep=""),text="Square Root"),row=1,column=6)
      tkgrid(tcl("label",paste(subfID,".top",7,sep=""),text="Logistic"),row=1,column=7)
      tkgrid(tcl("label",paste(subfID,".top",8,sep=""),text="ID Variable"),row=1,column=8)
    } else {
      tcltrans[[i]]<<-tclVar(init=paste(temp.trans[[i]]))
      for (j in 0:7) {
        if (j == 0) {
          tkgrid(tcl("label",paste(subfID,".left",i,sep=""),text=names(amelia.data)[i]),row=i+1,column=1)
        } else {
          trans.radio.buts[[indexmat[i,j]]]<<-tcl("radiobutton",
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
  temptol<<-tclVar(tclvalue(tol))
  tolerance.box<-tkentry(tt, width= "7", textvariable=temptol)
  var.status<-tkframe(tt, relief = "groove", borderwidth = 3)
  var.help<-tklabel(var.status, textvariable="varhelp", font=helpfont)



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
		sum.var<<-as.numeric(varnum2) + 1
		sum.var.name<<-names(amelia.data)[sum.var]
		tkconfigure(var.sum.name,text=sum.var.name)
		
		if (any(is.factor(amelia.data[,sum.var]),is.character(amelia.data[,sum.var]))) {
		  tkconfigure(var.but.plot, state="disabled")
		  tkconfigure(var.info.min, text = "Min: (is factor)")
      tkconfigure(var.info.max, text = "Max: ...")
      tkconfigure(var.info.mean,text = "Mean: ...")
      tkconfigure(var.info.sd, text = "SD: ...")
      tkconfigure(var.info.miss,text = "Missing: ...")
    }
    else {
      tkconfigure(var.info.mean,text=paste("Mean:",
			 signif(mean(as.numeric(amelia.data[,sum.var]),na.rm=T),digits=4)))
		  tkconfigure(var.info.sd,text=paste("SD: ",
			 signif(sd(as.numeric(amelia.data[,sum.var]),na.rm=T),digits=4)))
		  tkconfigure(var.info.miss,text=paste("Missing: ",nrow(amelia.data)-nrow(as.matrix(packr(as.matrix(amelia.data[,sum.var])))),"/",nrow(amelia.data),sep=""))
      tkconfigure(var.but.plot, state="active")
      tkconfigure(var.info.min, text = paste("Min:", 
        signif(min(amelia.data[,sum.var],na.rm=T),digits = 4)))
      tkconfigure(var.info.max, text = paste("Max:", 
        signif(max(amelia.data[,sum.var],na.rm=T),digits = 4)))
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
    for (i in 1:nrow(amelia.data)) {
      for (j in 1:ncol(amelia.data)) {
        count<-count+1
        if (is.na(amelia.data[i,j]))
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
    hist(amelia.data[,sum.var],main = paste("Histogram of",sum.var.name), 
      ylab = "Frequnecy",xlab = "")
 }
  gui.sum.left<-tkframe(gui.frame, relief = "groove", borderwidth = 2)
  gui.sum.right<-tkframe(gui.frame, relief = "groove", borderwidth = 2)
  
  
  sum.link <-tkbutton(gui.frame, text = "?", 
    command = function()browseURL("http://gking.harvard.edu/amelia/docs/"))
  
  if (!exists("sum.var")) {
    sum.var<<-1
    sum.var.name<<-names(amelia.data)[sum.var]
  }
  scr <- tkscrollbar(gui.sum.left, repeatinterval=5,
				   command=function(...)tkyview(var.list,...))
 	#Listbox for variables.
	var.list.label<-tklabel(gui.sum.left,text="Select a variable:")
	var.list<-tklistbox(gui.sum.left,height=10,selectmode="single",
    yscrollcommand=function(...)tkset(scr,...))
	for (i in 1:ncol(amelia.data))
		tkinsert(var.list,"end",names(amelia.data)[i])
	tkselection.set(var.list,(sum.var-1))
	tkbind(var.list,"<Button-1>",function(y)select.var(y))
	tkbind(var.list,"<Up>",function()select.var(sum.var))
	tkbind(var.list,"<Down>",function()select.var(sum.var-2))
	
	
	var.sum.name <- tklabel(gui.sum.right, text = sum.var.name,
    font = c("arial", 10, "bold"))
  if (!any(is.factor(amelia.data[,sum.var]),is.character(amelia.data[,sum.var]))) {
    var.info.mean<-tklabel(gui.sum.right,text=paste("Mean:",
	  	signif(mean(as.numeric(amelia.data[,sum.var]),na.rm=T),digits=4)))
    var.info.sd<-tklabel(gui.sum.right,text=paste("SD:",
      signif(sd(as.numeric(amelia.data[,sum.var]),na.rm=T),digits=4)))
    var.info.miss<-tklabel(gui.sum.right,text=paste("Missing: ",
      nrow(amelia.data)-nrow(as.matrix(packr(as.matrix(amelia.data[,sum.var])))),"/",nrow(amelia.data),sep=""))
    var.info.min<-tklabel(gui.sum.right, text = paste("Min:", 
      signif(min(amelia.data[,sum.var]),digits = 4)))
    var.info.max<-tklabel(gui.sum.right, text = paste("Max:", 
      signif(max(amelia.data[,sum.var]),digits = 4)))
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
	if (is.factor(amelia.data[,sum.var]))
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
  select.var<-function(varnum) {
    temp.lags[[tscs.var]]<<-as.numeric(tclvalue(lags))
    temp.leads[[tscs.var]]<<-as.numeric(tclvalue(leads))
		varnum2<-tknearest(var.list, varnum)
		tscs.var<<-as.numeric(varnum2) + 1
		tscs.var.name<<-names(amelia.data)[tscs.var]
		lags<<-tclVar(temp.lags[[tscs.var]])
		leads<<-tclVar(temp.leads[[tscs.var]])
		tkconfigure(var.tscs.name,text=tscs.var.name)
		tkconfigure(tscs.lag, variable = lags)
		tkconfigure(tscs.fut, variable = leads)
	}
  tscs.save<-function() {
    temp.lags[[tscs.var]]<<-as.numeric(tclvalue(lags))
    temp.leads[[tscs.var]]<<-as.numeric(tclvalue(leads))
    num.poly<<-temp.num
    intercs<<-temp.inter
    amelia.lags<<-temp.lags
    future.lags<<-temp.leads
    tkdestroy(tt)
  }
  
  
  temp.num<<-tclVar(tclvalue(num.poly))
  temp.inter<<-tclVar(tclvalue(intercs))
  temp.lags<<-amelia.lags
  temp.leads<<-future.lags
  
  if (!exists("tscs.var")) {
    tscs.var<<-1
    tscs.var.name<<-names(amelia.data)[tscs.var] 
  }


  tt<-tktoplevel()
  tkwm.title(tt, "TSCS Options")
  gui.top<-tkframe(tt, relief = "groove", borderwidth = 2)
  gui.bottom1<-tkframe(tt, relief = "groove", borderwidth = 2)
  gui.bottom2<-tkframe(tt, relief = "groove", borderwidth = 2)
  
  lags<<-tclVar(temp.lags[[tscs.var]])
  leads<<-tclVar(temp.leads[[tscs.var]])  
  tscs.link <-tkbutton(tt, text = "?", 
    command = function()browseURL("http://gking.harvard.edu/amelia/docs/Time_Series_Cross.html"))
  
  
  poly.time<-tklabel(gui.top, text="Polynomials of time?")
  poly.time0<-tkradiobutton(gui.top, text = "0", variable = temp.num, 
    value = "0")
  poly.time1<-tkradiobutton(gui.top, text = "1", variable = temp.num, 
    value = "1")
  poly.time2<-tkradiobutton(gui.top, text = "2", variable = temp.num, 
    value = "2")
  poly.time3<-tkradiobutton(gui.top, text = "3", variable = temp.num, 
    value = "3")
  inter.cs<-tkcheckbutton(gui.top, text = "Interact with Cross-Section",
    variable = temp.inter)
  if (csvar == 0)
    tkconfigure(inter.cs, state = "disabled")
 
 	#Listbox for variables.
  scr <- tkscrollbar(gui.bottom1, repeatinterval=5,
    command=function(...)tkyview(var.list,...))
	var.list.label<-tklabel(gui.bottom1,text="Select a variable:")
	var.list<-tklistbox(gui.bottom1,height=10,selectmode="single",
    yscrollcommand=function(...)tkset(scr,...))
	for (i in 1:ncol(amelia.data))
		tkinsert(var.list,"end",names(amelia.data)[i])
	tkselection.set(var.list,(tscs.var-1))
	tkbind(var.list,"<Button-1>",function(y)select.var(y))
	
	var.tscs.label<-tklabel(gui.bottom2,text="Set variable options for:")
	var.tscs.name <- tklabel(gui.bottom2, text = tscs.var.name,
    font = c("arial", 10, "bold"))
  tscs.lag <- tkcheckbutton(gui.bottom2, text = "Include Lag",variable = lags)
  tscs.fut <- tkcheckbutton(gui.bottom2, text = "Include Lead",variable = leads)
  
  tcl("set","tscshelp","")
  tscs.status<-tkframe(tt, relief = "groove", borderwidth = 3, width = 500)
  tscs.help<-tklabel(tscs.status, textvariable="tscshelp", font=helpfont)



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

  case.priors <- function() {
    reset.case <- function() {
      store.pri<<-NULL
      tkdestroy(ss)
      case.priors()
      return(NULL)
    }
    pri.toggle <- function(ii,jj) {
      ind <- ii + sum(seq(from = 1, to = (jj - 1)))
      pre.state <- hold.pri[ii,jj]
      if (pre.state == 3)
        new.state <- 0
      else
        new.state <- pre.state + 1
      tkconfigure(button.list[[ind]], text = new.state,
            background = bground[new.state+1],activebackground=bground[new.state+1])
      hold.pri[ii,jj] <<- new.state
    }
    case.save<-function() {
      store.pri<<-hold.pri
      tkdestroy(ss)
    }
    ss <- tktoplevel()
    tkwm.title(ss, "Case Priors")
    sw <- tkwidget(ss,"ScrolledWindow",relief="sunken",borderwidth=2)
    sf <- tkwidget(sw,"ScrollableFrame")
    tcl(sw,"setwidget",sf)
    subfID <- tclvalue(tcl(sf,"getframe"))
    reset.c<-tkbutton(ss, text = "Reset All", command = function() reset.case(), width = 10)
    case.ok<-tkbutton(ss, text = "OK", command = function() case.save(), width = 10)
    case.can<-tkbutton(ss, text = "Cancel", command = function() tkdestroy(ss), width = 10)
    legend<-tkframe(ss, relief = "groove", borderwidth = 2)
    grey.zero<-tklabel(legend, text = "0 - No relationship", bg = "grey")
    green.one<-tklabel(legend, text = "1 - Slightly similar", bg = "green")
    yellow.two<-tklabel(legend, text = "2 - Midly similar", bg = "yellow")
    red.three<-tklabel(legend, text = "3 - Highly similar", bg = "red")
    tkgrid(grey.zero, green.one, yellow.two, red.three, padx = 5, pady = 5)
    caselist <- unique(amelia.data[,csvar])
    casenum <- nrow(as.matrix(caselist))
    if (is.null(store.pri))
      store.pri <<- matrix(0,nrow = casenum, ncol = casenum)
    hold.pri<<-store.pri
    bground <<- c("grey","green", "yellow", "red")
    leg <<- matrix(0,nrow = casenum, ncol= casenum)
    button.list <- list()
    tkgrid(legend, row = 1, columnspan = 5, padx = 3, pady = 3) 
    tkgrid(case.ok, padx = 3, pady = 3, column = 1, sticky = "es", row = 3)
    tkgrid(case.can, padx = 3, pady = 3, column = 2, sticky = "es", row = 3)
    tkgrid(reset.c, padx = 3, pady = 3, column = 3, sticky = "es", row = 3)
    tkconfigure(options.csvar, state="disabled")
    for (i in 1:casenum) {
      if (i !=1)
        if (storage.mode(caselist) == "character")  {
          tkgrid(tcl("label",paste(subfID,".top",i,sep=""), 
            text = substr(as.list(caselist)[[i]],1,3), font=c("Courier",10)), row = 1, column = (i + 1))
        }
        else {
          tkgrid(tcl("label",paste(subfID,".top",i,sep=""), 
            text = substr(as.character(as.list(caselist)[[i]]),1,3), font=c("Courier",10)), row = 1, column = (i + 1))
        }
        tkgrid(tcl("label",paste(subfID,".left",i,sep=""), 
          text = as.list(caselist)[[i]], font = c("Courier",10)), row = (i+1), column = 1,
          columnspan=(i+1),sticky="e")
    }
    for (i in 1:casenum) {
      for (j in 1:casenum) {
        if (i > j)
          next
        if ((i == 1) && (j == 1))  {
          ind <<- 1
          leg[i,j] <- 1
        }
        else {
          ind <<- i + sum(seq(from = 1, to = (j - 1)))
          leg[i,j] <- i + sum(seq(from = 1, to = (j - 1)))
        }
        button.list[[ind]] <- tcl("button",paste(subfID,".but",ind,sep=""), 
          text = paste(hold.pri[i,j]), background = bground[hold.pri[i,j]+1], activebackground=bground[hold.pri[i,j]+1])
        if (i != j)
          tkgrid(button.list[[ind]], row = (i + 1) , column = (j + 1), pady = 2)
        local ({
          temp <- ind
          ii <- i
          jj <- j
          tkconfigure(button.list[[temp]], command = function() pri.toggle(ii,jj))
        })
      }
    }
    tkgrid(sw,sticky = "news", columnspan = 5, row = 2)
    tkgrid.rowconfigure(ss,2,weight = 1)
    tkgrid.columnconfigure(ss,1,weight=1)
    tkfocus(ss)
    tkgrab.set(ss)
    tkbind(ss,"<Destroy>",function(){tkgrab.release(ss);tkfocus(tt)})
  }

  obs.priors2 <- function() {
    removePriors <- function() {
      if (is.null(priorsmat)) {
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
      priorsmat <<- priorsmat[-rowSelected,, drop=FALSE]
      if (nrow(priorsmat)==0)
        priorsmat <<- NULL

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
          colSelection <- which(is.na(amelia.data[rowSelection,]))[varSelection]
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
          prMean<- prMin + (prMax-prMin/2)
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
        if (!is.null(priorsmat)) {
          matchPrior <- apply(priorsmat, 1,
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
            priorsmat<<-priorsmat[-which(matchPrior),]
        }
          
        priorsmat <<- rbind(priorsmat,newPrior)
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
          missVars    <- is.na(amelia.data[currentCase,])
        missVarNames<- colnames(amelia.data)[missVars]
        tcl("set","priVarNames",missVarNames)
        tkselect(varsBox,0)
      }
        
      
      addPrior.diag <- tktoplevel()
      tkwm.title(addPrior.diag, "Add Prior")
      missingCases <- apply(amelia.data, 1, function(x) any(is.na(x)))
      missingCases <- which(missingCases)
      anyMissing   <- apply(amelia.data, 2, function(x) any(is.na(x)))
      if (all(csvar>0,tsvar>0)) {
        cases <- apply(amelia.data[missingCases,c(csvar,tsvar)],1,
                       function(x) {
                         if (length(x)==2)
                           return(paste(x[1],x[2]))
                         else
                           paste(x)})
      } else if (all(csvar==0,tsvar==0)) {
        cases <- rownames(amelia.data)[missingCases]
      } else {
        cases <- paste(amelia.data[missingCases,c(csvar,tsvar)])
      }
      
      cases <- c("(whole variable)",cases)
      vars <- colnames(amelia.data)[anyMissing]
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
        tkgrid(meanBox, column=2, row=3, pady=3)
        tkgrid(sdBox, column=2, row=4, pady=3)        
        
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
       
        tkgrid(minBox, column=2, row=3, pady=3)
        tkgrid(maxBox, column=2, row=4, pady=3)
        tkgrid(confBox, column=2, row=5, pady=3)
        
      }
      
      addPriorOK <- tkbutton(addPrior.diag, text="OK", command=function() onOK())
      addPriorCancel <- tkbutton(addPrior.diag, text="Cancel",
                                 command=function() tkdestroy(addPrior.diag))

      tkgrid(addPriorOK, column=1,row=6)
      tkgrid(addPriorCancel, column=2, row=6)

      tkgrab.set(addPrior.diag)
      tkfocus(addPrior.diag)
      tkbind(addPrior.diag,"<Destroy>",
             function(){tkgrab.release(addPrior.diag);tkfocus(pp);tkgrab.set(pp)})
    }
    

    pp <- tktoplevel()
    
    tkwm.title(pp,"Observational Priors")
    current.priors <- tkframe(pp)
    currentPriorsSW <- tkwidget(pp, "ScrolledWindow", relief="sunken", borderwidth=2)
    priorsChecked <<- list()
    currentPriorsSF <- tkwidget(currentPriorsSW, "ScrollableFrame")
    subfID <- tclvalue(tcl(currentPriorsSF,"getframe"))
    tcl(currentPriorsSW, "setwidget", currentPriorsSF)
    subfID <- tclvalue(tcl(currentPriorsSF, "getframe"))
    addDistPriorButton <- tkbutton(pp, text = "Add Distribution Prior",
                               command = function() addPrior(type="dist"))
    addRangePriorButton <- tkbutton(pp, text = "Add Range Prior",
                               command = function() addPrior(type="range"))
    removePriorButton <- tkbutton(pp, text = "Remove Selected Priors", command =
                                  function() removePriors())
    obsOK <- tkbutton(pp, text="OK", command=function() {rm(priorsChecked,envir=.GlobalEnv);tkdestroy(pp);})
    
    if (is.null(priorsmat))
      localPriors <- matrix(NA,1,1)
    else
      localPriors <- priorsmat
    for (i in 1:nrow(localPriors)) {
      if (ncol(localPriors) == 1)
        next()
      priorsChecked[[i]]<<-tclVar("0")
      tkgrid(tcl("checkbutton",paste(subfID,".",i,sep=""),
                 variable = paste(priorsChecked[[i]])),
             pady=5, padx=5, row=i, column=1)

      if ((localPriors[i,1]==0))
        case <- "(whole variable)"
      else
        case <- paste(as.character(amelia.data[localPriors[i,1],csvar]),
                      as.character(amelia.data[localPriors[i,1],tsvar]))
      
      tkgrid(tcl("label",paste(subfID,".",i,"i",sep=""),text=case,
                 relief="sunken", width=20),row=i,column=2)

      tkgrid(tcl("label",paste(subfID,".",i,"i2",sep=""),text=
             names(amelia.data)[localPriors[i,2]], relief="sunken",
                 width=20),row=i,column=3)

      
      tkgrid(tcl("label",paste(subfID,".",i,"i3",sep=""),text=
             as.character(round(localPriors[i,3],5)), relief="sunken", width=20),
             row=i,column=4)

      
      tkgrid(tcl("label",paste(subfID,".",i,"i4",sep=""),text=
             as.character(round(localPriors[i,4],5)), relief="sunken", width=20),
             row=i,column=5)
    }
      
    #tkpack(currentPriorsSF)
    tkconfigure(currentPriorsSF,width=700)
    tkgrid(currentPriorsSW, row=1, sticky="news",columnspan=3)
    tkgrid(addDistPriorButton, addRangePriorButton, removePriorButton,row=2)
    tkgrid(obsOK, row=4, column=2, sticky="ns")
    tkfocus(pp)
    tkgrab.set(pp)
    tkbind(pp,"<Destroy>",function(){tkgrab.release(pp);tkfocus(tt);tkgrab.set(tt)})
  }
    
                                        
  pri.save <- function() {
    empri<<-tclVar(as.numeric(tclvalue(temp.empri)))
    tkdestroy(tt)
  }


  tt<-tktoplevel()
  tkwm.title(tt, "Priors Settings")
  gui.frame<-tkframe(tt)
  priors <- tkframe(tt, relief = "groove", borderwidth = 2)
	
  
  temp.empri<<-tclVar(as.numeric(tclvalue(empri)))
  empri.ent<-tkentry(priors,width=4,textvariable = temp.empri)
  empri.label<-tklabel(priors,text="Empirical prior:")
  case.but<-tkbutton(gui.frame,text="Set case priors",
                     command = function() case.priors())
  
  if (csvar == 0)
    tkconfigure(case.but, state="disabled")

  data.but<-tkbutton(gui.frame,text="Set observation priors")
  tkconfigure(data.but, command = function() obs.priors2())	
  tcl("set","prihelp","")
  
  pri.link <-tkbutton(tt, text = "?", 
    command = function()browseURL("http://gking.harvard.edu/amelia/docs/Priors_Dialog.html"))
  
  
  pri.status <- tkframe(tt, relief = "groove", borderwidth = 2)
  prihelp<-tklabel(pri.status, textvariable="prihelp", font=helpfont)
  
  pri.ok<-tkbutton(tt,text="   OK   ", command = function() pri.save())
  pri.cancel<-tkbutton(tt,text="Cancel", command = function()tkdestroy(tt))
  
  tkgrid(prihelp, row = 1, sticky = "nw")
  tkgrid(tkframe(pri.status, height = 0, width = 450), row = 2)
  tkgrid(empri.label, empri.ent,row = 1, padx = 5, pady = 5) 
  tkgrid(tklabel(priors, text = paste("(N for current dataset is ",
                           nrow(amelia.data),")",sep="")), row = 2)
  tkgrid(case.but,data.but, row = 1, padx = 5, pady = 5, sticky = "ew")
	
	
  
  
  
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
 	tkbind(case.but, "<Motion>","set prihelp \"Set prior beliefs about the similarity between different cross-sectional units.\"")
  tkbind(case.but, "<Leave>","set prihelp \"\"")
 	tkbind(data.but, "<Motion>","set prihelp \"Set priors about individual observations or whole variables.\"")
  tkbind(data.but, "<Leave>","set prihelp \"\"")                
}

gui.diag.setup <- function() {
  select.var <- function(varnum) {
		varnum2<-tknearest(diag.list, varnum)
		diag.sel.var<<-as.numeric(varnum2) + 1
		diag.var.name<<-names(amelia.data)[diag.sel.var]
		if (!is.numeric(amelia.data[,diag.sel.var]))
		  tkconfigure(diag.but.compare, state="disabled")
    else 
      tkconfigure(diag.but.compare, state="active")
  }
  tt<-tktoplevel()
  tkwm.title(tt, "Diagnostics")
  diag.var<-tkframe(tt, relief = "groove", borderwidth = 2)

 	if(!exists("diag.sel.var")) {
		diag.sel.var<<-1
		diag.var.name<<-names(amelia.data)[diag.sel.var]
	}
	#Listbox for variables.
	scr<-tkscrollbar(diag.var, repeatinterval = 5,
    command = function(...) tkyview(diag.list,...))
	diag.list<-tklistbox(diag.var,height=10,selectmode="single",
    yscrollcommand = function(...) tkset(scr,...))
	for (i in 1:ncol(amelia.data))
		tkinsert(diag.list,"end",names(amelia.data)[i])
	tkselection.set(diag.list,(diag.sel.var-1))
	tkbind(diag.list,"<Button-1>",function(y)select.var(y))
  diag.but.compare <- tkbutton(diag.var, text="Compare",
  	command = function() compare.density(data=amelia.data,output=amelia.list,
                          var=diag.sel.var,frontend=T))
  if (is.factor(amelia.data[,diag.sel.var]))
    tkconfigure(diag.but.compare, state="disabled")
  diag.overimp <- tkbutton(diag.var,text="Overimpute",state="normal",
    command = function() overimpute(data=amelia.data,output=amelia.list,
                                    var=diag.sel.var,frontend=T))
  diag.disp<-tkframe(tt, relief = "groove", borderwidth = 2)
  dimvalue<-tclVar("1")
  onedim<-tkradiobutton(diag.disp, variable=dimvalue, value="1")
  twodims<-tkradiobutton(diag.disp, variable=dimvalue, value="2")
  disp.imps.tcl<-tclVar("5")
  disp.imps<-tkentry(diag.disp,width="5",textvariable=disp.imps.tcl)
  disp.but<-tkbutton(diag.disp,text="Overdisperse",state="normal",
    command = function() disperse(data=amelia.data,m=as.numeric(tclvalue(disp.imps.tcl)),
    dims=as.numeric(tclvalue(dimvalue)),frontend=T,output=amelia.list))
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
  diaghelp<-tklabel(diag.status, textvariable="diaghelp", font=helpfont)

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
#tkwm.iconbitmap(gui,"c:/amelia/amelia.ico")
tkwm.deiconify(gui)
#tkwait.window(gui)

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
