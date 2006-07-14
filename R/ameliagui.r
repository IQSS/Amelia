##  ameliagui.r
##  a R-tcl/tk based frontend for Amelia (hopefully crossplatform)
##  mb 23/01/06 - function to be included in amelia package.
##  05/05/06 mb - catches unexpected amelia errors
##  09/05/06 mb - cleaned up diags menu, added overimpute button
##  22/06/06 mb - checks added to session loading.
##  26/06/06 mb - session saving/loading now mirrors amelia output.  

ameliagui<-function() {

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
    filetype<-"{{SPSS} {.dat}} {{All files} *}"
  if (as.numeric(drop.select)==4)
    filetype<-"{{SAS Transport} {.xport}} {{All files} *}"
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
    amelia.data<<-try(read.spss(am.filename,use.value.labels=F))
  if (drop.select == 4)
    amelia.data<<-try(read.xport(am.filename))
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
  idmat<<-matrix(0,nrow=ncol(amelia.data),ncol=1)
  transmat<<-matrix(0,nrow=ncol(amelia.data),ncol=1)
  amelia.lags<<-matrix(0,nrow=ncol(amelia.data),ncol=1)
  future.lags<<-matrix(0,nrow=ncol(amelia.data),ncol=1)
  num.poly<<-tclVar("1")
  intercs<<-tclVar("1")
  store.pri <<- NULL
  obs.prior <<- NULL
  r<<-NULL
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
  if (exists("amelia.list")) {
    amelia.list$amelia.args$am.filename <- am.filename
    amelia.list$amelia.args$varmin <- varmin
    amelia.list$amelia.args$varmax <- varmax
    amelia.list$amelia.args$output.select <- output.select
    dump("amelia.list", file.select)
    return(NULL)
  } 
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
  amelia.list$amelia.args$idvars <- c()
	amelia.list$amelia.args$ords <- c()
	amelia.list$amelia.args$noms <- c()
	amelia.list$amelia.args$logs <- c()
	amelia.list$amelia.args$sqrts <- c()
	amelia.list$amelia.args$lgstc <- c()
	amelia.list$amelia.args$lags <- c()
	amelia.list$amelia.args$leads <- c()
  
  for (i in 1:length(idmat)) {
    if (idmat[i] == 1)
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
  
	if (first.line != "\"amelia.list\" <-") {
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
    
  tclvalue(inname)<<-amelia.list$amelia.args$am.filename  
  load.data(session=T)
  tsvar <<- amelia.list$amelia.args$ts
  csvar <<- amelia.list$amelia.args$cs
  for (i in amelia.list$amelia.args$idvars)
    idmat[i]<<-1
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
  
  if (tsvar == 0) {
    num.poly<-NULL
    tsvar <-NULL
  }
  if (csvar == 0) {
    csvar <-NULL
    intercs<-FALSE
  }
  

  for (i in 1:ncol(amelia.data))
    if (idmat[i] == 1)
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


  mins <- matrix(NA, nrow = nrow(amelia.data), ncol = ncol(amelia.data))
  maxs <- matrix(NA, nrow = nrow(amelia.data), ncol = ncol(amelia.data))
  conf <- matrix(NA, nrow = nrow(amelia.data), ncol = ncol(amelia.data))
  means <- matrix(NA, nrow = nrow(amelia.data), ncol = ncol(amelia.data))
  sds <- matrix(NA, nrow = nrow(amelia.data), ncol = ncol(amelia.data))
  
  if (all(!is.null(varmin),!is.null(varmax))) {
    for (i in 1:ncol(amelia.data)) {
      if (all(!is.na(varmin[i]),!is.na(varmax[i]))) {
        mins[,i]<-varmin[i]
        maxs[,i]<-varmax[i]
        conf[,i]<-.95
      }
    }
  }
  if (!is.null(r)) {  
    for (i in 1:nrow(amelia.data)) {
      for (j in 1:ncol(amelia.data))  {
        if (all(r[i,j] != "MISS",r[i,j] != "--",j!=tsvar,j!=csvar)) {
          numholder <- strsplit(r[i,j],split = ",")
          if (length(numholder[[1]]) == 3) {
            mins[i,j] <- numholder[[1]][1]
            maxs[i,j] <- numholder[[1]][2]
            conf[i,j] <- numholder[[1]][3]
          } else {
            if (length(numholder[[1]]) == 2) {
              means[i,j] <- numholder[[1]][1]
              sds[i,j] <- numholder[[1]][2]
            } else {
              tkmessageBox(message="Failure in loading the observational priors.  Try again.",icon="error",type="ok")
              return(NULL)
            }
          }
        }
      }
    }
    storage.mode(mins) <- "numeric"
    storage.mode(maxs) <- "numeric"
    storage.mode(conf) <- "numeric"
    storage.mode(means) <- "numeric"
    storage.mode(sds) <- "numeric"
  }
  
  if (all(is.na(mins)))
    mins<-NULL
  if (all(is.na(maxs)))
    maxs<-NULL
  if (all(is.na(conf)))
    conf<-NULL
  if (all(is.na(means)))
    means<-NULL
  if (all(is.na(sds)))
    sds<-NULL
  
  
  amelia.list <<- try(amelia(amelia.data, m = as.numeric(tclvalue(outnum)),
        p2s = FALSE, idvars = id, casepri = store.pri, ts = tsvar, cs = csvar,
        mins=mins,maxs=maxs,conf=conf,means=means,sds=sds, lags = amlags,
        empri = as.numeric(tclvalue(empri)), intercs = intercs, leads = amfut, 
        polytime = num.poly, frontend=TRUE, logs=logs, sqrts=sqrts, lgstc=lgstc,
        ords=ord,write.out=F),silent=T)
  
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
      write.csv(out[[i]],file=paste(am.directory,"/",outname,i,".csv",sep=""))
  if (output.select == 2)
    for (i in 1:m) 
      write.table(out[[i]],file=paste(am.directory,"/",outname,i,".txt",sep=""),sep="\t")
  if (output.select == 3)
    for (i in 1:m) 
      write.dta(out[[i]],file=paste(am.directory,"/",outname,i,".dta",sep=""))
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
amelia.data <<- NULL
am.filename <<- NULL
varnames <<- NULL
tsvar <<- NULL
csvar <<- NULL
varmin <<- NULL
varmax <<- NULL
inname <<- tclVar("")


gui<-tktoplevel()
tkwm.title(gui, "Amelia II")
                         
##Menu
main.menu<-tkmenu(gui)
main.menu.file<-tkmenu(main.menu, tearoff=0)
main.menu.help<-tkmenu(main.menu, tearoff=0)
tkadd(main.menu.file,"command",label="Load Session",command=function()load.session())
tkadd(main.menu.file,"command",label="Save Session",command=function()save.session())
tkadd(main.menu.file,"command",label="Exit Amelia",command=function()main.close())
tkadd(main.menu.help,"command",label="About",command=function()tkmessageBox(title="Amelia For Windows",message="James Honaker, Gary King, & Matthew Blackwell\n2005",icon="info",type="ok"))
tkadd(main.menu,"cascade",label="File",menu=main.menu.file)
tkadd(main.menu,"cascade",label="Help",menu=main.menu.help)
tkconfigure(gui,menu=main.menu)

##Frame
gui.skel<-tkframe(gui)
main.title<-tklabel(gui, text="Amelia II",font=c("Arial",22))

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
file.types<-c("CSV","Tab Delimited","Stata","SPSS","SAS Transport")
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
input.load <- tkbutton(gui.input, text="Load Data", width = 10, command = function() load.data(,))
input.see <- tkbutton(gui.input, text="Summarize Data", state = "disabled", width = 13,
  command = function() sum.data())
input.help <-tkbutton(gui.input, text = "?", command = function()browseURL("http://gking.harvard.edu/amelia/amelia1/docs/"))

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
  command = function()browseURL("http://gking.harvard.edu/amelia/amelia1/docs/"))

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
output.types<-c("(no save)","CSV","Tab Delimited","Stata")
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
  command = function()browseURL("http://gking.harvard.edu/amelia/amelia1/docs/"))


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
    temp.trans[[selected.var]]<<-as.numeric(tclvalue(trans))
    temp.id[[selected.var]]<<-as.numeric(tclvalue(idvar))
    transmat<<-temp.trans
    idmat<<-temp.id
    tkdestroy(tt)
  }


  temp.trans<<-transmat
  temp.id<<-idmat
  

  var.link <-tkbutton(tt, text = "?", 
    command = function()browseURL("http://gking.harvard.edu/amelia/amelia1/docs/"))
	if(!exists("selected.var")) {
		selected.var<<-1
		selected.var.name<<-names(amelia.data)[selected.var]
	}
  
	idvar<<-tclVar(temp.id[[selected.var]])
	trans<<-tclVar(temp.trans[[selected.var]])

	gui.var.left<-tkframe(gui.frame,relief="groove",borderwidth=2)
	gui.var.center<-tkframe(gui.frame,relief="groove",borderwidth=2)
	gui.var.right<-tkframe(gui.frame,relief="groove",borderwidth=2)


	#Listbox for variables.
	scr<-tkscrollbar(gui.var.left,repeatinterval = 5,
	 command = function(...)tkyview(var.list,...))
	var.list.label<-tklabel(gui.var.left,text="Select a variable:")
	var.list<-tklistbox(gui.var.left,height=10,selectmode="single",
    yscrollcommand = function(...)tkset(scr,...))
	for (i in 1:ncol(amelia.data))
		tkinsert(var.list,"end",names(amelia.data)[i])
	tkselection.set(var.list,(selected.var-1))
	tkbind(var.list,"<Button-1>",function(y)select.var(y))

	#Options for vars
	var.opt.label<-tklabel(gui.var.center,text="Set variable options for:")
	var.opt.name <- tklabel(gui.var.center, text = selected.var.name,
					font = c("arial", 10, "bold"))
	var.opt.none<-tkradiobutton(gui.var.center,variable=trans,value="0",
				text="No Transformations")
	var.opt.ord<-tkradiobutton(gui.var.center,variable=trans,value="1",
				text="Ordinal")
	var.opt.nom<-tkradiobutton(gui.var.center,variable=trans,value="2",
				text="Nominal")
	var.opt.lgln<-tkradiobutton(gui.var.center,variable=trans,value="3",
				text="Log-Linear")
  var.opt.sqrt<-tkradiobutton(gui.var.center,variable=trans,value="4",
				text="Square Root")
	var.opt.lgstc<-tkradiobutton(gui.var.center,variable=trans,value="5",
				text="Logistic")
	var.opt.id<-tkcheckbutton(gui.var.center,text="ID Variable",variable=idvar)
  
  
  tcl("set","varhelp","")
  
  
  
  var.status<-tkframe(tt, relief = "groove", borderwidth = 3)
  var.help<-tklabel(var.status, textvariable="varhelp", font=helpfont)



  var.ok<-tkbutton(tt,text="OK", command = function() var.save())
  var.cancel<-tkbutton(tt,text="Cancel", command = function()tkdestroy(tt))

  tkgrid(var.list.label, row = 1, column = 1, columnspan = 2)
  tkgrid(var.list,row = 2, column = 1)
  tkgrid(scr, row = 2, column = 2)
  tkgrid.configure(scr,rowspan=4,sticky="nsw")
	tkgrid(tkframe(gui.var.center, height = 0, width = 150), row = 1, columnspan = 2)
	tkgrid(var.opt.label, row = 2, columnspan = 2)
	tkgrid(var.opt.name, row = 3)
	tkgrid(var.opt.none, row = 4, sticky = "w")
	tkgrid(var.opt.ord, row = 5, sticky = "w")
	tkgrid(var.opt.nom, row = 6, sticky = "w")
	tkgrid(var.opt.lgln, row = 7, sticky = "w")
	tkgrid(var.opt.sqrt, row = 8, sticky = "w")
	tkgrid(var.opt.lgstc, row = 9, sticky = "w")
	tkgrid(var.opt.id, row = 10, sticky = "w")
	tkgrid(tkframe(gui.var.center, width = 0, height = 12), column = 2, rowspan = 8)
	tkgrid(gui.var.left,gui.var.center,padx=10,pady=10,sticky="news")
	tkpack(var.help, anchor = "w")
	tkgrid(tklabel(tt, text="Variables Options", font="Arial 16 bold"),pady=5, row = 1, column = 1, columnspan = 2,sticky="w")
	tkgrid(var.link, row = 1, column = 4, sticky = "ne", padx = 2, pady = 2)
	tkgrid(gui.frame, row = 2,columnspan = 5)
	tkgrid(var.status, sticky="sew", row = 4,columnspan = 5)
	tkgrid(var.ok, row = 3, column = 3, sticky = "sew", padx = 10, pady = 10)
  tkgrid(var.cancel, row = 3, column = 4, sticky = "sew", padx = 10, pady = 10)
	
  tkfocus(tt)
  tkgrab.set(tt)
  tkbind(tt,"<Destroy>", function() {tkgrab.release(tt);tkfocus(gui)})
	
	tkbind(var.opt.none, "<Motion>","set varhelp \"The variable needs no transformations.\"")
  tkbind(var.opt.none, "<Leave>","set varhelp \"\"")
	tkbind(var.opt.ord, "<Motion>","set varhelp \"The variable is organized into ranked groups.\"")
  tkbind(var.opt.ord, "<Leave>","set varhelp \"\"")
	tkbind(var.opt.nom, "<Motion>","set varhelp \"The variable is grouped with no specific ordering.\"")
  tkbind(var.opt.nom, "<Leave>","set varhelp \"\"")
 	tkbind(var.opt.lgln, "<Motion>","set varhelp \"The variable needs a log-linear transformation.\"")
  tkbind(var.opt.lgln, "<Leave>","set varhelp \"\"")
  tkbind(var.opt.sqrt, "<Motion>","set varhelp \"The variable needs a square root transformation.\"")
  tkbind(var.opt.sqrt, "<Leave>","set varhelp \"\"")
 	tkbind(var.opt.lgstc, "<Motion>","set varhelp \"The variable needs a logistic transformation.\"")
  tkbind(var.opt.lgstc, "<Leave>","set varhelp \"\"")
 	tkbind(var.opt.id, "<Motion>","set varhelp \"The variable is an identification variable.\"")
  tkbind(var.opt.id, "<Leave>","set varhelp \"\"")
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
		
		if (is.factor(amelia.data[,sum.var])) {
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
    tkcmd(sw,"setwidget",sf)
    subfID <- tclvalue(tkcmd(sf,"getframe"))
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
    command = function()browseURL("http://gking.harvard.edu/amelia/amelia1/docs/"))
  
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
	
	var.sum.name <- tklabel(gui.sum.right, text = sum.var.name,
    font = c("arial", 10, "bold"))
  if (!is.factor(amelia.data[,sum.var])) {
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
    command = function()browseURL("http://gking.harvard.edu/amelia/amelia1/docs/"))
  
  
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
    tkcmd(sw,"setwidget",sf)
    subfID <- tclvalue(tkcmd(sf,"getframe"))
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




	call.datapri<-function() {
    up.prog <-function(i,tot) {
      percent<- 2*(as.integer(100*(i/tot)))
      tkcoords(prog,"bar",0,0,percent,20)
      tcl("update")
      if (i  == tot)
          tkdestroy(ttt)
    }
    reset.obs<-function() {
      obs.prior<<-NULL
      r<<-NULL
      tkdestroy(pp)
      call.datapri()
      return(NULL)
    }  
    save.obs<-function() {
      cell.location<-strsplit(tclvalue(tcl(table1,"curselection")),split = ",")
      row.pos<-as.integer(cell.location[[1]][1])
      col.pos<-as.integer(cell.location[[1]][2])
      if (!is.na(tclvalue(tcl(table1,"curvalue"))))
        rtemp[as.integer(row.names(shown.table)[row.pos]),col.pos]<<-tclvalue(tcl(table1,"curvalue")) 
      r<<-rtemp
      tkdestroy(pp)
    }  
    pull.table<-function(case.selected) {
      cell.location<-strsplit(tclvalue(tcl(table1,"curselection")),split = ",")
      row.pos<-as.integer(cell.location[[1]][1])
      col.pos<-as.integer(cell.location[[1]][2])
      if (!is.na(tclvalue(tcl(table1,"curvalue"))))
        rtemp[as.integer(row.names(shown.table)[row.pos]),col.pos]<<-tclvalue(tcl(table1,"curvalue"))
      invlist<<-rtemp[ifelse(apply(is.na(amelia.data),1,sum),TRUE,FALSE),]
      
      
      if (case.selected == 0) {
        case.selected<-seq(1:length(unique(amelia.data[,csvar])))
        shown.table<<-invlist
      } else {
        if (all(!is.na(amelia.data[amelia.data[,csvar]==unique(amelia.data[,csvar])[case.selected],]))) {
          tkmessageBox(title="No missing data",message="There is no missing data in the chosen subset.",type="ok")
          return(NULL)
        }
        shown.table<<-invlist[invlist[,csvar]==unique(amelia.data[,csvar])[case.selected],]
      } 
      if ((nrow(shown.table)* ncol(shown.table)) >  6000) {
        big.list<-tkmessageBox(title="Large Subset",message="You have specified a large subset of data which could take many minutes (up to 10) to load.  Are you sure you want load the whole dataset at one time?",type="yesno")
        if (tclvalue(big.list)=="no")
            return(NULL)
      }
      #tcl("unset",obs.prior)
      obs.prior<<- tclArray()
      tkconfigure(table1,variable=obs.prior,rows=nrow(shown.table)+1,cols=ncol(shown.table)+1)
      ttt<<-tktoplevel()
      tkwm.title(ttt,"Loading...")
      prog<<-tkcanvas(ttt, width = 200, height = 20, bd = 1, relief = "sunken",
        highlightt = 0)
      tkcreate(prog,"rectangle",0,0,0,20,tags = "bar",fill="navy")
      tkpack(prog,padx=10,pady=10)
      count2<-1
      tot2<-nrow(shown.table)*ncol(shown.table)
      
      for (i in 0:nrow(shown.table)) {  
        for (j in 0:ncol(shown.table)) {
          if (i==0 && j!=0) {
            tcl(table1,"set",paste(i,j,sep=","),colnames(shown.table)[j])
          }
          if (i!=0 && j==0) {
            tcl(table1,"set",paste(i,j,sep=","),rownames(shown.table)[i])
          }
          if (i!=0 && j!=0) {	        
            tcl(table1,"set",paste(i,j,sep=","),shown.table[i,j]) 
            if (!is.na(amelia.data)[rownames(shown.table)[i],j])
              tkcmd(table1,"tag","celltag","obs",paste(i,j,sep=","))
  			  	else
			  	    tkcmd(table1,"tag","celltag","miss",paste(i,j,sep=","))
            up.prog(count2,tot2)
            count2<-count2+1
            tcl("update")
          }
        }
        
        
      }
        
    }
    save.cell<-function(cell) {
      cell.location<-strsplit(cell,split = ",")
      row.pos<-as.integer(cell.location[[1]][1])
      col.pos<-as.integer(cell.location[[1]][2])
      if (!is.na(tclvalue(tcl(table1,"get",cell))))
        rtemp[as.integer(row.names(shown.table)[row.pos]),col.pos]<<-tclvalue(tcl(table1,"get",cell))
    }
   

    count<-1
    if (is.null(r)) {
      r<<-ifelse(is.na(amelia.data),"MISS","--")
      if (tsvar != 0)
        r[,tsvar] <<- amelia.data[,tsvar]
      if (csvar != 0)
        r[,csvar] <<- as.character(amelia.data[,csvar])
    }
    rtemp<<-r
    invlist<<-r[ifelse(apply(is.na(amelia.data),1,sum),TRUE,FALSE),]
    tot<-(nrow(invlist)*ncol(invlist))
    if (tot > 15000) {
      if (csvar == 0) {
        big.list<-tkmessageBox(title="Large Subset",message="You have specified a large subset of data which could take many minutes (up to 10) to load.  Are you sure you want load the whole dataset at one time?",type="yesno")
        if (tclvalue(big.list)=="no")
          return(NULL)
        else
          shown.table<<-invlist
      } else {
        shown.table<<-invlist[invlist[,csvar]==unique(amelia.data[,csvar])[1],]
        if (nrow(shown.table)*ncol(shown.table) > 15000) {
          big.list<-tkmessageBox(title="Large Subset",message="You have specified a large subset of data which could take many minutes (up to 10) to load.  Are you sure you want load the whole dataset at one time?",type="yesno")
          if (tclvalue(big.list)=="no")
            return(NULL)
        }
        tot<-nrow(shown.table)*ncol(shown.table)
      }
    } else {
      shown.table<<-invlist
    }
    ttt<-tktoplevel()
    tkwm.title(ttt,"Loading...")
    prog<<-tkcanvas(ttt, width = 200, height = 20, bd = 1, relief = "sunken",
      highlightt = 0)
    tkcreate(prog,"rectangle",0,0,0,20,tags = "bar",fill="navy")
    tkpack(prog,padx=10,pady=10)
    if (is.null(obs.prior))
      obs.prior<<- tclArray()
    
    #for (i in 1:nrow(shown.table)) {
    #  for (j in 1:ncol(shown.table)) {
    #    obs.prior[i,j] <- shown.table[i,j]
    #    up.prog(count,tot)
    #    count<-count+1
    #  }
    #}
    #fill.names(obs.prior,amelia.data)
    #tclArrayName <<- ls(obs.prior$env)
		height<--1
		width<--1
		pp <- tktoplevel()
 		#tkwm.title(pp,tclArrayName)
 		table1 <<- tkwidget(pp, "table", rows = nrow(shown.table)+1,
			cols = ncol(shown.table)+1, titlerows = "1", titlecols = "1",
			height = 20, width = paste(width+1),
			xscrollcommand=function(...) tkset(xscr,...),
			yscrollcommand=function(...) tkset(yscr,...))
 		xscr <-tkscrollbar(pp,orient="horizontal",
			command=function(...)tkxview(table1,...))
 		yscr <- tkscrollbar(pp,command=function(...)tkyview(table1,...))
		tkgrid(tklabel(pp,text="-If you have an idea of how the distribution of the observation looks: mean, standard deviation", justify = "left"),
			 row = 4, sticky = "nw", columnspan = 5)
		tkgrid(tklabel(pp,text="-If you have an idea of the approximate range of the value: minimum, maximum, degree of confidence.",
			justify = "left"), row = 5, sticky = "nw", columnspan = 5)
		
    caselist <- "        . . . . . . . . . . . . . . . . . . "
    tcl("set","caselist",caselist)	
		priCaseSelect<-tkwidget(pp,"combobox",listvar="caselist",editable="FALSE")
		if (csvar == 0) {
      tkconfigure(priCaseSelect, state="disabled")
    } else {                                               
      caselist<-unique(amelia.data[,csvar])
      caselist<-c("(all)",as.character(caselist))
      tcl("set","caselist",caselist)
    }
    if (nrow(invlist)*ncol(invlist) > 15000)
      tkselect(priCaseSelect,1)
    else 
      tkselect(priCaseSelect,0) 
    applyPriChanges<-tkbutton(pp, text="Apply",command=function() pull.table(as.numeric(tkcurselection(priCaseSelect))))
		tkgrid(priCaseSelect,applyPriChanges,row=1, sticky="sew",padx=5,pady=5)
    ok.obs<-tkbutton(pp, text="Ok", command = function() save.obs())
		can.obs<-tkbutton(pp, text="Cancel", command = function() tkdestroy(pp))
    reset.o<-tkbutton(pp,text = "Reset Values to Default",
      command = function() reset.obs())
    tkgrid(ok.obs, can.obs, reset.o, row = 6, sticky = "sew", padx = 5, pady = 5)
 		tkgrid(table1, row = 2, columnspan = 5)
 		tkgrid(yscr,sticky="nsw",row = 2, column = 6)
 		tkgrid(xscr,sticky="new", row = 3, columnspan = 5)
 		tkconfigure(table1,background="white", variable=obs.prior,
        selectmode="extended", coltagcommand = "colorize",autoclear=1,browsecommand=function(s) save.cell(s))
    tcl(table1,"tag","delete","obs")
    tcl(table1,"tag","configure","active",foreground = "black",background="white")
    tcl(table1,"tag","configure","obs",state="disabled",background="SystemButtonFace",foreground="SystemDisabledText")
    tcl(table1,"tag","configure","miss",state="normal")
    tkbind(table1,"<Return>","break")
 		for (i in 0:nrow(shown.table)) {  
			for (j in 0:ncol(shown.table)) {
			  if (i==0 && j!=0) {
			    tcl(table1,"set",paste(i,j,sep=","),colnames(shown.table)[j])
	      }
	      if (i!=0 && j==0) {
			    tcl(table1,"set",paste(i,j,sep=","),rownames(shown.table)[i])
	      }
        if (i!=0 && j!=0) {	        
          tcl(table1,"set",paste(i,j,sep=","),shown.table[i,j]) 
				  if (!is.na(amelia.data)[rownames(shown.table)[i],j])
 				  	tkcmd(table1,"tag","celltag","obs",paste(i,j,sep=","))
			  	else
			  	  tkcmd(table1,"tag","celltag","miss",paste(i,j,sep=","))
	        up.prog(count,tot)
          count<-count+1
          tcl("update")
        }
			}
		}
		tkfocus(pp)
		tkgrab.set(pp)
		tkbind(pp,"<Destroy>",function() {tkgrab.release(pp);tkfocus(tt)})
	}
	
	#observational priors for linux/mac?
  unix.datapri<-function() {
    up.prog <-function(i,tot) {
      percent<- 2*(as.integer(100*(i/tot)))
      tkcoords(prog,"bar",0,0,percent,20)
      tcl("update")
      if (i  == tot)
          tkdestroy(ttt)
    }
    reset.obs<-function() {
      r<<-NULL
      tkdestroy(pp)
      unix.datapri()
      return(NULL)
    }  
    save.obs<-function() {
      r<<-rtemp
      tkdestroy(pp)
    }
    unix.save.cell<-function(i,j) {
      rtemp[rownames(shown.table)[i],j-1]<<-tclvalue(rArray[[(ncol(shown.table)*(as.numeric(rownames(shown.table)[i])-1)+(j-1))]])
    }
    pull.table<-function(case.selected) {
      invlist<<-rtemp[ifelse(apply(is.na(amelia.data),1,sum),TRUE,FALSE),]
      if (case.selected == 0) {
        case.selected<-seq(1:length(unique(amelia.data[,csvar])))
        shown.table<<-invlist
      } else {
        if (all(!is.na(amelia.data[amelia.data[,csvar]==unique(amelia.data[,csvar])[case.selected],]))) {
          tkmessageBox(title="No missing data",message="There is no missing data in the chosen subset.",type="ok")
          return(NULL)
        }
        shown.table<<-invlist[invlist[,csvar]==unique(amelia.data[,csvar])[case.selected],]
      } 
      if ((nrow(shown.table)* ncol(shown.table)) >  6000) {
        big.list<-tkmessageBox(title="Large Subset",message="You have specified a large subset of data which could take many minutes (up to 10) to load.  Are you sure you want load the whole dataset at one time?",type="yesno")
        if (tclvalue(big.list)=="no")
            return(NULL)
      }
      #tcl("unset",obs.prior)
      ttt<<-tktoplevel()
      tkwm.title(ttt,"Loading...")
      prog<<-tkcanvas(ttt, width = 200, height = 20, bd = 1, relief = "sunken",
        highlightt = 0)
      tkcreate(prog,"rectangle",0,0,0,20,tags = "bar",fill="navy")
      tkpack(prog,padx=10,pady=10)
      count2<-1
      tot2<-nrow(shown.table)*ncol(shown.table)
      rArray<<-list()
      rArray[[(nrow(r)*ncol(r))+1]]<<-NA
      tkdestroy(sw2)
      sw2 <<- tkwidget(pp,"ScrolledWindow",relief="sunken",borderwidth=2)
      s2 <- tkwidget(sw2,"ScrollableFrame")
      tkcmd(sw2,"setwidget",s2)
      s2.height<-nrow(shown.table)*25
      s2.width<-nrow(shown.table)*115
      if (s2.height > 550)
        s2.height<-550
      if (s2.width > 750)
        s2.width<-750 
      tkconfigure(s2,width=s2.width,height=s2.height)
      subfID <- tclvalue(tkcmd(s2,"getframe"))
      for (i in 1:(1+nrow(shown.table))) {
        for (j in 1:(1+ncol(shown.table))) {
          if (j!=1 && i==1) {
            tkgrid(tcl("entry",paste(subfID,".",i,"k",j,sep=""), textvariable= tclVar(names(amelia.data)[j-1]), 
              relief="flat",width=11,state="disabled",foreground="black",justify="center",background="grey"),row=1,column=j)
          }   
          if (j==1 && i !=1) {
            tkgrid(tcl("entry",paste(subfID,".",i,"g",j,sep=""), 
              textvariable = tclVar(paste(row.names(shown.table)[i-1])), state="disabled",
              width=8,foreground="black",background="gray", relief="flat",justify="right"), row = i+3, column = j)
          } 
          if (j!=1 && i!=1) {  
            if (!is.na(amelia.data)[rownames(shown.table)[i-1],j-1]) {
              tkgrid(tcl("entry",paste(subfID,".",i,"h",j,sep=""), 
                textvariable = tclVar(paste(shown.table[i-1,j-1])), state="disabled",width=11,
                foreground="black"), row = i+3, column = j)
              tkbind(paste(subfID,".",i,"h",j,sep=""),"<Motion>",paste("set hovervar \"", names(amelia.data)[j-1],"\"",sep=""))
              tkbind(paste(subfID,".",i,"h",j,sep=""),"<Leave>","set hovervar \" \"") 
            } else {
              rArray[[(ncol(shown.table)*(as.numeric(row.names(shown.table)[i-1])-1)+(j-1))]]<-tclVar(shown.table[i-1,j-1])
              tkgrid(tcl("entry",paste(subfID,".",i,"i",j,sep=""), background="white",
                width=11,textvariable = rArray[[ncol(shown.table)*(as.numeric(row.names(shown.table)[i-1])-1)+(j-1)]]), 
                row = i+3, column = j)
              tkbind(paste(subfID,".",i,"i",j,sep=""),"<Motion>",paste("set hovervar \"", names(amelia.data)[j-1],"\"",sep=""))
              tkbind(paste(subfID,".",i,"i",j,sep=""),"<Leave>","set hovervar \" \"")
              local ({
                ii <- i
                jj <- j
                tkbind(paste(subfID,".",i,"i",j,sep=""),"<KeyRelease>",function()unix.save.cell(ii,jj))
              })
            
            }
            up.prog(count2,tot2)
            count2<-count2+1
                
          }
        }
      }
      tkpack(sw2,fill="both",expand="yes",side="bottom",after=but.frame)   
      tkfocus(pp)
    }    
        
    count<-1
    if (is.null(r)) {
      r<<-ifelse(is.na(amelia.data),"MISS","--")
      if (tsvar != 0)
        r[,tsvar] <<- amelia.data[,tsvar]
      if (csvar != 0)
        r[,csvar] <<- as.character(amelia.data[,csvar])
    }
    rtemp<<-r
    invlist<<-r[ifelse(apply(is.na(amelia.data),1,sum),TRUE,FALSE),]
    tot<-(nrow(invlist)*ncol(invlist))
      
    if (tot > 7000) {
      if (csvar == 0) {
        big.list<-tkmessageBox(title="Large Subset",message="You have specified a large subset of data which could take minutes to load.  Are you sure you want load the whole dataset at one time?",type="yesno")
        if (tclvalue(big.list)=="no")
          return(NULL)
        else
          shown.table<<-invlist
      } else {
        shown.table<<-invlist[invlist[,csvar]==unique(amelia.data[,csvar])[1],]
        if (nrow(shown.table)*ncol(shown.table) > 15000) {
          big.list<-tkmessageBox(title="Large Subset",message="You have specified a large subset of data which could take minutes to load.  Are you sure you want load the whole dataset at one time?",type="yesno")
          if (tclvalue(big.list)=="no")
            return(NULL)
        }
        tot<-nrow(shown.table)*ncol(shown.table)
      }
    } else {
      shown.table<<-invlist
      tot<-nrow(shown.table)*ncol(shown.table)
    }
    ttt<-tktoplevel()
    tkwm.title(ttt,"Loading...")
    prog<-tkcanvas(ttt, width = 200, height = 20, bd = 1, relief = "sunken",
      highlightt = 0)
    tkcreate(prog,"rectangle",0,0,0,20,tags = "bar",fill="navy")
    tkpack(prog,padx=10,pady=10)

		pp <- tktoplevel()
 		tkwm.title(pp,"Observational Priors")
    sw2 <- tkwidget(pp,"ScrolledWindow",relief="sunken",borderwidth=2)
    sf <- tkwidget(sw2,"ScrollableFrame")
    tkcmd(sw2,"setwidget",sf)
    sf.height<-nrow(shown.table)*25
    sf.width<-nrow(shown.table)*115
    if (sf.height > 550)
      sf.height<-550
    if (sf.width > 750)
      sf.width<-750 
    tkconfigure(sf,width=sf.width,height=sf.height)
    subfID <- tclvalue(tkcmd(sf,"getframe"))
    cnameframe<-tkframe(pp)
    but.frame<-tkframe(pp)
    caselist <- "        . . . . . . . . . . . . . . . . . . "
    tcl("set","caselist",caselist)	
		priCaseSelect<-tkwidget(cnameframe,"combobox",listvar="caselist",editable="FALSE")
		if (csvar == 0) {
      tkconfigure(priCaseSelect, state="disabled")
    } else {                                               
      caselist<-unique(amelia.data[,csvar])
      caselist<-c("(all)",as.character(caselist))
      tcl("set","caselist",caselist)
    }
    if (nrow(invlist)*ncol(invlist) > 7000)
      tkselect(priCaseSelect,1)
    else 
      tkselect(priCaseSelect,0)                                   
    applyPriChanges<-tkbutton(cnameframe, text="Apply",width=11,command=function() pull.table(as.numeric(tkcurselection(priCaseSelect))))
		tkgrid(priCaseSelect,applyPriChanges,row=1, sticky="sew",padx=5,pady=5)   
    
    tkpack(cnameframe,anchor="nw")
    tkpack(but.frame, side="bottom",anchor="se")
    rArray<<-list()
    rArray[[(nrow(r)*ncol(r))+1]]<<-NA
 		ok.obs<-tkbutton(but.frame, text="Ok", command = function() save.obs(), width=11)
		can.obs<-tkbutton(but.frame, text="Cancel", command = function() tkdestroy(pp),width=11)
    reset.o<-tkbutton(but.frame,text = "Reset Values", width=11,
      command = function() reset.obs())
    tkgrid(ok.obs, can.obs, reset.o, row = 5, sticky = "sew", padx = 5, pady = 5)
    hover.frame<-tkframe(pp,relief="groove",borderwidth=2)
    tcl("set","hovervar","")
    hover.var<-tklabel(hover.frame,textvariable="hovervar")
    tkpack(hover.var,anchor="w")
    tkpack(hover.frame,fill="x",expand="yes",side="bottom",before=but.frame)
    for (i in 1:(1+nrow(shown.table))) {
      for (j in 1:(1+ncol(shown.table))) {
        if (j!=1 && i==1) {
          tkgrid(tcl("entry",paste(subfID,".",i,"k",j,sep=""), textvariable= tclVar(names(amelia.data)[j-1]), 
            relief="flat",width=11,state="disabled",foreground="black",justify="center",background="grey"),row=1,column=j)
        }    
        if (j==1 && i !=1) {
          tkgrid(tcl("entry",paste(subfID,".",i,"g",j,sep=""), 
            textvariable = tclVar(paste(row.names(shown.table)[i-1])), state="disabled",
            width=8,foreground="black",background="gray", relief="flat",justify="right"), row = i+3, column = 1)
        } 
        if (j!=1 && i!=1) {  
          if (!is.na(amelia.data)[rownames(shown.table)[i-1],j-1]) {
            tkgrid(tcl("entry",paste(subfID,".",i,"h",j,sep=""), 
              textvariable = tclVar(paste(shown.table[i-1,j-1])), state="disabled",width=11,
              foreground="black"), row = i+3, column = j)
            tkbind(paste(subfID,".",i,"h",j,sep=""),"<Motion>",paste("set hovervar \"", names(amelia.data)[j-1],"\"",sep=""))
            tkbind(paste(subfID,".",i,"h",j,sep=""),"<Leave>","set hovervar \" \"") 
          } else {
            rArray[[(ncol(shown.table)*(as.numeric(row.names(shown.table)[i-1])-1)+(j-1))]]<-tclVar(shown.table[i-1,j-1])
            tkgrid(tcl("entry",paste(subfID,".",i,"i",j,sep=""), background="white",
              width=11,textvariable = rArray[[ncol(shown.table)*(as.numeric(row.names(shown.table)[i-1])-1)+(j-1)]]), 
              row = i+3, column = j)
            tkbind(paste(subfID,".",i,"i",j,sep=""),"<Motion>",paste("set hovervar \"", names(amelia.data)[j-1],"\"",sep=""))
            tkbind(paste(subfID,".",i,"i",j,sep=""),"<Leave>","set hovervar \" \"")
            local ({
              ii <- i
              jj <- j
              tkbind(paste(subfID,".",i,"i",j,sep=""),"<KeyRelease>",function()unix.save.cell(ii,jj))
            })
            
          }
          
          up.prog(count,tot)
          count<-count+1 
        }
                
      }
    }
    tkpack(sw2,fill="both",expand="yes",side="bottom",after=but.frame)
		tkfocus(pp)
		tkgrab.set(pp)
		tkbind(pp,"<Destroy>",function() {tkgrab.release(pp);tkfocus(tt);})
	}

  pri.save <- function() {
    empri<<-tclVar(as.numeric(tclvalue(temp.empri)))
    for (i in 1:ncol(amelia.data)) {
      varmin[i]<<-as.numeric(tclvalue(tclvarmin[[i]]))
      varmax[i]<<-as.numeric(tclvalue(tclvarmax[[i]]))
    }
    tkdestroy(tt)
  }
  select.var<-function(varnum) {
		varnum2<-tknearest(var.list, varnum)
		pri.var<<-as.numeric(varnum2) + 1
		pri.varname<<-names(amelia.data)[pri.var]
		tkconfigure(minbox, textvariable = tclvarmin[[pri.var]])
		tkconfigure(maxbox, textvariable = tclvarmax[[pri.var]])
		if (any(is.factor(amelia.data[,pri.var]),pri.var==tsvar,pri.var==csvar)) {
		  tkconfigure(minbox, state = "disabled")
		  tkconfigure(maxbox, state = "disabled")
    } else {
		  tkconfigure(minbox, state = "normal")
		  tkconfigure(maxbox, state = "normal")
    }
	}
  if (is.null(varmin)) {
    varmin<<-matrix(NA,ncol(amelia.data),1)
    varmax<<-matrix(NA,ncol(amelia.data),1)
    pri.var<<-1
    pri.varname<<-names(amelia.data)[pri.var]

  }
  
  for (i in 1:ncol(amelia.data)) {
    tclvarmin[[i]]<<-tclVar(paste(varmin[i]))
    tclvarmax[[i]]<<-tclVar(paste(varmax[i]))
  }



  tt<-tktoplevel()
  tkwm.title(tt, "Priors Settings")
  gui.frame<-tkframe(tt)
	priors <- tkframe(tt, relief = "groove", borderwidth = 2)
	gui.bottom1<-tkframe(tt, relief = "groove",borderwidth=2)
	gui.bottom2<-tkframe(tt, relief = "groove", borderwidth=2)
	
	 	#Listbox for variables.
  scr <- tkscrollbar(gui.bottom1, repeatinterval=5,
    command=function(...)tkyview(var.list,...))
	var.list.label<-tklabel(gui.bottom1,text="Select a variable:")
	var.list<-tklistbox(gui.bottom1,height=10,selectmode="single",
    yscrollcommand=function(...)tkset(scr,...))
	for (i in 1:ncol(amelia.data))
		tkinsert(var.list,"end",names(amelia.data)[i])
	tkselection.set(var.list,(pri.var-1))
	tkbind(var.list,"<Button-1>",function(y)select.var(y))
	
	boxlabel<-tklabel(gui.bottom2, text=paste("Lower and Upper Bounds:"))
	minbox<-tkentry(gui.bottom2, width = 10, textvariable=tclvarmin[[pri.var]])
	maxbox<-tkentry(gui.bottom2, width = 10, textvariable=tclvarmax[[pri.var]])
 
 if (any(is.factor(amelia.data[,pri.var]),pri.var==tsvar,pri.var==csvar)) {
		  tkconfigure(minbox, state = "disabled")
		  tkconfigure(maxbox, state = "disabled")
    } else {
		  tkconfigure(minbox, state = "normal")
		  tkconfigure(maxbox, state = "normal")
    }
 
	temp.empri<<-tclVar(as.numeric(tclvalue(empri)))
	empri.ent<-tkentry(priors,width=4,textvariable = temp.empri)
	empri.label<-tklabel(priors,text="Empirical prior:")
	case.but<-tkbutton(gui.frame,text="Set case priors",
    command = function() case.priors())
  if (csvar == 0)
    tkconfigure(case.but, state="disabled")
	data.but<-tkbutton(gui.frame,text="Set observation priors")
  if (.Platform$OS.type == "windows")
		tkconfigure(data.but, command = function() call.datapri())
	else 
    tkconfigure(data.but, command = function() unix.datapri())	
  tcl("set","prihelp","")
  
  pri.link <-tkbutton(tt, text = "?", 
    command = function()browseURL("http://gking.harvard.edu/amelia/amelia1/docs/"))
  
  
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
	tkgrid(var.list.label,row = 1, column = 1)
	tkgrid(var.list, row = 2, column = 1)
  tkgrid(scr, row = 2, column = 2, sticky="nsw")
  tkgrid(boxlabel, row = 1, padx=3, pady=3,columnspan = 2)
  tkgrid(minbox, maxbox, row = 2, padx=3, pady=3,sticky="ew")
  tkgrid(pri.link, sticky = "ne", row = 1, column = 2, padx = 2, pady = 2)
	tkgrid(priors, sticky = "nsew", row = 2, column = 0, columnspan = 3, padx = 10, pady = 10)
	tkgrid(gui.bottom1, row = 3, column = 0, sticky="news",padx=10,pady=10)
	tkgrid(gui.bottom2, row = 3, column = 1, sticky="news", columnspan=2,padx=10,pady=10)
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
 	tkbind(data.but, "<Motion>","set prihelp \"Set beliefs about the likely ranges of individual missing observations.\"")
  tkbind(data.but, "<Leave>","set prihelp \"\"")                
}

gui.diag.setup <- function() {
  select.var <- function(varnum) {
		varnum2<-tknearest(diag.list, varnum)
		diag.sel.var<<-as.numeric(varnum2) + 1
		diag.var.name<<-names(amelia.data)[diag.sel.var]
		if (is.factor(amelia.data[,diag.sel.var]))
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
tkwait.window(gui)

}
                                                                  
