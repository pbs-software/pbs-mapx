##==============================================================================
## mapExtra
## --------
##  calcGAP..............Calculate the gridded area within a polygon.
##  combineEventsQuickly......Combine measurements of events using a quicker technique than that used in PBSmapping.
##  colTarg..............Colour points depending on target field from qualified data.
##  sumT.................Summary function for summing catch in tonnes.
##  xtget................Provide wrappers for PBSmodelling functions tget/tcall/tprint/tput/lisp
##
##-----Supplementary hidden functions-----
## .fixNumIDs............Quick fixer-upper to rectify `.createIDs' handling of SIDs
## .win.get.lang.........Get language(s) for use in figure files
##==============================================================================


## calcGAP------------------------------2013-01-24
## Calculate the gridded area within a polygon.
## Uses output from 'createMap'.
## ---------------------------------------------RH
calcGAP = function(polyA,events,loc,pdata,polyID)
{
	## Populate the events data with PID and SID from a locData set
	pid = loc$PID; names(pid) = loc$EID
	sid = loc$SID; names(sid) = loc$EID
	events$PID = pid[as.character(events$EID)]
	events$SID = sid[as.character(events$EID)]
	events$ID = .createIDs(events,cols=c("PID","SID"),fastIDdig=3)
	ev0 = events[events$Z>0 & !is.na(events$Z),]

	## Add grid areas to the events data using a PolyData set
	pdata$ID = .createIDs(pdata,cols=c("PID","SID"),fastIDdig=3)
	area = pdata$area; names(area) = pdata$ID
	events$area = area[as.character(events$ID)]
	ev0 = events[events$Z>0 & !is.na(events$Z),]

	## Calculate the area covered by grid cells within each polyID of polyA
	areaN = NULL
	for (i in polyID) {
		polyAsub = polyA[is.element(polyA$PID,i),]
		#zI = as.logical(sp::point.in.polygon(ev0$X,ev0$Y,polyAsub$X,polyAsub$Y))
		#zI = as.logical(.Call("R_point_in_polygon_sp", as.numeric(ev0$X), as.numeric(ev0$Y), 
		#	as.numeric(polyAsub$X), as.numeric(polyAsub$Y), PACKAGE = "sp"))
		eval(call(text="zI = as.logical(.Call(\"R_point_in_polygon_sp\", as.numeric(ev0$X), as.numeric(ev0$Y), 
			as.numeric(polyAsub$X), as.numeric(polyAsub$Y), PACKAGE = \"sp\"))"))
		evI = ev0[zI,]
		areaI = sum(sapply(split(evI$area,evI$ID),mean))
		areaN = c(areaN,areaI)
	}
	areaN = c(areaN,sum(areaN))
	label = polyID
	pAdat = attributes(polyA)$PolyData
	if (!is.null(pAdat) && !is.null(pAdat$label)) 
		label = pAdat$label[is.element(pAdat$PID,polyID)]
	names(areaN) = c(label,paste(label,collapse="+"))
	attr(events,"area") = areaN
	save("events", file=paste("events-",as.character(substitute(polyA)),"(",paste(label,collapse="+"),").rda",sep=""))
	junk = gc(verbose=FALSE)
	return(areaN)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~calcGAP


## combineEventsQuickly-----------------2019-04-10
## Combine measurements of events using a quicker
## technique than that used in PBSmapping.
## ------------------------------------------NB/RH
combineEventsQuickly <- function(events, locs, FUN, ..., bdryOK=TRUE, dig=NULL)
{
	events <- .validateEventData(events);
	if (is.character(events))
		stop(paste("Invalid EventData 'events'.\n", events, sep=""));
	if (!is.element("Z", names(events))) {
		stop (
"EventData is missing required column 'Z'.\n");
	}

	## filter the boundary points (this can still be slow if used)
	if (!bdryOK)
		locs <- locs[is.element(locs$Bdry,0), ];

	## make the list...
	if (is.element("SID", names(locs))) {
		colIDs <- 2;
		colNames <- c("PID", "SID", "Z");
		#locs <- split(locs$EID, paste(locs$PID, locs$SID, sep = "-")) ## old in PBSmapping
		if (is.null(dig))
			dig = .createFastIDdig(locs, cols=c("PID","SID"))
		locs$ID = .createIDs(locs, c("PID","SID"), fastIDdig=dig)
	} else {
		colIDs <- 1; dig=0
		colNames <- c("PID", "Z");
		locs$ID = locs$PID
	}
	elocs = sapply(split(locs$ID,locs$EID),function(x){x}) ## IDs named by EID
	events$ID = elocs[as.character(events$EID)]
	ilocs = split(events$Z, events$ID)
	summary = lapply(ilocs, function(x, FUN, ...){FUN(x, ...)}, FUN, ...)

	## The as.numeric function calls (below) are very important.	They prevent
	## elements in the PID and SID columns from becoming factors.
	#output <- as.numeric(unlist(strsplit(names(summary), "-"))); ##old

	## .createIDs() will render SID=50 to 0.5 (i.e., loses zeroes)
	charID <- strsplit(names(summary),  paste0("\\",options()$OutDec))
	if (dig>0) charID = lapply(charID,function(x){if(nchar(x[2])<dig) c(x[1], paste0(x[2],paste0(rep(0,dig-nchar(x[2])),collapse=""))) else x})
	output <- as.numeric(unlist(charID))
	output <- as.data.frame(matrix(output, byrow = TRUE, ncol = colIDs));

	output <- cbind(output, unlist(summary));
	names(output) <- colNames;
	rownames(output) = sapply(charID,function(x){if (length(x)==1) x else paste0(x,collapse=options()$OutDec)})

	if (!is.null(output) && !is.PolyData(output, fullValidation = FALSE))
		class(output) <- c("PolyData", class(output));

	return(output);
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~combineEventsQuickly


## colTarg------------------------------2012-09-18
## Colour points depending on target field from qualified data.
## ---------------------------------------------RH
colTarg=function(dat=testdatC, tfld="cfv", qfld="year", qval=NULL,
   clrs=c("red","orange","yellow","green","blue"),
   border=0, add=TRUE, ...)
{
	datnam=as.character(substitute(dat))
	getFile(datnam); dat=get(datnam)
	if (is.null(qval)) qval=sort(unique(dat[,qfld]))
	stab = eval(parse(text=paste("with(dat,table(",qfld,",",tfld,"))",sep="")))
	Ntar = 1:dim(stab)[2]; names(Ntar)=dimnames(stab)[[2]]
	Ctar = rep(clrs,length(Ntar))[Ntar]; names(Ctar) = names(Ntar) # colours of all potential targets
	
	tdat = dat[is.element(dat[,qfld],qval),]
	if (nrow(tdat)==0) stop("No records when '",qfld,"' = ",deparse(qval),sep="")
	targ = tdat[,tfld] # targets given qfld's qval
	tdat$col = Ctar[as.character(targ)]
	tdat = as.EventData(tdat,projection="LL")

	if (!add) plotMap(tdat,type="n") # only plot this if there is no map plot to add points to
	addPoints(tdat,pch=21,col=border,bg=tdat$col,...)
	xlim=par()$usr[1:2]; ylim=par()$usr[3:4]
	invisible(list(tdat=tdat,Ctar=Ctar,qval=qval,xlim=xlim,ylim=ylim)) # returns list object of target data, colour vector, and unique qualifiers
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~colTarg


## sumT---------------------------------2019-04-11
## Summary function for summing catch in tonnes.
## ---------------------------------------------RH
sumT = function(x)
{
	sum(x[is.finite(x)]) / 1000.
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~sumT


## ttget--------------------------------2013-01-23
## Provide wrappers for PBSmodelling functions tget/tcall/tprint/tput/lisp
## ---------------------------------------------RH 
xtget   = function(...) {tget  (..., penv=parent.frame(), tenv=.PBSmapxEnv)}
xtcall  = function(...) {tcall (..., penv=parent.frame(), tenv=.PBSmapxEnv)}
xtprint = function(...) {tprint(..., penv=parent.frame(), tenv=.PBSmapxEnv)}
xtput   = function(...) {tput  (..., penv=parent.frame(), tenv=.PBSmapxEnv)}
xlisp   = function(...) {lisp  (..., pos =.PBSmapxEnv)}


## .fixNumIDs---------------------------2019-04-10
## Quick fixer-upper to rectify `.createIDs' handling of SIDs
## ---------------------------------------------RH
.fixNumIDs = function(x, dig)
{
	#as.character(show0(x,dig,add2int=TRUE)) ## 'show0' screwing things up -- assumes decimal = "."
	formatC(x, digits=dig, format="f")
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.fixNumIDs


## .win.get.lang------------------------2019-04-10
## Get language(s) for use in figure files.
## ---------------------------------------------RH
.win.get.lang = function()
{
	old.LANG = xtget(LANG)
	LANG = getWinVal()$uselang
	if (is.null(LANG) || LANG=="" || !any(LANG))
		lang = "e"
	else
		lang = names(LANG)[LANG]
	xtput(LANG) ## save for next call to this function
	.change = is.null(old.LANG) || !all(LANG==old.LANG)
	xtput(.change)
	return(lang)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.win.get.lang

## functions called from window description files
.win.map.exit = function(){ xtcall(.map.exit)() }


