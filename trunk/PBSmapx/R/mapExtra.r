#===============================================================================
# mapExtra
# --------
#  calcGAP... .....Calculate the gridded area within a polygon.
#  colTarg ........Colour points depending on target field from qualified data.
#
#-----Supplementary hidden functions-----
#
#===============================================================================

#calcGAP--------------------------------2012-09-26
# Calculate the gridded area within a polygon.
# Uses output from 'createMap'.
#-----------------------------------------------RH
calcGAP = function(polyA,events,loc,pdata,polyID){
	# Populate the events data with PID and SID from a locData set
	pid = loc$PID; names(pid) = loc$EID
	sid = loc$SID; names(sid) = loc$EID
	events$PID = pid[as.character(events$EID)]
	events$SID = sid[as.character(events$EID)]
	events$ID = .createIDs(events,cols=c("PID","SID"),fastIDdig=3)
	ev0 = events[events$Z>0 & !is.na(events$Z),]

	# Add grid areas to the events data using a PolyData set
	pdata$ID = .createIDs(pdata,cols=c("PID","SID"),fastIDdig=3)
	area = pdata$area; names(area) = pdata$ID
	events$area = area[as.character(events$ID)]
	ev0 = events[events$Z>0 & !is.na(events$Z),]

	# calculate the area covered by grid cells within each polyID of polyA
	areaN = NULL
	for (i in polyID) {
		polyAsub = polyA[is.element(polyA$PID,i),]
		#zI = as.logical(sp::point.in.polygon(ev0$X,ev0$Y,polyAsub$X,polyAsub$Y))
		zI = as.logical(.Call("R_point_in_polygon_sp", as.numeric(ev0$X), as.numeric(ev0$Y), 
			as.numeric(polyAsub$X), as.numeric(polyAsub$Y), PACKAGE = "sp"))
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
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^calcGAP


#colTarg--------------------------------2012-09-18
# Colour points depending on target field from qualified data.
#-----------------------------------------------RH
colTarg=function(dat=testdatC, tfld="cfv", qfld="year", qval=NULL,
   clrs=c("red","orange","yellow","green","blue"),
   border=0, add=TRUE, ...) {

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
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^colTarg

