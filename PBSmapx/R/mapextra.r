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

