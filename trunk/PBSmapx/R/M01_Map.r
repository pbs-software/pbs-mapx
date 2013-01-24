#createMap------------------------------2013-01-23
# Map wrapper for plotting PBS maps using a GUI.
#-----------------------------------------------RH
createMap = function(hnam=NULL,...) {
	if (exists(".coast",envir=.PBSmapxEnv)) rm(.coast,pos=.PBSmapxEnv)
	options(warn=-1)
	pset=c("isobath","major","minor","locality","srfa","popa",
		"hsgrid","hsisob","ltsa","qcssa","wcvisa") # PolySets
	dset=c("spn","testdatC","testdatR")           # Datasets
	data(list=pset); data(list=dset)
	PBSmap=list(module="M01_Map", call=match.call(), plotname="Rplot", pset=pset, disproj="LL",
		bvec0=rep(FALSE,9), isob0=rep(FALSE,18), hsi0=rep(FALSE,2), dis0=rep(FALSE,6), AofO=NA)
	assign("PBSmap",PBSmap,envir=.PBSmapxEnv)
	# Monitor GUI values:
	cmon = c("cnam","projection","zone")                               # coast line file
	Qmon = c("fnam","xlim","ylim","zlim","dlim","strSpp","zfld","fid") # Qfile (qualified fishing data)
	emon = c(Qmon,"bg","fg","eN","bo")                                 # events
	pmon = c(emon,"cells","fn","Vmin","Q","Flevs","track")             # pdata
	vval = c("nMix")                                                   # non-GUI values for events
	pval = c("clrs","brks","AREA","AofO")                              # non-GUI values for pdata
	packList(c("cmon","Qmon","emon","pmon","vval","pval"),"PBSmap",tenv=.PBSmapxEnv)
	.map.getCoast(cnam="nepacLL")

	pdir = system.file(package="PBSmapx")
	wdir = paste(pdir,"/win",sep=""); sdir = paste(pdir,"/sql",sep="")
	rtmp = tempdir(); rtmp = gsub("\\\\","/",rtmp)
	wnam = paste(wdir,"mapWin.txt",sep="/");  wtmp = paste(rtmp,"mapWin.txt",sep="/")
	snam = paste(sdir,"pht_map.sql",sep="/"); stmp = paste(rtmp,"pht_map.sql",sep="/")
	temp = readLines(wnam)
	temp = gsub("@wdf",wtmp,temp)
	temp = gsub("@sql",stmp,temp)
	if (!is.null(hnam) && is.character(hnam))
		temp = gsub("#import=",paste("import=\"",hnam,"\"",sep=""),temp)
	writeLines(temp,con=wtmp)
	file.copy(snam,stmp)
	eval(parse(text=".PBSmod$.options$par.map <<- list(...)"))
	# R-2.14.0 appears to implement windows buffering, which can screw the interactive nature of 'createMap'
	winbuf = windows.options()$buffered
	eval(parse(text=
		paste("assign(\".map.exit\",function(){windows.options(buffered=",winbuf,")},tenv=.PBSmapxEnv)",sep="")))
	windows.options(buffered=FALSE)
	resetGraph()
	createWin(wtmp)
	if (is.null(hnam) || !is.character(hnam)) .map.map()
}
#----------------------------------------createMap

#.map.map-------------------------------2013-01-23
# Controls the flow of mapping.
#-----------------------------------------------RH
.map.map = function(addA=FALSE,addI=FALSE,addG=FALSE,addT=FALSE,addB=FALSE,addC=FALSE,addL=FALSE,...) {
	.map.checkCoast()
	getWinVal(winName="window",scope="L")
	unpackList(xtcall(PBSmap),scope="L")

	spp  = eval(parse(text=paste("c(\"",gsub(",","\",\"",strSpp),"\")",sep="")))
	bvec = c(m1,m2,m3,m4,m5,s1,s2,s3,s4); # logical boundary vector
	#crap = colorRamp(c("white",bg,fg,"black"),space="Lab") # now using frap in fcell.
	# note: it now seems necessary to use colorRamp within the function and cannot be passed to other functions ???
	# colour ramp function bounded by white and black
	expr=paste("crap=function(colors=c(\"",bg,"\",\"",fg,"\")){",
		"rfun=colorRampPalette(c(\"white\",colors,\"black\"),space=\"Lab\"); ",
		"return(rfun) }",sep=""); eval(parse(text=expr))
	act  = getWinAct()[1]; eps=pix=wmf=FALSE
	if (!is.null(act) && act=="eps") eps = TRUE
	if (!is.null(act) && act=="pix") pix = TRUE
	if (!is.null(act) && act=="wmf") wmf = TRUE
	redraw = TRUE;

	dis1 = c(disG,disT,disB,disC,disL,disA); # current displays for species
	if (length(dis1)!=length(dis0)) 
		showError("Programmer Alert!\n\nInitalize 'dis0' to match 'dis1' on line 51")
	if (projection==disproj) {
	if ( all((bvec-bvec0)>=0) && all((hsi-hsi0)>=0) && all((isob-isob0)>=0) && all((dis1-dis0)>=0) ) { # no removals
		if (any(c(addA,addI,addG,addT,addB,addC,addL)==TRUE)) {
			shapes = list()
			if (addG && disG) { .map.checkGrid();   shapes$grid = TRUE } 
			if (addC && disC) { .map.checkGrid(); .map.checkQfile(); .map.checkEvents(); .map.checkCells();  shapes$cell = TRUE } 
			if (addT && disT) { .map.checkQfile(); .map.checkEvents(); shapes$tows = TRUE } 
			if (addB && disB) { .map.checkQfile(); .map.checkEvents(); shapes$bubb = TRUE } 
			if (addA) { shapes$bdry = TRUE }
			if (addI && any(hsi==TRUE) )  { shapes$hiso = TRUE }
			if (addI && any(isob==TRUE) ) { shapes$ziso = TRUE }
			if ((addL | addT | addB |addC) && (disL | disA)) { shapes$lege = TRUE }
			unpackList(xtcall(PBSmap),scope="L")
			redraw = FALSE; .map.addShapes(shapes) } }
	}
	packList(c("crap","eps","pix","wmf"),"PBSmap",tenv=.PBSmapxEnv)
	bvec0=bvec; dis0=dis1; hsi0=hsi; isob0=isob
	packList(c("bvec0","dis0","hsi0","isob0"),"PBSmap",tenv=.PBSmapxEnv)

	if (redraw) {
		unpackList(getPBSoptions("par.map"),scope="L")
		if (!isThere("plt"))      plt      = c(0.08,0.99,0.08,0.99)
		if (!isThere("mgp"))      mgp      = c(2.75,0.5,0)
		if (!isThere("las"))      las      = 0
		if (!isThere("cex.axis")) cex.axis = 1.2
		if (!isThere("cex.lab"))  cex.lab  = 1.75
		if (!isThere("cex.txt"))  cex.txt  = 0.9
		if (!isThere("cex.leg"))  cex.leg  = 0.9; 
		if (!isThere("tit.loc"))  tit.loc  = "topright"
		if (!isThere("leg.loc"))  leg.loc  = c(0.025,0.05)
		if (!isThere("leg.font")) leg.font = 2
		pin = par()$pin  # assumes plot is already on screen
		pin[1] = pin[1]/diff(plt[1:2]); pin[2] = pin[2]/diff(plt[3:4]) # adjust plot dimensions to accommodate 'plt' boundaries
		setPBSoptions("par.map", list(plt=plt, mgp=mgp, las=las, cex.axis=cex.axis, cex.lab=cex.lab,
			cex.txt=cex.txt, cex.leg=cex.leg, pin=pin, tit.loc=tit.loc, leg.loc=leg.loc, leg.font=leg.font))

		onam = match(zfld,c("catch","cat","C","effort","eff","E","CPUE","cpue","U"))
		if (is.na(onam)) onam = "X" else onam = switch(onam,"C","C","C","E","E","E","U","U","U")
		onam = paste(onam,"-",spp,"-d(",paste(substring(gsub("-","",dlim),3),collapse="-"),")",sep="")
		onam = paste(onam,"-z(",zlim[1],"-",zlim[2],")",sep="")
		if (any(fid))
			onam = paste(onam,"-fid(",paste(names(fid)[fid],collapse=""),")",sep="")
		if (pix) { PIN = 7.5 * pin/max(pin)
			#png(filename=paste(onam,".png",sep=""),units="in",width=PIN[1],height=PIN[2],res=200) }
			png(filename=paste(onam,".png",sep=""),width=round(100*PIN[1]),height=round(100*PIN[2])) }
		if (wmf) { PIN = 10 * pin/max(pin)
			win.metafile(filename=paste(onam,".wmf",sep=""),width=PIN[1],height=PIN[2]) }
		if (eps) { PIN = 10 * pin/max(pin)
			postscript(file=paste(onam,".eps",sep=""),width=PIN[1],height=PIN[2],fonts="mono") }
		coast = clipPolys(xtcall(.coast),xlim=xlim,ylim=ylim)
		if (is.null(coast) || nrow(coast)==0) { # create a box that is essentially a hole (piece of ocean)
			atts=attributes(xtcall(xtcall(.coast)))[setdiff(names(attributes(xtcall(.coast))),c("names","row.names","class"))] # extra attributes
			coast=as.PolySet(data.frame(
			PID=rep(1,8), SID=rep((1:2),each=4), POS=c(1:4,4:1),
			X=c(xlim[1],xlim[1],xlim[2],xlim[2],xlim[1],xlim[2],xlim[2],xlim[1]),
			Y=c(ylim[1],ylim[2],ylim[2],ylim[1],ylim[1],ylim[1],ylim[2],ylim[2]) ) )
			for (i in names(atts))
				attr(coast,i)=atts[[i]]
		}
		plotMap(coast,xlim=xlim,ylim=ylim,plt=plt,mgp=mgp,las=las,cex.axis=cex.axis,cex.lab=cex.lab,col="transparent")
		if (any(c(bvec,isob,hsi,disG,disT,disB,disC)==TRUE)) {
			if (any(c(disT,disB,disC)==TRUE)) { # display tows, bubbles, cells
				.map.checkMfile(); .map.checkQfile(); }
			shapes = list()
			if (any(bvec==TRUE)) { shapes$bdry = TRUE }
			if (any(hsi==TRUE))  { shapes$hiso = TRUE }
			if (any(isob==TRUE)) { shapes$ziso = TRUE }
			if (disG) { .map.checkGrid();   shapes$grid = TRUE }
			if (disT) { .map.checkEvents(); shapes$tows = TRUE }
			if (disB) { .map.checkEvents(); shapes$bubb = TRUE }
			if (disC) { .map.checkGrid(); .map.checkEvents(); .map.checkCells();  shapes$cell = TRUE }
			if (length(shapes)>0) unpackList(xtcall(PBSmap),scope="L")
			.map.addShapes(shapes) }
		addPolys(coast,col=land)
		.map.addAxis()
		if (disL|disA) { .map.addShapes(list(lege=TRUE)) }
		disproj = projection
		packList("disproj","PBSmap",tenv=.PBSmapxEnv)
		box() }
	# If comma-delimited file exists with fields EID, X, Y, and label, use 'addLabels' with placement = "DATA".
	if (file.exists("pbs.lab")) {
		pbs.lab = as.EventData(read.csv("pbs.lab"),projection="LL")
		if (disproj!="LL") pbs.lab=convUL(pbs.lab)
		addLabels(pbs.lab,placement="DATA",adj=1,cex=1.2)
	}
	box()
	if (eps | pix | wmf) dev.off()
	invisible() }
#-----------------------------------------.map.map

#.map.getCoast--------------------------2013-01-23
# Get the coast file (e.g., 'nepacLL')
#-----------------------------------------------RH
.map.getCoast = function(cnam=NULL) { 
	expr = paste("getFile(",cnam,",use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); coast=get(\"",cnam,"\")",sep="")
	unpackList(xtcall(PBSmap),scope="L")
	if (!is.null(cnam)) { # essentially first call or direct call from command line
		eval(parse(text=expr))
		if (is.null(attributes(coast)$projection)) {
			if (any(coast$X<0)) attr(coast,"projection")="LL"
			else attr(coast,"projection")="UTM" }
		if (is.null(attributes(coast)$zone))  attr(coast,"zone")=9
		attr(coast,"cnam")=cnam
		assign(".coast",coast,envir=.PBSmapxEnv)
	} else {
		getWinVal(winName="window",scope="L") 
		cstname=attributes(xtcall(.coast))$cnam
		cstproj=attributes(xtcall(.coast))$projection
		if (cstname!=cnam) .map.getCoast(cnam)  # function calls itself with non-NULL 'cnam'
		if (cstproj!=projection) {
			if ((projection=="UTM" & any(xlim<0)) || (projection=="LL" & any(xlim>360))) {
				lims=as.PolySet(data.frame(PID=rep(1,4),POS=1:4,X=c(xlim,rev(xlim)),Y=rep(ylim,each=2)),
					projection=cstproj,zone=zone)
				crnr=convUL(lims); newl = apply(crnr, 2, range)
				xlim=round(newl[,"X"]); ylim=round(newl[,"Y"])
				cells=cells*ifelse(projection=="UTM",100,.01)
				setWinVal(list(xlim=xlim,ylim=ylim,cells=cells)) }
			for (i in pset) { # Change all georeferenced PolySets
				expr=paste("attr(",i,",\"zone\")<<-",zone,"; ",i,"<<-convUL(",i,")",sep="")
				eval(parse(text=expr)) } 
			assign(".coast",convUL(xtcall(.coast)),envir=.PBSmapxEnv)
			#for (i in cmon) attr(coast,i) = get(i)
		}
	}
}
#------------------------------------.map.getCoast


#.map.mfile-----------------------------2013-01-23
# Get the Master data file from one of many 
# potential sources, and standardise fields names.
#-----------------------------------------------RH
.map.mfile = function() { # Mfile = Master file read in from GUI
	getWinVal(winName="window",scope="L")
	unpackList(xtcall(PBSmap),scope="L")
	eval(parse(text=paste("getFile(",fnam,",use.pkg=TRUE,try.all.frames=TRUE,tenv=penv())",sep="")))
	Mfile = get(fnam); flds = names(Mfile)
	STOP = function(msg="Stop due to error and set Mfile to NULL") {
		#eval(parse(text="PBSmap$Mfile <<- NULL"))
		xtget(PBSmap); PBSmap$Mfile <- NULL; xtput(PBSmap)
		showAlert(msg, title="Error", icon="error"); stop(msg,call.=FALSE) }
	if (!any(flds=="EID"))  Mfile$EID = 1:nrow(Mfile)
	if (!any(flds=="X"))    Mfile=.map.checkFlds(c("longitude","long","lon","x"),"X",Mfile)
	Mfile=Mfile[!is.na(Mfile$X),]
	if (!any(flds=="Y"))    Mfile=.map.checkFlds(c("latitude","lat","y"),"Y",Mfile)
	Mfile=Mfile[!is.na(Mfile$Y),]
	if (!any(flds=="X2"))   Mfile=.map.checkFlds(c("X","longitude","long","lon","x"),"X2",Mfile)
	if (!any(flds=="Y2"))   Mfile=.map.checkFlds(c("Y","latitude","lat","y"),"Y2",Mfile)
	if (!any(flds=="fdep")) Mfile=.map.checkFlds(c("depth","depth1","fishdepth","Z","z"),"fdep",Mfile)
	if (!any(flds=="cfv"))  Mfile=.map.checkFlds(c("CFV","vessel","boat","ship"),"cfv",Mfile)
	if (!byC) 
		Mfile=.map.checkFlds(c("spp","species","sp","hart","code"),"spp",Mfile)

	attr(Mfile,"last") = fnam;
	#packList(c("Mfile"),"PBSmap") # too slow
	#eval(parse(text="PBSmap$Mfile <<- Mfile"))
	xtget(PBSmap); PBSmap$Mfile <- Mfile; xtput(PBSmap)
	invisible() }
#---------------------------------------.map.mfile


#.map.qfile-----------------------------2013-01-23
# Qualify the Master file using various limits
#-----------------------------------------------RH
.map.qfile = function() { # Qfile = Qualified file from Master
	getWinVal(winName="window",scope="L")
	.map.checkMfile()
	unpackList(xtcall(PBSmap),scope="L")
	Qfile = Mfile;  flds = names(Qfile)
	STOP = function(msg="Stop due to error and set Qfile to NULL") {
		#eval(parse(text="PBSmap$Qfile <<- NULL"))
		xtget(PBSmap); PBSmap$Qfile <- NULL; xtput(PBSmap)
		showAlert(msg, title="Error", icon="error"); stop(msg,call.=FALSE) }
	cstproj=attributes(Qfile)$projection
	if (is.null(cstproj)) {
		if (any(Qfile$X>360)) cstproj="UTM" else cstproj="LL"
		attr(Qfile,"projection")=cstproj }
	attr(Qfile,"zone")=zone
	if (cstproj!=projection) {
		Qfile=convUL(Qfile)
		z=!is.na(Qfile$X2) & !is.na(Qfile$Y2)
		tmp=as.EventData(cbind(EID=Qfile$EID[z],X=Qfile$X2[z],Y=Qfile$Y2[z]),projection=cstproj,zone=zone)
		tmpUL=convUL(tmp)
		Qfile$X2[z]=tmpUL$X; Qfile$Y2[z]=tmpUL$Y }

	Qfile = Qfile[(Qfile$X>=xlim[1] | Qfile$X2>=xlim[1]) & (Qfile$X<=xlim[2] | Qfile$X2<=xlim[2]) & (!is.na(Qfile$X) | !is.na(Qfile$X2)),]
	if (nrow(Qfile)==0) STOP("No records in this longitude range")
	Qfile = Qfile[(Qfile$Y>=ylim[1] | Qfile$Y2>=ylim[1]) & (Qfile$Y<=ylim[2] | Qfile$Y2<=ylim[2]) & (!is.na(Qfile$Y) | !is.na(Qfile$Y2)),]
	if (nrow(Qfile)==0) STOP("No records in this latitude range")
	Qfile = Qfile[Qfile$fdep>zlim[1] & Qfile$fdep<=zlim[2] & !is.na(Qfile$fdep),]
	if (nrow(Qfile)==0) STOP("No records in this depth range")
	Qfile = Qfile[!is.na(Qfile$date),]
	Qfile$date = as.POSIXct(substring(Qfile$date,1,10)) 
	Qfile = Qfile[Qfile$date>=as.POSIXct(dlim[1]) & Qfile$date<=as.POSIXct(dlim[2]),]
	if (nrow(Qfile)==0) STOP("No records in this date range")
	if (is.element("fid",flds) && any(fid)) 
		Qfile = Qfile[is.element(Qfile$fid,names(fid)[fid]),]

	spp = eval(parse(text=paste("c(\"",gsub(",","\",\"",strSpp),"\")",sep="")))
	sppErr = function(SPP) {
		spplist=paste(SPP,spn[as.character(SPP)],sep=" - ")
		STOP(paste(c("Choose another species from:",spplist),collapse="\n")) }
	if (byC){
		spp = intersect(spp,flds)
		if (length(spp)==0) sppErr(SPP=names(spn[flds][!is.na(spn[flds])]))
		if (length(spp)==1) Qfile$catch = Qfile[,spp]
		else Qfile$catch = apply(Qfile[,spp],1,sum,na.rm=TRUE)
		spp = intersect(spp,flds) }
	else {
		SPP=sort(unique(Mfile$spp))
		if (all(spp=="*") | all(spp=="")) spp = SPP
		Qfile = Qfile[is.element(Qfile$spp,spp),]
		if (nrow(Qfile)==0) sppErr(SPP)
		if (!any(flds=="catch")) 
			Qfile=.map.checkFlds(c("cat","total","tcat","landed","land","kept","discarded","discards","discard","C"),"catch",Qfile)
		spp = sort(unique(Qfile$spp)); }
	setWinVal(winName="window",list(strSpp=paste(spp,collapse=","))) 
	if (!any(flds=="effort")) Qfile=.map.checkFlds(c("eff","duration","time","hours","minutes","E"),"effort",Qfile)
	if (!any(flds=="cpue")) {
		Qfile=.map.checkFlds(c("CPUE","cpue","U"),"cpue",Qfile)
		if (!any(names(Qfile)=="cpue")) Qfile$cpue=Qfile$catch/Qfile$effort }
	flds=names(Qfile)

	if (any(flds==zfld)) 
		Qfile$Z = Qfile[,zfld] 
	else
		STOP(wrapText(paste("Choose a Z-field from:\n",paste(flds,collapse=", "),sep=""),width=30,prefix="",exdent=5))

	for (i in Qmon) attr(Qfile,i) = get(i)
	packList(c("spp","SPP"),"PBSmap",tenv=.PBSmapxEnv)
	#eval(parse(text="PBSmap$Qfile <<- Qfile"))
	xtget(PBSmap); PBSmap$Qfile <- Qfile; xtput(PBSmap)
	invisible() }
#---------------------------------------.map.qfile


#.map.mgrid-----------------------------2013-01-23
# Make a grid for fcell
#-----------------------------------------------RH
.map.mgrid = function() {
	getWinVal(winName="window",scope="L")
	gx = seq(xlim[1],xlim[2],cells[1]); gy = seq(ylim[1],ylim[2],cells[2])
	agrid = makeGrid(x=gx,y=gy,projection=projection,zone=zone)
	#eval(parse(text="PBSmap$agrid <<- agrid"))
	xtget(PBSmap); PBSmap$agrid <- agrid; xtput(PBSmap)
	gcells = cells
	nMix = ifelse(eN==3,0,1) # if grid changes and tow position is set to blend, routine must remake events
	packList(c("gcells","nMix"),"PBSmap",tenv=.PBSmapxEnv)
	invisible() }


#.map.gevent----------------------------2013-01-23
# Get events from Qfile
#-----------------------------------------------RH
.map.gevent = function() {
	.map.checkMfile(); .map.checkQfile()
	getWinVal(winName="window",scope="L")
	unpackList(xtcall(PBSmap),scope="L")

	EID = eid = Qfile$EID;
	X   = Qfile$X;   Y  = Qfile$Y
	X2  = Qfile$X2;  Y2 = Qfile$Y2
	cfv = Qfile$cfv; eos = rep(1,length(EID))
	zXY = is.na(X2) | is.na(Y2); X2[zXY] = X[zXY]; Y2[zXY] = Y[zXY]
	Z   = Qfile$Z

	if (eN==1) { Xnew = X;  Ynew = Y ; nMix = 1 }
	if (eN==2) { Xnew = X2; Ynew = Y2; nMix = 1 }
	if (eN==3) {
		xmat = cbind(X,X2); ymat = cbind(Y,Y2)
		same = round(apply(xmat,1,diff),5)==0 & round(apply(ymat,1,diff),5)==0
		if (all(same==TRUE)) { 
			Xnew = X2; Ynew = Y2; }
		else {
			# Remember tows with no movement (start and end positions the same)
			eid0 = EID[same]; x0 = X[same]; y0 = Y[same]; z0= Z[same]; cfv0 = cfv[same]
			# Collect tows that have moved from start to end
			x1 = X[!same]; x2 = X2[!same]; y1 = Y[!same]; y2 = Y2[!same]
			eid1 = EID[!same]; z1 = Z[!same]; cfv1 = cfv[!same]

			zmat = data.frame(eid1,x1,x2,y1,y2,z1,cfv1); rownames(zmat) = eid1
			#zmat$towd = apply(zmat,1,function(x){ sqrt((x["x2"]-x["x1"])^2 + (x["y2"]-x["y1"])^2) })
			zmat$nMix = apply(zmat,1,function(x){ ceiling(sqrt(((x["x2"]-x["x1"])/cells[1])^2 + ((x["y2"]-x["y1"])/cells[2])^2))+1 })
			#PBSmap$blend <<- array(NA,dim=c(sum(zmat$nMix),6),dimnames=list(EID=1:sum(zmat$nMix),val=c("eid","Xnew","Ynew","Z","cfv","eos")))
			#eval(parse(text="PBSmap$blend <<- array(NA,dim=c(sum(zmat$nMix),6),dimnames=list(EID=1:sum(zmat$nMix),val=c(\"eid\",\"Xnew\",\"Ynew\",\"Z\",\"cfv\",\"eos\")))"))
			#eval(parse(text="PBSmap$tally <<- 0"))
			xtget(PBSmap)
			PBSmap$blend <- array(NA,dim=c(sum(zmat$nMix),6),dimnames=list(EID=1:sum(zmat$nMix),val=c("eid","Xnew","Ynew","Z","cfv","eos")))
			PBSmap$tally <- 0
			xtput(PBSmap)
			apply(zmat,1,function(x){
				unpackList(x,scope="L")
				eid  = rep(eid1,nMix)
				Xnew = seq(x1,x2,len=nMix)
				Ynew = seq(y1,y2,len=nMix)
				# distribute Z uniformly or normally between start and end position tow
				switch( bo,
					{pZ = rep(1.,nMix)/nMix}, # uniform
					# normal: create a weighting vector dZ, convert to proportions pZ to distribute Z along the nMix vertices
					{dZ = dnorm(seq(-3,3,len=nMix)); pZ = dZ/sum(dZ)} )
				Z   = z1*pZ
				cfv = rep(cfv1,nMix)
				eos = 1:nMix
				#eval(parse(text="PBSmap$tally <<- PBSmap$tally + nMix"))
				#eval(parse(text="PBSmap$blend[(PBSmap$tally-nMix+1):PBSmap$tally,]<<-cbind(eid,Xnew,Ynew,Z,cfv,eos)"))
				xtget(PBSmap)
				PBSmap$tally <- PBSmap$tally + nMix
				PBSmap$blend[(PBSmap$tally-nMix+1):PBSmap$tally,] <- cbind(eid,Xnew,Ynew,Z,cfv,eos)
				xtput(PBSmap)
				invisible()
				})
			#eval(parse(text="PBSmap$blend <<- as.data.frame(PBSmap$blend)"))
			xtget(PBSmap); PBSmap$blend <- as.data.frame(PBSmap$blend); xtput(PBSmap)
			unpackList(xtcall(PBSmap)$blend,scope="L")
			EID=1:length(c(eid,eid0)); eid=c(eid,eid0); Xnew=c(Xnew,x0); Ynew=c(Ynew,y0)
			Z=c(Z,z0); cfv=c(cfv,cfv0); eos=c(eos,rep(1,length(eid0)))
		}
	}
	events = data.frame(EID=EID,X=Xnew,Y=Ynew,Z=Z,cfv=cfv,eid=eid,eos=eos)
	zev    = !is.na(events$X) & !is.na(events$Y) & !is.na(events$Z) & !is.infinite(events$Z)
	events = events[zev,]
	zev    = is.na(events$cfv); if (length(zev)>0) events$cfv[zev] = 9999
	events=as.EventData(events,projection=projection,zone=zone)
	nMix = max(events$eos)

	packList(c("nMix"),"PBSmap",tenv=.PBSmapxEnv)  # need these to monitor
	for (i in emon) attr(events,i) = get(i)
	for (i in vval) attr(events,i) = get(i)
	#packList("events","PBSmap") # too slow
	#eval(parse(text="PBSmap$events <<- events"))
	xtget(PBSmap); PBSmap$events <- events; xtput(PBSmap)
	invisible() }
#--------------------------------------.map.gevent


#.map.fcell-----------------------------2013-01-23
# Find events in cells
#-----------------------------------------------RH
.map.fcell = function(OK=TRUE) {
	.map.checkMfile(); .map.checkQfile(); .map.checkGrid(); .map.checkEvents()
	getWinVal(winName="window",scope="L")
	unpackList(xtcall(PBSmap),scope="L")

	# fishing vessels in all events (including expanded)
	vess = events[,c("EID","X","Y","cfv")]; names(vess)[4] = "Z"
	# unique fishing events in tows
	fevs = events[is.element(events$eos,1),c("EID","X","Y","cfv")]; names(fevs)[4] = "Z"
	LocData = findCells(events,agrid); locClass=attributes(LocData)$class
	# Get rid of events duplicated on boundaries
	locData=LocData[order(LocData$EID),]
	zD=duplicated(locData$EID)     # find duplicated events
	locData=locData[!zD,]          # get the unique events (not duplicated)
	attr(locData,"class")=locClass

	pdata=Pdata=combineEvents(events,locData,FUN=get(fn))                   # Summarize Z
	tdata = combineEvents(fevs,locData,FUN=length)                          # Total number of original tows
	xdata = combineEvents(events,locData,FUN=length)                        # Total number of expanded tows
	vdata = combineEvents(vess,locData,FUN=function(x){length(unique(x))} ) # Number of vessels (cfv)

	dig=.createFastIDdig(agrid,cols=c("PID","SID")) # sometimes # grid colums overwhelms # of events
	rownames(pdata) = .createIDs(pdata,c("PID","SID"),fastIDdig=dig)
	pdata$vess = pdata$xtow = pdata$tows = rep(0,nrow(pdata))
	tows=tdata$Z; names(tows)=.createIDs(tdata,c("PID","SID"),fastIDdig=dig)
	pdata[names(tows),"tows"] = tows
	xtow=xdata$Z; names(xtow)=.createIDs(xdata,c("PID","SID"),fastIDdig=dig)
	pdata[names(xtow),"xtow"] = xtow
	vess=vdata$Z; names(vess)=.createIDs(vdata,c("PID","SID"),fastIDdig=dig)
	pdata[names(vess),"vess"] = vess

	Zkeep=rep(TRUE,nrow(pdata))
	if (ex0)   Zkeep=Zkeep & (round(pdata$Z,5)!=0 & !is.na(pdata$Z)) # exclude zeroes
	if (exneg) Zkeep=Zkeep & (pdata$Z>=0 & !is.na(pdata$Z))          # exclude negative values
	pdata=pdata[Zkeep,]
	Vmax  = max(pdata$vess,na.rm=TRUE)
	if (Vmin>Vmax) {
		setWinVal(winName="window",list(Vmin=Vmax))
		showError("Choose a smaller minimum # cfvs") }
	pdata$vsee = vsee = is.element(pdata$vess,Vmin:Vmax)  # vessel minimum/cell allowed to display
	switch ( Q,
		{
		qq = c(0,0.5,0.75,0.9,0.95,1)
		brks = quantile(pdata$Z,qq)
		if (any(duplicated(brks))) {
			qq = c(0,0.6,0.9,1)
			brks = quantile(pdata$Z,qq)
			if (any(duplicated(brks))) {
				qq = c(0,0.8,1)
				brks = quantile(pdata$Z,qq)
				if (any(duplicated(brks))) {
					qq = c(0,1)
					brks = quantile(pdata$Z,qq)
		}	}	}	}, {
		nq = 6
		for (i in nq:2) {
			qq = seq(0,1,len=i)
			brks = quantile(pdata$Z,qq)
			if (any(duplicated(brks))) next else break
		}	},
		{brks = Flevs}
	)
	nclr  = length(brks)-1
	frap  = colorRamp(c("white",bg,fg,"black"),space="Lab") # colour ramp function bounded by white and black
	clrs  = apply(frap(seq(.15,.9,len=nclr)),1,function(x){rgb(x[1],x[2],x[3],maxColorValue=255)})
	pdata = makeProps(pdata,breaks=brks,propName="lev",propVals=1:nclr)
	pdata = makeProps(pdata,breaks=brks,propName="col",propVals=clrs)

	#--- Calculate area (km²) for each cell
	pdata$area = rep(0,nrow(pdata))
	idp  = .createIDs(pdata,c("PID","SID"),fastIDdig=dig)
	ida  = .createIDs(agrid,c("PID","SID"),fastIDdig=dig)
	temp = agrid[is.element(ida,idp),]
	atmp = calcArea(temp)
	area=atmp$area; names(area)=.createIDs(atmp,c("PID","SID"),fastIDdig=dig)
	pdata[names(area),"area"] = area
	AREA = rep(0,nclr); names(AREA)=1:nclr
	areasum = sapply(split(pdata$area,pdata$lev),sum)  # split: missing values in f are dropped together with the corresponding values of x
	AREA[names(areasum)] = areasum
	AofO = sum(AREA,na.rm=TRUE)
	#--- End area calculation

	packList(c("clrs","brks","AREA","AofO"),"PBSmap",tenv=.PBSmapxEnv)  # need these to monitor
	for (i in pmon) attr(pdata,i) = get(i)
	for (i in pval) attr(pdata,i) = get(i)

	pid = .createIDs(pdata[vsee,],c("PID","SID"))
	zlev = !is.na(pdata$lev) # within the breaks and permitted to see
	tows = c(sum(pdata$tows[zlev]),sum(tdata$Z[zlev&vsee]),sum(tdata$Z[zlev&!vsee]))
	attr(tdata,"tows") = tows

	index=.createIDs(locData,c("PID","SID"))
	names(index)=locData$EID
	Qfile$index=rep(NA,nrow(Qfile))
	Qfile$index=index[as.character(Qfile$EID)]
#print(track); print(sys.nframe())
	if (is.element(track,names(Qfile))) {
		kid=is.element(Qfile$index,pid)
		tracked=Qfile[,track][kid]
		names(tracked)=Qfile$index[kid] 
		attr(tracked,"unique")=sort(unique(tracked)) }
	else tracked="No matching fields in Qfile"
	
	#eval(parse(text="PBSmap$LocData <<- LocData; PBSmap$locData <<- locData"))
	xtget(PBSmap); PBSmap$LocData <- LocData; PBSmap$locData <- locData; xtput(PBSmap)
	stuff=c("Pdata","pdata","tdata","xdata","vdata","tracked","index")
	packList(stuff,"PBSmap",tenv=.PBSmapxEnv) 
	setWinVal(winName="window",list(strSpp=paste(spp,collapse=","),Vmax=Vmax)) }
#---------------------------------------.map.fcell


#.map.checkFlds-------------------------2010-10-19
# Check fields for avaialbility & create standardised fields.
#-----------------------------------------------RH
.map.checkFlds = function(badnames,goodname,dat) {
	allnames = names(dat)
	for (i in badnames) {
		if (any(allnames==i)) {
			dat[,goodname] = dat[,i]
			if (any(goodname==c("X","X2"))) { 
				badX=dat[,goodname]>0 & !is.na(dat[,goodname])
				dat[,goodname][badX]=-dat[,goodname][badX] }
			if (any(goodname==c("spp")))
				dat[,goodname]=as.character(dat[,goodname]) 
			if (any(goodname==c("effort"))) {
				if (median(dat[,goodname],na.rm=TRUE)>24) dat[,goodname]=dat[,goodname]/60 } # convert min to h 
			return(dat) }
	}
	if (!any(allnames=="effort")) { dat$effort=rep(1,nrow(dat));   return(dat) }
	if (any(goodname=="cpue"))    { dat$cpue=dat$catch/dat$effort; return(dat) }
	showError(paste("'",paste(c(goodname,badnames),collapse="', '"),"'",sep=""),"nofields")
}

#.map.checkLims-------------------------2012-02-17
# Check that second limit is >= the first limit.
#-----------------------------------------------RH
.map.checkLims <- function(limObj) {
	limName = as.character(substitute(limObj))
	limObj1 = limObj[1]
	limObj2 = rev(limObj)[1]
	if (limObj1 >= limObj2) showError(paste("Second <",limName,"> must be greater than the first.",sep=""))
	invisible() }

#.map.checkCoast------------------------2013-01-23
# Check to see if the coast file needs to change.
#-----------------------------------------------RH
.map.checkCoast = function(change=FALSE){ 
	getWinVal(winName="window",scope="L")
	for (i in c("xlim","ylim","zlim","dlim"))
		eval(parse(text=paste(".map.checkLims(",i,")",sep="")))
	if (!exists(".coast",envir=.PBSmapxEnv)) {
		.map.catf("\nNew Coast\n")
		.map.getCoast("nepacLL") }
	else {
		change = .map.changeC(xtcall(PBSmap)$cmon)
		if (change) { .map.catf("Coast changed\n"); .map.getCoast() } } }

#.map.checkMfile------------------------2013-01-23
# Check to see if a new master file is needed.
#-----------------------------------------------RH
.map.checkMfile = function() {
	getWinVal(winName="window",scope="L")
	Mfile = xtcall(PBSmap)$Mfile
	if (is.null(Mfile) || attributes(Mfile)$last != fnam) {
		.map.catf("\nNew Mfile\n")
		#eval(parse(text="PBSmap$Qfile <<- NULL")); .map.mfile() }
		xtget(PBSmap); PBSmap$Qfile <- NULL; xtput(PBSmap)
		.map.mfile() }
	invisible() }

#.map.checkQfile------------------------2013-01-23
# Check to see if GUI settings match those of the current file.
#-----------------------------------------------RH
.map.checkQfile = function(change=FALSE){ 
	getWinVal(winName="window",scope="L")
	Qfile = xtcall(PBSmap)$Qfile;
	if (is.null(Qfile)) { .map.catf("\nNew Qfile\n"); .map.qfile() }
	else {
		change = .map.changeW(xtcall(PBSmap)$Qmon,Qfile)
		if (change) { .map.catf("Qfile changed\n"); .map.qfile(); } } }

#.map.checkGrid-------------------------2013-01-23
# Check to see if GUI settings match those of the current file.
#-----------------------------------------------RH
.map.checkGrid = function(change=FALSE){ 
	getWinVal(winName="window",scope="L")
	agrid = xtcall(PBSmap)$agrid
	if (is.null(agrid)) { .map.catf("\nNew grid\n"); .map.mgrid() }
	else {
		if (!all((cells==xtcall(PBSmap)$gcells)==TRUE) ) change=TRUE 
		if (change) { .map.catf("grid changed\n"); .map.mgrid(); } } }

#.map.checkEvents-----------------------2013-01-23
# Check to see if GUI settings match those of the current file.
#-----------------------------------------------RH
.map.checkEvents = function(change=FALSE){ 
	getWinVal(winName="window",scope="L")
	events = xtcall(PBSmap)$events
	if (is.null(events)) { .map.catf("\nNew events\n"); .map.gevent() }
	else {
		change = .map.changeW(xtcall(PBSmap)$emon,events) | .map.changeV(xtcall(PBSmap)$vval,events)
		if (change) { .map.catf("events changed\n"); .map.gevent(); } } }

#.map.checkCells------------------------2013-01-23
# Check to see if GUI settings match those of the current file
#-----------------------------------------------RH
.map.checkCells = function(change=FALSE){
	getWinVal(winName="window",scope="L")
	pdata = xtcall(PBSmap)$pdata
	if (is.null(pdata)) { .map.catf("\nNew pdata\n"); .map.fcell() }
	else {
		change = .map.changeW(xtcall(PBSmap)$pmon,pdata) | .map.changeV(xtcall(PBSmap)$pval,pdata)
		if (change) { .map.catf("pdata changed\n"); .map.fcell(); } } }

#.map.addShapes-------------------------2013-01-23
# Add shapes to the plot; redraw if a shape has been removed.
#-----------------------------------------------RH
.map.addShapes = function(shapes=list()) { # 0=no click, 1=click added, -1=click removed
	#---See-Functions-begin----------
	seeGrid = function() {
		getWinVal(winName="window",scope="L")
		unpackList(xtcall(PBSmap),scope="L")
		addPolys(agrid,border="gray",density=0);  box(); }
	seeCell = function() { # add coloured cells
		getWinVal(winName="window",scope="L")
		unpackList(xtcall(PBSmap),scope="L")
		addPolys(agrid,polyProps=pdata[pdata$vsee,],border="#DFDFDF")
		box(); }
	seeTows = function() { # add event data as points or bubbles
		getWinVal(winName="window",scope="L")
		unpackList(xtcall(PBSmap),scope="L")
		edata = events[events$Z>0 & !is.na(events$Z),]
		addPoints(edata,col=bg,pch=16,cex=tsize)
		addPoints(edata,col=fg,pch=1,cex=tsize)
		box(); }
	seeBubb = function() { # add event data as points or bubbles
		getWinVal(winName="window",scope="L")
		unpackList(xtcall(PBSmap),scope="L")
		xwid = par()$pin[1]; size = psize*xwid;
		edata = events[events$Z>0 & !is.na(events$Z),]
		edata = edata[rev(order(edata$Z)),]
		symbols(edata$X,edata$Y,circles=edata$Z^powr,inches=size,fg=fg,bg=bg,add=TRUE)
		box(); }
	seeHiso = function () { 
		getWinVal(winName="window",scope="L")
		hbar = c(20,50)[hsi]; hclr = c("lightseagreen","darkgreen")[hsi]
		hfile = hsisob[is.element(hsisob$PID,hbar),]
		addLines(hfile,col=hclr)
		box() }
	seeZiso = function () { 
		getWinVal(winName="window",scope="L")
		irap = colorRamp(c("white",icol,"black"),space="Lab") # isobath ramp function bounded by white and black
		iclrs = apply(irap(seq(.2,.9,len=18)),1,function(x){rgb(x[1],x[2],x[3],maxColorValue=255)})
		zbar = seq(100,1800,100)[isob]; iclr = iclrs[isob]
		ifile = isobath[is.element(isobath$PID,zbar),]
		addLines(ifile,col=iclr)
		box(); }
	seeBdry = function(){
		getWinVal(winName="window",scope="L")
		mnam = c("major","minor","locality","srfa","popa")
		#mclr = c("blue","forestgreen","#FF8000","purple","red")
		mclr = c("red","forestgreen","#FF8000","purple","red") # temp for POP spatial
		snam = c("hsgrid","ltsa","qcssa","wcvisa")
		sclr = c("magenta","darkorange4","black","magenta")
		bvec = c(m1,m2,m3,m4,m5,s1,s2,s3,s4)
		bdry = c(mnam,snam)[bvec]; clrs = c(mclr,sclr)[bvec]; nb = length(bdry)
		if (nb>0) {
			for (i in 1:nb) addPolys(get(bdry[i]),border=clrs[i],density=0) }
		box() }
	seeLege = function () { 
		getWinVal(winName="window",scope="L")
		unpackList(xtcall(PBSmap),scope="L")
		# Top right info legend
		cap = NULL
		if (disL|disA) {
			cap = paste(spn[as.character(xtcall(PBSmap)$spp)],collapse="\n",sep="")
			if (is.null(cap) || is.na(cap) || cap=="NA" || cap=="")
				cap = toupper(as.character(xtcall(PBSmap)$spp)) }
		if (disL) cap = paste(c(cap,paste(dlim,collapse=" to ")),collapse="\n")
		if (disC && disA)
			cap = paste(c(cap,paste("Encountered area =",format(round(xtcall(PBSmap)$AofO),
				big.mark=",",scientific=FALSE),"km\262")),collapse="\n")
		legend(x=tit.loc,legend=cap,cex=cex.txt,adj=c(0,0.15),bg="aliceblue",text.col="black")

		# Grid cell legend
		if (disL && disC) {
			breaker = function(brks,sig=3,nx=length(brks)) {
				xform = sapply(brks,function(x){format(x,scientific=FALSE,digits=sig)})
				lab = paste(ifelse(ex0 & round(brks[1],5)==0,">",">="),xform[1]," & <=",xform[2],sep="")
				if (nx>3) {
					for (i in 2:(nx-2))
						lab = c(lab,paste(">",xform[i]," & <=",xform[i+1],sep="")) }
				lab = c(lab,paste(">",xform[nx-1],sep=""))
				return (lab) }
			brks = attributes(pdata)$brks
			nleg = floor(par()$pin[2]/(.8*par()$cin[2])); nQ = length(brks)
			leg0 = 1 - ((nleg-nQ-4)/nleg)
			llab = breaker(brks); nspace = max(nchar(llab))-nchar(llab)
			space= sapply(nspace,function(x){paste(rep.int(" ",x),collapse="",sep="")})
			AREA = attributes(pdata)$AREA;
			AREAformat = format(round(AREA),big.mark=",",scientific=FALSE)
			AREAblanks = sapply(AREAformat,function(x){paste(rep(" ",nchar(x)),collapse="",sep="")})
			if (disA) llab = paste(llab,space,AREAformat)
			else      llab = paste(llab,space,AREAblanks)
			packList(c("leg0","llab"),"PBSmap",tenv=.PBSmapxEnv)

			par(family="mono", font=leg.font) # note: cex must be even (or available) for 'mono' to work
			L1 = leg.loc[1]; L2 = leg.loc[2]
			addLegend(L1,L2,fill=attributes(pdata)$clrs,legend=llab,bty="n",cex=cex.leg,yjust=0,
				title=paste(fn,"(",zfld,")",paste(rep.int(" ",max(nspace)),collapse=""),ifelse(disA,"Area(km\262)","         "),sep="") )
			par(family="", font=1)

			if (all(fid)) fisheries = "  (trawl + H&L)"
			else {
				FID = c("trawl","halibut","sable","dog/lin","HLrock")
				fisheries = paste("  (",paste(FID[fid],collapse="+"),")",sep="") }
			if (Vmin==1) {
				addLabel(L1+0.025,L2-0.02,paste("Events: ",format(attributes(tdata)$tows[1],big.mark=","),
					fisheries,sep=""),cex=cex.leg,col="grey30",adj=c(0,0)) 
			}
			else {
				mess = paste("\225 ",Vmin,"+ vessels/cell",sep="")
				mess = c(mess, paste("Events:",paste(paste(c("T","V","H"),  # Total, Visible, Hidden
					format(attributes(tdata)$tows,big.mark=",",trim=TRUE),sep="="),
					collapse="; ")))
				addLabel(L1+0.025,L2-0.03,paste(mess,collapse="\n",sep=""),cex=cex.leg,col="grey30",adj=c(0,0))
			}
		} 
		box(); }
	#---See-Functions-end------------

	unpackList(getPBSoptions("par.map"),scope="L")

	if (length(shapes)==0) return()
	sord = c("grid","cell","bubb","tows","ziso","hiso","bdry","lege")
	suse = sapply(shapes,function(x){x},simplify=TRUE)
#print(shapes);print(suse)
	snam = names(suse[suse])
	snam = sord[sort(match(snam,sord))]
	for (i in snam) {
		if (i=="grid")      seeGrid()
		else if (i=="cell") seeCell()
		else if (i=="tows") seeTows()
		else if (i=="bubb") seeBubb()
		else if (i=="bdry") seeBdry()
		else if (i=="hiso") seeHiso()
		else if (i=="ziso") seeZiso()
		else if (i=="lege") seeLege()
		else box() }
}
#-----------------------------------.map.addShapes


#.map.changeC---------------------------2013-01-23
# Have the settings to display the coast changed?
#-----------------------------------------------RH
.map.changeC = function(Cnam=NULL,change=FALSE) {
	Clist = getWinVal(winName="window",scope="L")[Cnam]
	if (!is.null(Clist)) {
		for (i in Cnam) {
			if (any(is.na(Clist[[i]])) || any(is.na(attributes(xtcall(.coast))[[i]])) || 
				!all((Clist[[i]]==attributes(xtcall(.coast))[[i]])==TRUE) ) change=TRUE  } }
	return(change) }

#.map.changeW---------------------------2010-10-19
# Have the settings of the data files changed?
#-----------------------------------------------RH
.map.changeW = function(Wnam=NULL,file=NULL,change=FALSE) {
	Wlist = getWinVal(winName="window",scope="L")[Wnam]
	if (!is.null(file)) {
		if (!is.null(Wlist)) {
			for (i in Wnam) {
				if (any(is.na(Wlist[[i]])) || any(is.na(attributes(file)[[i]])) || 
					!all((Wlist[[i]]==attributes(file)[[i]])==TRUE) ) change=TRUE } } }
	return(change) }

#.map.changeV---------------------------2013-01-23
# Have monitored shapes and settings changed?
#-----------------------------------------------RH
.map.changeV = function(Vnam=NULL,file=NULL,change=FALSE) {
	Vlist = xtcall(PBSmap)[Vnam]
	if (!is.null(file)) {
		if (!is.null(Vlist)) {
			for (i in Vnam) {
				if (any(is.na(Vlist[[i]])) || any(is.na(attributes(file)[[i]])) || 
					!all((Vlist[[i]]==attributes(file)[[i]])==TRUE) ) change=TRUE } } }
	return(change) }

.map.addArea  = function() { .map.map(addA=TRUE); }
.map.addIsob  = function() { .map.map(addI=TRUE); }
.map.addGrid  = function() { .map.map(addG=TRUE); }
.map.addTows  = function() { .map.map(addT=TRUE); }
.map.addBubb  = function() { .map.map(addB=TRUE); }
.map.addCell  = function() { .map.map(addC=TRUE); }
.map.addLege  = function() { .map.map(addL=TRUE); }

# Flush the cat
.map.catf = function(...) { cat(...); flush.console() }

#.map.reColour--------------------------2013-01-23
# Quick cell colour change
#-----------------------------------------------RH
.map.reColour = function(pdata=xtcall(PBSmap)$pdata) {
	getWinVal(winName="window",scope="L")
	if (!is.null(pdata)) {
		oldclrs = sapply(split(pdata$col,pdata$lev),unique); nclr = length(oldclrs)
		frap = colorRamp(c("white",bg,fg,"black"),space="Lab") # colour ramp function bounded by white and black
		clrs  = apply(frap(seq(.15,.9,len=nclr)),1,function(x){rgb(x[1],x[2],x[3],maxColorValue=255)})
		if (length(clrs)!=length(oldclrs)) return()
		attr(pdata,"clrs") = clrs
		names(clrs) = names(oldclrs)
		pdata$col=clrs[as.character(pdata$lev)]
		#eval(parse(text="PBSmap$pdata <<- pdata")) }
		xtget(PBSmap); PBSmap$pdata <- pdata; xtput(PBSmap)
	}
	.map.addShapes(list(tows=disT,bubb=disB,cell=disC,lege=disL))
	invisible() }

#.map.addAxis---------------------------2010-12-03
# Use PBSmapping's .addAxis function.
#-----------------------------------------------RH
.map.addAxis = function(xlim=par()$usr[1:2], ylim=par()$usr[3:4], 
     tckLab=FALSE, tck=0.014, tckMinor=0.5*tck, ...) {
	tckLab   = rep(tckLab,   length.out = 2)
	tck      = rep(tck,      length.out = 2)
	tckMinor = rep(tckMinor, length.out = 2)

	# 1 is the horizontal axis, 2 is the vertical axis
	lim = list(xlim, ylim)
	rotate = list(0, 90)
	for (i in 1:2) {
		if (((i == 1) && (par()$xaxt != "n")) || ((i == 2) && (par()$yaxt != "n"))) {
			# create both major and minor ticks
			ticks = pretty(lim[[i]])
			ticksMinor = pretty(c(0, diff(ticks)[1]))
			ticksMinor = (sort(rep(ticks, length(ticksMinor))) + rep(ticksMinor, length(ticks)))
			# filter them for anything on the extents
			ticks = ticks[ticks > lim[[i]][1] & ticks < lim[[i]][2]]
			ticksMinor = ticksMinor[ticksMinor > lim[[i]][1] & ticksMinor < lim[[i]][2]]
			if (!tckLab[i]) {
				tickLabels = FALSE
			} else {
				tickLabels = as.character(ticks)
			}
			# plot the major and minor axes
			axis(side = i, at = ticks, labels = tickLabels, tck = tck[i], srt = rotate[[i]])
			axis(side = i, at = ticksMinor, labels = FALSE, tck = tckMinor[i])
		}
	}
invisible(NULL)
}

