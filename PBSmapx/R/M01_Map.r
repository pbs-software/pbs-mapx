##==============================================================================
## M01_Map
## -------
##  createMap............Map wrapper for plotting PBS maps using a GUI.
##
##-----Supplementary hidden functions-----
## .map.map..............Controls the flow of mapping.
## .map.getCoast.........Get the coast file (e.g., 'nepacLL')
## .map.mfile............Get the Master data file from one of many potential sources, and standardise fields names.
## .map.qfile............Qualify the Master file using various limits.
## .map.mgrid............Make a grid for fcell.
## .map.gevent...........Get events from Qfile.
## .map.fcell............Find events in cells.
## .map.checkFlds........Check fields for avaialbility & create standardised fields.
## .map.checkLims........Check that second limit is >= the first limit.
## .map.checkCoast.......Check to see if the coast file needs to change.
## .map.checkMfile.......Check to see if a new master file is needed.
## .map.checkQfile.......Check to see if GUI settings match those of the current file.
## .map.checkGrid........Check to see if GUI settings match those of the current file.
## .map.checkEvents......Check to see if GUI settings match those of the current file.
## .map.checkCells.......Check to see if GUI settings match those of the current file
## .map.addShapes........Add shapes to the plot; redraw if a shape has been removed.
## .map.changeC..........Have the settings to display the coast changed?
## .map.changeW..........Have the settings of the data files changed?
## .map.changeV..........Have monitored shapes and settings changed?
## .map.catf.............Flush the cat down the console.
## .map.reColour.........Quick cell colour change.
## .map.addAxis..........Use PBSmapping's .addAxis function.
##==============================================================================


## createMap----------------------------2023-02-28
## Map wrapper for plotting PBS maps using a GUI.
## ---------------------------------------------RH
createMap = function(hnam=NULL, ...)
{
	if (exists(".coast",envir=.PBSmapxEnv)) rm(.coast,pos=.PBSmapxEnv)
	if (exists("PBSmap",envir=.PBSmapxEnv)) rm(PBSmap,pos=.PBSmapxEnv)
	if (exists("Mfile",envir=.PBSmapxEnv))  rm(Mfile,pos=.PBSmapxEnv)
	if (exists("Qfile",envir=.PBSmapxEnv))  rm(Qfile,pos=.PBSmapxEnv)
	options(warn=-1)
	mset = c("isobath","major","minor","locality","srfa")    ## Management areas
	sset = c("hsgrid","ltsa","qcssa","wcvisa")               ## Surveys
	oset = c("trawlfoot","spongeCPZ","spongeAMZ","rca")      ## Zones (e.g., MPAs)
	pset = c(mset,sset,oset)
	dset = c("spn","testdatC","testdatR")                    ## Datasets
	data(list=pset, envir=.PBSmapxEnv)
	data(list=dset, envir=.PBSmapxEnv)
	PBSmap = list(module="M01_Map", call=match.call(), plotname="PBSmap", pset=pset, disproj="LL",
		bvec0=rep(FALSE,length(pset)), isob0=rep(FALSE,18), dis0=rep(FALSE,6), AofO=NA)
	assign("PBSmap",PBSmap,envir=.PBSmapxEnv)
	## Monitor GUI values:
	cmon = c("cnam","projection","zone")                     ## coast line file
	gmon = c("xlim","ylim","cells","byrow","gtype")          ## grid values
	Qmon = c("fnam","xlim","ylim","zlim","dlim","strSpp","zfld","fid","gear")    ## Qfile (qualified fishing data)
	emon = c(Qmon,"bg","fg","eN","bo")                       ## events
	pmon = c(emon,"cells","fn","Vmin","Q","Flevs","track")   ## pdata
	vval = c("nMix")                                         ## non-GUI values for grid and events 
	pval = c("clrs","brks","AREA","AofO")                    ## non-GUI values for pdata
	packList(c("cmon","gmon","Qmon","emon","pmon","vval","pval"), "PBSmap", tenv=.PBSmapxEnv)
	.map.getCoast(cnam="nepacLL")

	pdir = system.file(package="PBSmapx")
	wdir = paste(pdir,"/win",sep="")
	sdir = paste(pdir,"/sql",sep="")
	rtmp = tempdir(); rtmp = gsub("\\\\","/",rtmp)
	wnam = paste(wdir,"mapWin.txt",sep="/")
	wtmp = paste(rtmp,"mapWin.txt",sep="/")
	snam = paste(sdir,"fos_map_density.sql",sep="/")
	stmp = paste(rtmp,"fos_map_density.sql",sep="/")
	temp = readLines(wnam)
	temp = gsub("@ehat",  eval(parse(text=deparse("\u{00EA}"))), temp)  ## RH 230228
	temp = gsub("@wdf",wtmp,temp)
	temp = gsub("@sql",stmp,temp)
	temp = gsub("@wdir",wdir,temp)
	if (!is.null(hnam) && is.character(hnam))
		temp = gsub("#import=",paste("import=\"",hnam,"\"",sep=""),temp)
	writeLines(temp,con=wtmp)
	file.copy(snam,stmp)
	tget(.PBSmod); .PBSmod$.options$par.map <- list(...); tput(.PBSmod)
	# R-2.14.0 appears to implement windows buffering, which can screw the interactive nature of 'createMap'
	winbuf = windows.options()$buffered
	eval(parse(text=
		paste("assign(\".map.exit\",function(){windows.options(buffered=",winbuf,")},envir=.PBSmapxEnv)",sep="")))
	windows.options(buffered=FALSE)
	resetGraph()
	createWin(wtmp)
	if (is.null(hnam) || !is.character(hnam))
		.map.map()
	mess = c("Additional labels and/or shapes will be automatically added when:",
	"   'pbs.lab' -- comma-delimited text file exists in the working directory; and/or",
	"   'pbs.pset' -- list object containing a collection of PolySets exists in the R working environment.",
	"",
	"If user ticks languange check box for French, a subdirectory called 'french'",
	"   will be created to dump duplicated figures but with French annotations.")
	cat(paste0(mess,collapse="\n"),"\n")
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~createMap


## .map.map-----------------------------2021-04-12
## Controls the flow of mapping.
## ---------------------------------------------RH
.map.map = function(addA=FALSE,addI=FALSE,addG=FALSE,addT=FALSE,addB=FALSE,addC=FALSE,addL=FALSE,lwd=0.3,...)
{
	opts = c(options()[c("OutDec", "stringsAsFactors")], list(big.mark=""))
	on.exit(options(opts))
	options(stringsAsFactors=FALSE)

	.map.checkCoast()
	getWinVal(winName="window",scope="L")
	unpackList(xtcall(PBSmap),scope="L")
	lang = .win.get.lang()

	spp  = eval(parse(text=paste("c(\"",gsub(",","\",\"",strSpp),"\")",sep="")))
	mvec = ls()[grep("^m[1-9]",ls())]
	svec = ls()[grep("^s[1-9]",ls())]
	ovec = ls()[grep("^o[1-9]",ls())]
	bvec = c( sapply(mvec,function(x){eval(parse(text=paste0(x,"=",x)))}),
				sapply(svec,function(x){eval(parse(text=paste0(x,"=",x)))}),
				sapply(ovec,function(x){eval(parse(text=paste0(x,"=",x)))}) )

	## colour ramp function bounded by white and black
	expr=paste("crap=function(colors=c(\"",bg,"\",\"",fg,"\")){",
		"rfun=colorRampPalette(c(\"white\",colors,\"black\"),space=\"Lab\"); ",
		"return(rfun) }",sep=""); eval(parse(text=expr))
	act  = getWinAct()[1]; png=tif=eps=wmf=FALSE
	if (!is.null(act) && act=="png") png = TRUE
	if (!is.null(act) && act=="tif") tif = TRUE
	if (!is.null(act) && act=="eps") eps = TRUE
	if (!is.null(act) && act=="wmf") wmf = TRUE
	redraw = TRUE;
	if (!any(c(png,tif,eps,wmf)))
		lang = lang[1] ## only need one language for viewing on screen

	dis1 = c(disG,disT,disB,disC,disL,disA); ## current displays for species
	if (length(dis1)!=length(dis0)) 
		showError("Programmer Alert!\n\nInitalize 'dis0' to match 'dis1' on line 86")
	if (projection==disproj) {
		if ( all((bvec-bvec0)>=0) && all((isob-isob0)>=0) && all((dis1-dis0)>=0) ) { ## no removals
			## Areas, Isobaths, Grid, Tows, Bubbles, Cells, Legend
			if (any(c(addA,addI,addG,addT,addB,addC,addL)==TRUE)) {
				shapes = list()
				if (addG && disG) { .map.checkGrid();   shapes$grid = TRUE } 
				if (addC && disC) { .map.checkGrid();  .map.checkQfile(); .map.checkEvents(); .map.checkCells();  shapes$cell = TRUE } 
				if (addT && disT) { .map.checkQfile(); .map.checkEvents(); shapes$tows = TRUE } 
				if (addB && disB) { .map.checkQfile(); .map.checkEvents(); shapes$bubb = TRUE } 
				if (addA) { shapes$bdry = TRUE }
				if (addI && any(isob==TRUE) ) { shapes$ziso = TRUE }
				if ((addL | addT | addB |addC) && (disL | disA)) { shapes$lege = TRUE }
				unpackList(xtcall(PBSmap),scope="L")
				redraw = FALSE; .map.addShapes(shapes, onelang=lang[1])
			}
		}
	}
	packList(c("crap","png","tif","eps","wmf"),"PBSmap",tenv=.PBSmapxEnv)
	bvec0=bvec; dis0=dis1; isob0=isob; #hsi0=hsi
	packList(c("bvec0","dis0","isob0"),"PBSmap",tenv=.PBSmapxEnv)

	if (redraw || xtcall(.change)) {
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
		if (!isThere("col.pset")) col.pset = c("transparent","slategray") # c(bg,fg) 
		pin = par()$pin  ## assumes plot is already on screen
		pin[1] = pin[1]/diff(plt[1:2]); pin[2] = pin[2]/diff(plt[3:4]) ## adjust plot dimensions to accommodate 'plt' boundaries
		setPBSoptions("par.map", list(plt=plt, mgp=mgp, las=las, cex.axis=cex.axis, cex.lab=cex.lab,
			cex.txt=cex.txt, cex.leg=cex.leg, pin=pin, tit.loc=tit.loc, leg.loc=leg.loc, leg.font=leg.font,
			col.pset=col.pset))

		fsep = "."
		if (disT|disB|disC) {  ## (RH 210412)
			onam = paste0("map",fsep)
			qnam = match(zfld,c("catch","cat","C","effort","eff","E","CPUE","cpue","U"))
			if (is.na(qnam)) {
				if (zfld!=spp)
					.flush.cat(paste0("Warning: Potential mismatch between 'spp' (", spp, ") and 'zfld' (", zfld, ")\n"))
				if (fn %in% "sumT")           ## sumT = sum of catch in tonnes
					onam = paste0(onam,"C")
				else if (fn %in% "sumE")      ## sumE = sum of effort in ??? (not defined yet)
					onam = paste0(onam,"E")
				else
					onam = paste0(onam,"X")
			}
			else onam = paste0(onam, switch(qnam,"C","C","C","E","E","E","U","U","U"))
#browser();return()
			onam = paste0(onam,".",paste0(c("tows","bubbs",paste0(switch(gtype,"r","h"),"cells"))[c(disT,disB,disC)],collapse="+"))
			onam = paste0(onam,fsep,spp,fsep,"d(",paste(substring(gsub("-","",dlim),3),collapse="-"),")")
			onam = paste0(onam,fsep,"z(",zlim[1],"-",zlim[2],")")
			if (any(fid))
				onam = paste0(onam,fsep,"fid(",paste(names(fid)[fid],collapse=""),")")
			if (any(gear))
				onam = paste0(onam,fsep,"gear(",paste(names(gear)[gear],collapse=""),")")
		} else {
			onam = "map"
			onam = paste0(onam,fsep,"x(",xlim[1],",",xlim[2],")")
			onam = paste0(onam,fsep,"y(",ylim[1],",",ylim[2],")")
		}

		fout = fout.e = onam
		## Create a subdirectory called `french' for French-language figures (if option is selected)
		if (png|tif|eps|wmf) lang=c("e","f")  ## for multiple images from now on (RH 210224)
		createFdir(lang)
		for (l in lang) {  ## could switch to other languages if available in 'linguaFranca'.
			changeLangOpts(L=l)  ## (RH 210224)
			fout = switch(l, 'e' = fout.e, 'f' = paste0("./french/",fout.e) )
			if (png) {
				PIN = 8.5 * pin/max(pin)
				png(filename=paste(fout,".png",sep=""), units="in", res=400, width=PIN[1], height=PIN[2])
			} else if (tif) {
				PIN = 8.5 * pin/max(pin)
				tiff(filename=paste(fout,".tif",sep=""), units="in", res=400, width=PIN[1], height=PIN[2])
			} else if (wmf) {
				PIN = 10 * pin/max(pin)
				do.call("win.metafile",list(filename=paste(fout,".wmf",sep=""), width=PIN[1], height=PIN[2]))
			} else if (eps) {
				PIN = 10 * pin/max(pin)
				postscript(file=paste(fout,".eps",sep=""), width=PIN[1], height=PIN[2], fonts="mono", paper="special", horizontal=FALSE)
			}
			coast = clipPolys(xtcall(.coast),xlim=xlim,ylim=ylim)
			## create a box that is essentially a hole (piece of ocean)
			if (is.null(coast) || nrow(coast)==0) {
				atts=attributes(xtcall(.coast))[setdiff(names(attributes(xtcall(.coast))),c("names","row.names","class"))] # extra attributes
				coast=as.PolySet(data.frame(
				PID=rep(1,8), SID=rep((1:2),each=4), POS=c(1:4,4:1),
				X=c(xlim[1],xlim[1],xlim[2],xlim[2],xlim[1],xlim[2],xlim[2],xlim[1]),
				Y=c(ylim[1],ylim[2],ylim[2],ylim[1],ylim[1],ylim[1],ylim[2],ylim[2]) ) )
				for (i in names(atts))
					attr(coast,i)=atts[[i]]
			}
			plotMap(coast, xlim=xlim, ylim=ylim, plt=plt, mgp=mgp, las=las, cex.axis=cex.axis, cex.lab=cex.lab, col="transparent", border="transparent", lwd=lwd)

			if (any(c(bvec,isob,disG,disT,disB,disC)==TRUE)) {
				if (any(c(disT,disB,disC)==TRUE)) {
					## display tows, bubbles, cells
					.map.checkMfile(); .map.checkQfile()
				}
				shapes = list()
				if (any(bvec==TRUE)) { shapes$bdry = TRUE }
				if (any(isob==TRUE)) { shapes$ziso = TRUE }
				if (disG) { .map.checkGrid();   shapes$grid = TRUE }
				if (disT) { .map.checkEvents(); shapes$tows = TRUE }
				if (disB) { .map.checkEvents(); shapes$bubb = TRUE }
				if (disC) { .map.checkGrid(); .map.checkEvents(); .map.checkCells();  shapes$cell = TRUE }
				if (length(shapes)>0) unpackList(xtcall(PBSmap),scope="L")
#browser();return()
				.map.addShapes(shapes, onelang=l)
			}
			addPolys(coast,col=land,lwd=lwd)
			.map.addAxis()
			if (disL|disA) {
				.map.addShapes(list(lege=TRUE), onelang=l)
			}
			disproj = projection
			packList("disproj","PBSmap",tenv=.PBSmapxEnv)

			## If comma-delimited file exists with fields EID, X, Y, and label, use 'addLabels' with placement = "DATA".
			## For french labels, supply 'pbs.lab.f'
			plab = paste0("pbs.lab", switch(l, 'e'="", 'f'=".f"))
			if (file.exists(plab)) {
				pbs.lab = read.csv(plab, allowEscapes=TRUE)
				pbs.lab = pbs.lab[grep("^#", pbs.lab[,"EID"], invert=TRUE),]
				pbs.lab[,"EID"] = as.numeric(pbs.lab[,"EID"])
				pbs.lab = as.EventData(pbs.lab,projection="LL")
				if (disproj!="LL") pbs.lab=convUL(pbs.lab)
				if ("adj" %in% names(pbs.lab)){
					for (a in .su(pbs.lab$adj))
						addLabels(pbs.lab[is.element(pbs.lab$adj,a),], placement="DATA", adj=a, cex=1.2)
				} else
					addLabels(pbs.lab, placement="DATA", adj=1, cex=1.2)
			}
			## Add in extra Polysets contain in list called `pbs.pset'
			if (exists("pbs.pset",where=1)) {
				for (i in 1:length(pbs.pset)) {
					ipset = pbs.pset[[i]]
					if (class(ipset)[1] != "PolySet") next
					if ("PolyData" %in% names(attributes(ipset))) {
						pdata = attributes(ipset)$PolyData
						addPolys(ipset, polyProps=pdata, lwd=lwd)
					} else
						addPolys(ipset, lwd=lwd, col=col.pset[1], border=col.pset[2])
				}
			}
			box()
			if (png|tif|eps|wmf) dev.off()
		} ; eop()  ## (RH 210224)
	}    ## end if redraw
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.map


## .map.getCoast------------------------2013-01-23
## Get the coast file (e.g., 'nepacLL')
## ---------------------------------------------RH
.map.getCoast = function(cnam=NULL)
{ 
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
				expr=paste("xtget(",i,"); attr(",i,",\"zone\")<-",zone,"; cat(\"",i," - \"); ",i,"<-convUL(",i,"); xtput(",i,")",sep="")
				eval(parse(text=expr)) } 
			cat("coast - "); assign(".coast",convUL(xtcall(.coast)),envir=.PBSmapxEnv)
		}
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.getCoast


## .map.mfile---------------------------2020-03-09
## Get the Master data file from one of many 
## potential sources, and standardise fields names.
## ---------------------------------------------RH
.map.mfile = function() 
{ # Mfile = Master file read in from GUI
	getWinVal(winName="window",scope="L")
	unpackList(xtcall(PBSmap),scope="L")
	#if (!("Mfile" %in% xlisp())) {
		expr = paste0("getFile(",fnam,",use.pkg=TRUE,try.all.frames=TRUE,tenv=penv()); Mfile=",fnam)
		eval(parse(text=expr))
	#} else xtget(Mfile)
#browser();return()
	flds = names(Mfile)
	STOP = function(msg="Stop due to error and set Mfile to NULL") {
		Mfile <- NULL; xtput(Mfile)
		showAlert(msg, title="Error", icon="error"); stop(msg,call.=FALSE) }
	if (!any(flds=="EID"))  Mfile$EID = 1:nrow(Mfile)
	if (!any(flds=="X"))    Mfile=.map.checkFlds(c("longitude","long","lon","x"),"X",Mfile)
	Mfile = Mfile[!is.na(Mfile$X),]
	if (!any(flds=="Y"))    Mfile=.map.checkFlds(c("latitude","lat","y"),"Y",Mfile)
	Mfile = Mfile[!is.na(Mfile$Y),]
	if (!any(flds=="X2"))   Mfile=.map.checkFlds(c("X","longitude","long","lon","x"),"X2",Mfile)
	if (!any(flds=="Y2"))   Mfile=.map.checkFlds(c("Y","latitude","lat","y"),"Y2",Mfile)
	if (!any(flds=="fdep")) Mfile=.map.checkFlds(c("depth","depth1","fishdepth","Z","z"),"fdep",Mfile)
	if (!any(flds=="cfv"))  Mfile=.map.checkFlds(c("CFV","vessel","boat","ship","VID"),"cfv",Mfile) ## (RH 200309 -- added 'VID')
	if (!byC) 
		Mfile=.map.checkFlds(c("spp","species","sp","hart","code"),"spp",Mfile)

	attr(Mfile,"last") = fnam;
	xtput(Mfile)
	invisible() }
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.mfile


## .map.qfile---------------------------2017-07-28
## Qualify the Master file using various limits
## ---------------------------------------------RH
.map.qfile = function() 
{ # Qfile = Qualified file from Master
	getWinVal(winName="window",scope="L")
	.map.checkMfile()
	Qfile = xtcall(Mfile)
	unpackList(xtcall(PBSmap),scope="L")
	flds = names(Qfile)
	STOP = function(msg="Stop due to error and set Qfile to NULL") {
		Qfile <- NULL; xtput(Qfile)
		showAlert(msg, title="Error", icon="error"); stop(msg,call.=FALSE) }
	cstproj = attributes(Qfile)$projection
	if (is.null(cstproj)) {
		if (any(Qfile$X>360)) cstproj="UTM" else cstproj="LL"
		attr(Qfile,"projection") = cstproj }
	attr(Qfile,"zone") = zone
	if (cstproj!=projection) {
		Qfile=convUL(Qfile)
		z = !is.na(Qfile$X2) & !is.na(Qfile$Y2)
		tmp = as.EventData(cbind(EID=Qfile$EID[z],X=Qfile$X2[z],Y=Qfile$Y2[z]),projection=cstproj,zone=zone)
		tmpUL = convUL(tmp)
		Qfile$X2[z] = tmpUL$X; Qfile$Y2[z] = tmpUL$Y
	}
	Qfile = Qfile[(Qfile$X>=xlim[1] | Qfile$X2>=xlim[1]) & (Qfile$X<=xlim[2] | Qfile$X2<=xlim[2]) & (!is.na(Qfile$X) | !is.na(Qfile$X2)),]
	if (nrow(Qfile)==0) STOP("No records in this longitude range")
	Qfile = Qfile[(Qfile$Y>=ylim[1] | Qfile$Y2>=ylim[1]) & (Qfile$Y<=ylim[2] | Qfile$Y2<=ylim[2]) & (!is.na(Qfile$Y) | !is.na(Qfile$Y2)),]
	if (nrow(Qfile)==0) STOP("No records in this latitude range")
	Qfile = Qfile[Qfile$fdep>zlim[1] & Qfile$fdep<=zlim[2] & !is.na(Qfile$fdep),]
	if (nrow(Qfile)==0) STOP("No records in this depth range")
	Qfile = Qfile[!is.na(Qfile$date),]
	#Qfile$date = as.POSIXct(substring(Qfile$date,1,10))    ## this is way too slow
	#Qfile = Qfile[Qfile$date>=as.POSIXct(dlim[1]) & Qfile$date<=as.POSIXct(dlim[2]),]
	#Qfile$date = substring(Qfile$date,1,10)                ## this also take a while
	Qfile = Qfile[Qfile$date>=dlim[1] & Qfile$date<=dlim[2],]
	if (nrow(Qfile)==0) STOP("No records in this date range")
	if (is.element("fid",flds) && any(fid)) {
		Qfile = Qfile[is.element(Qfile$fid,names(fid)[fid]),]
		if (nrow(Qfile)==0) STOP("No records for the selected fisheries")
	}
	if (is.element("gear",flds) && any(gear)) {
		Qfile = Qfile[is.element(Qfile$gear,names(gear)[gear]),]
		if (nrow(Qfile)==0) STOP("No records for the selected gears")
	}

	spp = eval(parse(text=paste("c(\"",gsub(",","\",\"",strSpp),"\")",sep="")))
	xtget(spn)

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
		SPP = sort(unique(xtcall(Mfile)$spp))
		if (all(spp=="*") | all(spp=="")) spp = SPP
		Qfile = Qfile[is.element(Qfile$spp,spp),]
		if (nrow(Qfile)==0) sppErr(SPP)
		if (!any(flds=="catch")) 
			Qfile=.map.checkFlds(c("cat","total","tcat","landed","land","kept","discarded","discards","discard","C"),"catch",Qfile)
		spp = sort(unique(Qfile$spp)); }
	setWinVal(winName="window",list(strSpp=paste(spp,collapse=","))) 

	if (!any(flds=="effort"))
		Qfile=.map.checkFlds(c("eff","duration","time","hours","minutes","E"),"effort",Qfile)
#browser();return()

	if (is.element(zfld,c("CPUE","cpue","U"))) {
		Qfile = Qfile[Qfile$effort>0 & !is.na(Qfile$effort),]
		if (nrow(Qfile)==0) STOP("No records with valid effort data")
		if (!any(flds=="cpue")) {
			Qfile=.map.checkFlds(c("CPUE","cpue","U"),"cpue",Qfile)
			if (!any(names(Qfile)=="cpue")) Qfile$cpue=Qfile$catch/Qfile$effort 
		}
	}
	flds = names(Qfile)

	if (any(flds==zfld)) 
		Qfile$Z = Qfile[,zfld] 
	else
		STOP(wrapText(paste("Choose a Z-field from:\n",paste(flds,collapse=", "),sep=""),width=30,prefix="",exdent=5))

	#for (i in Qmon) attr(Qfile,i) = get(i)
	for (i in Qmon) eval(parse(text=paste0("attr(Qfile,\"",i,"\") = ",i)))
	packList(c("spp","SPP"),"PBSmap",tenv=.PBSmapxEnv)
	xtput(Qfile)
	invisible() }
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.qfile


## .map.mgrid---------------------------2013-01-23
## Make a grid for fcell
## ---------------------------------------------RH
.map.mgrid = function() 
{
	getWinVal(winName="window",scope="L")
	gx = seq(xlim[1],xlim[2],cells[1]); gy = seq(ylim[1],ylim[2],cells[2])
	agrid = makeGrid(x=gx,y=gy,projection=projection,zone=zone,byrow=byrow,type=switch(gtype,"rectangle","hexagon"))
	xtget(PBSmap)
	gcells = cells
	nMix = ifelse(eN==3,0,1) # if grid changes and tow position is set to blend, routine must remake events
#browser();return()
	for (i in c(PBSmap$gmon,"nMix")) attr(agrid,i) = get(i)
	PBSmap$agrid <- agrid; xtput(PBSmap)
	packList(c("gcells","nMix"),"PBSmap",tenv=.PBSmapxEnv)
	invisible() }
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.mgrid


## .map.gevent--------------------------2016-03-29
## Get events from Qfile
## ---------------------------------------------RH
.map.gevent = function() 
{
	.map.checkMfile(); .map.checkQfile()
	getWinVal(winName="window",scope="L")
	unpackList(xtcall(PBSmap),scope="L")
	xtget(Qfile)

	EID = eid = Qfile$EID;
	X   = Qfile$X;   Y  = Qfile$Y
	X2  = Qfile$X2;  Y2 = Qfile$Y2
	if (!is.element("cfv",colnames(Qfile)))
		cfv = Qfile[,track]
	else
		cfv = Qfile$cfv
	eos = rep(1,length(EID))
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
			zmat$nMix = apply(zmat,1,function(x){ ceiling(sqrt(((x["x2"]-x["x1"])/cells[1])^2 + ((x["y2"]-x["y1"])/cells[2])^2))+1 })
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
				xtget(PBSmap)
				PBSmap$tally <- PBSmap$tally + nMix
				PBSmap$blend[(PBSmap$tally-nMix+1):PBSmap$tally,] <- cbind(eid,Xnew,Ynew,Z,cfv,eos)
				xtput(PBSmap)
				invisible()
				})
			xtget(PBSmap); PBSmap$blend <- as.data.frame(PBSmap$blend); xtput(PBSmap)
			unpackList(xtcall(PBSmap)$blend,scope="L")
			EID=1:length(c(eid,eid0)); eid=c(eid,eid0); Xnew=c(Xnew,x0); Ynew=c(Ynew,y0)
			Z=c(Z,z0); cfv=c(cfv,cfv0); eos=c(eos,rep(1,length(eid0)))
		}
	}
#browser();return()
	events = data.frame(EID=EID,X=Xnew,Y=Ynew,Z=Z,cfv=cfv,eid=eid,eos=eos)
	zev    = !is.na(events$X) & !is.na(events$Y) & !is.na(events$Z) & !is.infinite(events$Z)
	events = events[zev,,drop=FALSE]
	zev    = is.na(events$cfv); if (length(zev)>0) events$cfv[zev] = 9999
	events = as.EventData(events,projection=projection,zone=zone)
	nMix   = max(events$eos)

	packList(c("nMix"),"PBSmap",tenv=.PBSmapxEnv)  # need these to monitor
	for (i in emon) attr(events,i) = get(i)
	for (i in vval) attr(events,i) = get(i)
	xtget(PBSmap); PBSmap$events <- events; xtput(PBSmap)
	invisible() }
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.gevent


## .map.fcell---------------------------2019-04-11
## Find events in cells
## ---------------------------------------------RH
.map.fcell = function(OK=TRUE) 
{
	on.exit(gc(verbose=FALSE))
	.map.checkMfile(); .map.checkQfile(); .map.checkGrid(); .map.checkEvents()
	getWinVal(winName="window",scope="L")
	unpackList(xtcall(PBSmap),scope="L")
	xtget(Qfile)

	# fishing vessels in all events (including expanded)
	vess    = events[,c("EID","X","Y","cfv")]; names(vess)[4] = "Z"
	# unique fishing events in tows
	fevs    = events[is.element(events$eos,1),c("EID","X","Y","cfv")]; names(fevs)[4] = "Z"
	# findCells cannot handle extra attributes on agrid
	bgrid   = agrid; attributes(bgrid)=attributes(bgrid)[1:5]
	if (attributes(agrid)$gtype == 1) {
		LocData = findCells(events,bgrid)
	} else {
		LocData = findPolys(events,bgrid,maxRows=1e+07)
	}
	locClass = attributes(LocData)$class
	# Get rid of events duplicated on boundaries
	locData = LocData[order(LocData$EID),]
	zD = duplicated(locData$EID)     # find duplicated events
	locData = locData[!zD,]          # get the unique events (not duplicated)
	attr(locData,"class") = locClass

	## Sometimes no. grid colums overwhelms no. of events, but still need no. digits from grid
	## combineEventsQuickly adds rownames after 
	dig = .createFastIDdig(agrid,cols=c("PID","SID"))

	##### pdata is subset later but tdata is not = mismatch so that T != V + H (thanks Brian)
	pdata = Pdata = combineEventsQuickly(events, locData, FUN=get(fn), dig=dig)               ## Summarize Z
	tdata = combineEventsQuickly(fevs, locData, FUN=length, dig=dig)                          ## Total number of original tows
	xdata = combineEventsQuickly(events, locData, FUN=length, dig=dig)                        ## Total number of expanded tows
	vdata = combineEventsQuickly(vess, locData, FUN=function(x){length(unique(x))}, dig=dig)  ## Number of vessels (cfv)

	#dig = .createFastIDdig(agrid,cols=c("PID","SID")) # sometimes # grid colums overwhelms # of events
	## combineEventsQuickly adds rownames
	#rownames(pdata) = .createIDs(pdata,c("PID","SID"),fastIDdig=dig)
	pdata$vess = pdata$xtow = pdata$tows = rep(0,nrow(pdata))
	tows = tdata$Z; names(tows)=dimnames(tdata)[[1]] #.createIDs(tdata,c("PID","SID"),fastIDdig=dig)
	pdata[names(tows),"tows"] = tows
	xtow = xdata$Z; names(xtow)=dimnames(xdata)[[1]] #.createIDs(xdata,c("PID","SID"),fastIDdig=dig)
	pdata[names(xtow),"xtow"] = xtow
	vess = vdata$Z; names(vess)=dimnames(vdata)[[1]] #.createIDs(vdata,c("PID","SID"),fastIDdig=dig)
	pdata[names(vess),"vess"] = vess
#browser();return()

	Zkeep=rep(TRUE,nrow(pdata))
	if (ex0)   Zkeep=Zkeep & (round(pdata$Z,5)!=0 & !is.na(pdata$Z)) # exclude zeroes
	if (exneg) Zkeep=Zkeep & (pdata$Z>=0 & !is.na(pdata$Z))          # exclude negative values
	pdata=pdata[Zkeep,]; tdata=tdata[Zkeep,]                         # need to reduce dimensions of tdata also
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

	#--- Calculate area (km^2) for each cell
#browser();return()
	pdata$area = rep(0,nrow(pdata))
	#idp  = .createIDs(pdata,c("PID","SID"),fastIDdig=dig) ## these are numeric and don't always match ida indices #dimnames(pdata)[[1]]
	idp  = .fixNumIDs(.createIDs(pdata,c("PID","SID"),fastIDdig=dig),dig)
	ida  = .fixNumIDs(.createIDs(agrid,c("PID","SID"),fastIDdig=dig),dig)
	temp = agrid[is.element(ida,idp),]
	atmp = calcArea(temp)
	area = atmp$area
	names(area) = .fixNumIDs(.createIDs(atmp,c("PID","SID"),fastIDdig=dig),dig)
	pdata[names(area),"area"] = area

	## restrict area calculation to visible cells (based on Vmin)  (RH 190411)
	adata = pdata[pdata$vsee,]
	AREA = rep(0,nclr); names(AREA)=1:nclr
	areasum = sapply(split(adata$area,adata$lev),sum)  # split: missing values in f are dropped together with the corresponding values of x
	#areasum = sapply(split(pdata$area,pdata$lev),sum)  # split: missing values in f are dropped together with the corresponding values of x
	AREA[names(areasum)] = areasum
	AofO = sum(AREA,na.rm=TRUE)
	#--- End area calculation

	packList(c("clrs","brks","AREA","AofO"),"PBSmap",tenv=.PBSmapxEnv)  # need these to monitor
	for (i in pmon) attr(pdata,i) = get(i)
	for (i in pval) attr(pdata,i) = get(i)

	pid  = .fixNumIDs(.createIDs(pdata[vsee,],c("PID","SID"),fastIDdig=dig),dig)
	zlev = !is.na(pdata$lev) # within the breaks and permitted to see
	tows = c(sum(pdata$tows[zlev]),sum(tdata$Z[zlev&vsee]),sum(tdata$Z[zlev&!vsee]))
	attr(tdata,"tows") = tows

	index = .fixNumIDs(.createIDs(locData,c("PID","SID"),fastIDdig=dig),dig)
	names(index) = locData$EID
	Qfile$index  = rep(NA,nrow(Qfile))
	Qfile$index  = index[as.character(Qfile$EID)]
	if (is.element(track,names(Qfile))) {
		kid = is.element(Qfile$index,pid)
		tracked = Qfile[,track][kid]
		names(tracked) = Qfile$index[kid] 
		attr(tracked,"unique") = sort(unique(tracked)) }
	else tracked = "No matching fields in Qfile"
	
	xtget(PBSmap); PBSmap$LocData <- LocData; PBSmap$locData <- locData; xtput(PBSmap)
	stuff=c("Pdata","pdata","tdata","xdata","vdata","tracked","index","adata")
	packList(stuff,"PBSmap",tenv=.PBSmapxEnv) 
	setWinVal(winName="window",list(strSpp=paste(spp,collapse=","),Vmax=Vmax))
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.fcell


## .map.checkFlds-----------------------2010-10-19
## Check fields for avaialbility & create standardised fields.
## ---------------------------------------------RH
.map.checkFlds = function(badnames,goodname,dat) 
{
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
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.checkFlds


## .map.checkLims-----------------------2012-02-17
## Check that second limit is >= the first limit.
## ---------------------------------------------RH
.map.checkLims <- function(limObj) 
{
	limName = as.character(substitute(limObj))
	limObj1 = limObj[1]
	limObj2 = rev(limObj)[1]
	if (limObj1 >= limObj2) showError(paste("Second <",limName,"> must be greater than the first.",sep=""))
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.checkLims


## .map.checkCoast----------------------2013-01-23
## Check to see if the coast file needs to change.
## ---------------------------------------------RH
.map.checkCoast = function(change=FALSE)
{
	getWinVal(winName="window",scope="L")
	for (i in c("xlim","ylim","zlim","dlim"))
		eval(parse(text=paste(".map.checkLims(",i,")",sep="")))
	if (!exists(".coast",envir=.PBSmapxEnv)) {
		.map.catf("\nNew Coast\n")
		.map.getCoast("nepacLL") }
	else {
		change = .map.changeC(xtcall(PBSmap)$cmon)
		if (change) {
			.map.catf("Coast changed\n"); 
			.map.getCoast()
		}
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.checkCoast


## .map.checkMfile----------------------2016-03-29
## Check to see if a new master file is needed.
## ---------------------------------------------RH
.map.checkMfile = function() 
{
	getWinVal(winName="window",scope="L")
	Mfile = xtcall(Mfile)
	if (is.null(Mfile) || attributes(Mfile)$last != fnam) {
		.map.catf("\nNew Mfile\n")
		Qfile <- NULL; xtput(Qfile)
		.map.mfile() }
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.checkMfile


## .map.checkQfile----------------------2016-03-29
## Check to see if GUI settings match those of the current file.
## ---------------------------------------------RH
.map.checkQfile = function(change=FALSE)
{
	getWinVal(winName="window",scope="L")
	Qfile = xtcall(Qfile)
	if (is.null(Qfile)) { .map.catf("\nNew Qfile\n"); .map.qfile() }
	else {
		change = .map.changeW(xtcall(PBSmap)$Qmon,Qfile)
		if (change) {
			.map.catf("Qfile changed\n")
			.map.qfile()
		}
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.checkQfile


## .map.checkGrid-----------------------2013-01-23
## Check to see if GUI settings match those of the current file.
## ---------------------------------------------RH
.map.checkGrid = function(change=FALSE)
{
	getWinVal(winName="window",scope="L")
	agrid = xtcall(PBSmap)$agrid
	if (is.null(agrid)) { .map.catf("\nNew grid\n"); .map.mgrid() }
	else {
		change = .map.changeW(xtcall(PBSmap)$gmon,agrid) | .map.changeV(xtcall(PBSmap)$vval,agrid)
		if (change) {
			.map.catf("grid changed\n")
			.map.mgrid()
		}
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.checkGrid


## .map.checkEvents---------------------2013-01-23
## Check to see if GUI settings match those of the current file.
## ---------------------------------------------RH
.map.checkEvents = function(change=FALSE)
{
	getWinVal(winName="window",scope="L")
	events = xtcall(PBSmap)$events
	if (is.null(events)) { .map.catf("\nNew events\n"); .map.gevent() }
	else {
		change = .map.changeW(xtcall(PBSmap)$emon,events) | .map.changeV(xtcall(PBSmap)$vval,events)
		if (change) {
			.map.catf("events changed\n")
			.map.gevent()
		}
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.checkEvents


## .map.checkCells----------------------2013-01-23
## Check to see if GUI settings match those of the current file
## ---------------------------------------------RH
.map.checkCells = function(change=FALSE)
{
	getWinVal(winName="window",scope="L")
	pdata = xtcall(PBSmap)$pdata
	if (is.null(pdata)) { .map.catf("\nNew pdata\n"); .map.fcell() }
	else {
		change = .map.changeW(xtcall(PBSmap)$pmon,pdata) | .map.changeV(xtcall(PBSmap)$pval,pdata)
		if (change) {
			.map.catf("pdata changed\n")
			.map.fcell()
		}
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.checkCells


## .map.addShapes-----------------------2020-03-09
## Add shapes to the plot; redraw if a shape has been removed.
## ---------------------------------------------RH
.map.addShapes = function(shapes=list(), lwd=0.3, onelang) 
{ # 0=no click, 1=click added, -1=click removed
	#---See-Functions-begin----------
	seeGrid = function() {
		getWinVal(winName="window",scope="L")
		unpackList(xtcall(PBSmap),scope="L")
		addPolys(agrid,border="gray",density=0,lwd=lwd);  box()
	}
	## add coloured cells
	seeCell = function() {
		getWinVal(winName="window",scope="L")
		unpackList(xtcall(PBSmap),scope="L")
#browser();return()
		addPolys(agrid, polyProps=pdata[pdata$vsee,], border=ifelse(addBord,"#DFDFDF",NA), lwd=lwd)
		box()
	}
	## add event data as points or bubbles
	seeTows = function() {
		getWinVal(winName="window",scope="L")
		unpackList(xtcall(PBSmap),scope="L")
		edata = events[events$Z>0 & !is.na(events$Z),]
		addPoints(edata,col=bg,pch=16,cex=tsize)
		addPoints(edata,col=fg,pch=1,cex=tsize)
		box()
	}
	## add event data as points or bubbles
	seeBubb = function() {
		getWinVal(winName="window",scope="L")
		unpackList(xtcall(PBSmap),scope="L")
		xwid = par()$pin[1]; size = psize*xwid;
		edata = events[events$Z>0 & !is.na(events$Z),]
		edata = edata[rev(order(edata$Z)),]
		symbols(edata$X,edata$Y,circles=edata$Z^powr,inches=size,fg=fg,bg=bg,add=TRUE,lwd=lwd)
		box()
	}
	seeZiso = function () { 
		getWinVal(winName="window",scope="L")
		irap = colorRamp(c("white",icol,"black"),space="Lab") # isobath ramp function bounded by white and black
		iclrs = apply(irap(seq(.2,.9,len=18)),1,function(x){rgb(x[1],x[2],x[3],maxColorValue=255)})
		zbar = seq(100,1800,100)[isob]; iclr = iclrs[isob]
		xtget(isobath)
		ifile = isobath[is.element(isobath$PID,zbar),]
		addLines(ifile,col=iclr,lwd=lwd)
		box()
	}
	seeBdry = function(fill=FALSE){
		getWinVal(winName="window",scope="L")
		mnam = c("major","minor","locality") #,"srfa") #,"popa")
		mvec =paste0("m",1:length(mnam))
		mclr = rep(c("red","forestgreen","#FF8000","purple","red"),length(mnam))[1:length(mnam)] # temp for POP spatial
		snam = c("hsgrid","qcssa","wcvisa") #,"ltsa"
		svec =paste0("s",1:length(snam))
		sclr = rep(c("magenta","darkorange4","black","magenta"),length(snam))[1:length(snam)]
		onam = c("trawlfoot","spongeCPZ","spongeAMZ","rca")
		ovec =paste0("o",1:length(onam))
		oclr = rep(c("green3","red","blue","orange"),length(onam))[1:length(onam)]
		#oclr = rep(c("darkblue","red","pink","orange"),length(onam))[1:length(onam)]
		bvec = c( sapply(mvec,function(x){eval(parse(text=paste0(x,"=",x)))}),
					sapply(svec,function(x){eval(parse(text=paste0(x,"=",x)))}),
					sapply(ovec,function(x){eval(parse(text=paste0(x,"=",x)))}) )
		bdry = c(mnam,snam,onam)[bvec]; nb = length(bdry)
		clrs = c(mclr,sclr,oclr)[bvec] # polygon border colours only
		if (nb>0) {
			if (!fill) {
				for (i in 1:nb)
					addPolys(get(bdry[i],envir=.PBSmapxEnv),border=clrs[i],density=0,lwd=1) 
			}
			if (fill && is.element("trawlfoot",bdry)){
				addPolys(get("trawlfoot",envir=.PBSmapxEnv),border="honeydew",col="honeydew",lwd=lwd)
				#addPolys(get("trawlfoot",envir=.PBSmapxEnv),border="skyblue",col="skyblue",lwd=lwd)
			}
			if (fill && is.element("spongeAMZ",bdry)){
				addPolys(get("spongeAMZ",envir=.PBSmapxEnv),border="pink",col="pink",lwd=lwd)
				#addPolys(get("spongeAMZ",envir=.PBSmapxEnv),border="turquoise1",col="turquoise1",lwd=lwd)
			}
			if (fill && is.element("spongeCPZ",bdry)){
				addPolys(get("spongeCPZ",envir=.PBSmapxEnv),border="red",col="red",lwd=lwd)
				#addPolys(get("spongeCPZ",envir=.PBSmapxEnv),border="pink",col="pink",lwd=lwd)
			}
			if (fill && is.element("rca",bdry))
				addPolys(get("rca",envir=.PBSmapxEnv),border="gold",col="gold",lwd=lwd)
		}
		box(lwd=1)
	}
	seeLege = function (onelang) {
		getWinVal(winName="window", scope="L")
		unpackList(xtcall(PBSmap), scope="L")
		xtget(spn)
		## Top right info legend
		cap = NULL
		if (disL|disA) {
			cap = paste(spn[as.character(xtcall(PBSmap)$spp)],collapse="\n",sep="")
			if (is.null(cap) || is.na(cap) || cap=="NA" || cap=="")
				cap = toupper(as.character(xtcall(PBSmap)$spp)) }
		if (disL) cap = paste(c(cap,paste(dlim,collapse=" to ")),collapse="\n")
		if (disC && disA)
			cap = paste(c(cap,paste("Encountered area =",format(round(xtcall(PBSmap)$AofO),
				big.mark=options()$big.mark, scientific=FALSE),"km\262")),collapse="\n")
		legend(x=tit.loc, legend=linguaFranca(cap,onelang), cex=cex.txt, adj=c(0,0.15), bg="aliceblue", text.col="black")

		## Grid cell legend
		if (disL && disC) {
			breaker = function(brks,sig=3,nx=length(brks)) {
				xform = sapply(brks,function(x){format(x,scientific=FALSE,digits=sig)})
				lab = paste(ifelse(ex0 & round(brks[1],5)==0,">",">="),xform[1]," & <=",xform[2],sep="")
				if (nx>3) {
					for (i in 2:(nx-2))
						lab = c(lab,paste(">",xform[i]," & <=",xform[i+1],sep="")) }
				lab = c(lab,paste(">",xform[nx-1],sep=""))
				return (lab) }
			brks   = attributes(pdata)$brks
			nleg   = floor(par()$pin[2]/(.8*par()$cin[2])); nQ = length(brks)
			leg0   = 1 - ((nleg-nQ-4)/nleg)
#browser();return()
			llab   = linguaFranca(breaker(brks),onelang)

			nspace = max(nchar(llab))-nchar(llab)
			space  = sapply(nspace,function(x){paste(rep.int(" ",x),collapse="",sep="")})
			AREA   = attributes(pdata)$AREA;
			AREAformat = format(round(AREA),big.mark=options()$big.mark, scientific=FALSE)
			AREAblanks = sapply(AREAformat,function(x){paste(rep(" ",nchar(x)),collapse="",sep="")})
			if (disA) llab = paste(llab,space,AREAformat)
			else      llab = paste(llab,space,AREAblanks)
			packList(c("leg0","llab"),"PBSmap",tenv=.PBSmapxEnv)

			## Note: cex must be even (or available) for 'mono' to work
			par(family="mono", font=leg.font)
			L1 = leg.loc[1]; L2 = leg.loc[2]
#assign("mots",paste0(fn,"(",zfld,")",paste(rep.int(" ",max(nspace)),collapse=""),ifelse(disA,"Area(km\262)","         ")),envir=.GlobalEnv)
			addLegend(L1, L2, fill=attributes(pdata)$clrs, legend=llab, bty="n", cex=cex.leg, yjust=0, title=linguaFranca(paste0(fn,"(",zfld,")",paste(rep.int(" ",max(nspace)),collapse=""),ifelse(disA,"Area(km\262)","         ")),onelang) )
			par(family="", font=1)

			if (!any("fid" %in% colnames(xtcall(Qfile)))) {
				fisheries = ""
				fid = rep(FALSE,5); names(fid)=1:5
				setWinVal(list(fid=fid))
			} else if (all(fid)) {
				fisheries = " (trawl+others)"
			} else if (!any(fid)) {
				fisheries = ""
			} else {
				#FID = c("trawl","halibut","sable","dog/lin","HLrock")
				FID = c("T","H","S","D+L","H&L")
				fisheries = paste(" (",paste(FID[fid],collapse="+"),")",sep="")
			}

			if (!any("gear" %in% colnames(xtcall(Qfile)))) {
				gtypes = ""
				gear = rep(FALSE,4); names(gear)=1:4
				setWinVal(list(gear=gear))
			} else if (all(gear)) {
				gtypes = paste0(fisheries, " [all gear types]")
			} else if (!any(gear)) {
				gtypes = ""
			} else {
				GID = c("BT","MW","H&L","Trap")
				gtypes = paste0(" [", paste0(GID[gear],collapse="+"),"]") 
			}
			fisheries = paste0(fisheries, gtypes)
			if (Vmin==1) {
				addLabel(L1+0.025, L2-(0.02*cex.leg), linguaFranca(paste0("Events: ", format(attributes(tdata)$tows[1],big.mark=options()$big.mark), fisheries),onelang), cex=cex.leg, col="grey30", adj=c(0,0)) 
			}
			else {
				mess = linguaFranca(paste0("+ vessel", switch(onelang, 'e'="s", 'f'="s"), "/cell"),onelang) ## (RH 200807 -- originally using bateaux, now using navires)
				mess = paste("\225 ", Vmin, mess, sep="")
				mess = c(mess, paste(linguaFranca("Events:",onelang),paste(paste(c("T","V",switch(onelang, 'e'="H", 'f'="C")),  # Total, Visible, Hidden\Cach\'{e}
					format(attributes(tdata)$tows,big.mark=options()$big.mark,trim=TRUE), sep="="), collapse="; ")))
				addLabel(L1+0.025, L2-(0.03*cex.leg), paste0(mess,collapse="\n"), cex=cex.leg, col="grey30", adj=c(0,0))
			}
		} ## end if (grid cell legend)
		box()
	}
	#---See-Functions-end------------

	unpackList(getPBSoptions("par.map"),scope="L")

	if (length(shapes)==0) return()
	sord = c("grid","cell","bubb","tows","ziso","bdry","lege")
	suse = sapply(shapes,function(x){x},simplify=TRUE)
	snam = names(suse[suse])
	snam = sord[sort(match(snam,sord))]
	bdry.fill =  ("bdry" %in% snam) && any(sapply(getWinVal()[grep("^o[1-9]",names(getWinVal()))],any))
	if (bdry.fill) seeBdry(fill=TRUE)
	for (i in snam) {
		if (i=="grid")      seeGrid()
		else if (i=="cell") seeCell()
		else if (i=="tows") seeTows()
		else if (i=="bubb") seeBubb()
		else if (i=="bdry") seeBdry()
		else if (i=="ziso") seeZiso()
		else if (i=="lege") seeLege(onelang=onelang)
		else box()
	}
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.addShapes


## .map.changeC-------------------------2013-01-23
## Have the settings to display the coast changed?
## ---------------------------------------------RH
.map.changeC = function(Cnam=NULL,change=FALSE) 
{
	Clist = getWinVal(winName="window",scope="L")[Cnam]
	if (!is.null(Clist)) {
		for (i in Cnam) {
			if (any(is.na(Clist[[i]])) || any(is.na(attributes(xtcall(.coast))[[i]])) || 
				!all((Clist[[i]]==attributes(xtcall(.coast))[[i]])==TRUE) ) change=TRUE  } }
	return(change)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.changeC


## .map.changeW-------------------------2010-10-19
## Have the settings of the data files changed?
## ---------------------------------------------RH
.map.changeW = function(Wnam=NULL,file=NULL,change=FALSE)
{
	Wlist = getWinVal(winName="window",scope="L")[Wnam]
	if (!is.null(file)) {
		if (!is.null(Wlist)) {
			for (i in Wnam) {
				if (any(is.na(Wlist[[i]])) || any(is.na(attributes(file)[[i]])) || 
					!all((Wlist[[i]]==attributes(file)[[i]])==TRUE) ) change=TRUE } } }
	return(change)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.changeW


## .map.changeV-------------------------2013-01-23
## Have monitored shapes and settings changed?
## ---------------------------------------------RH
.map.changeV = function(Vnam=NULL,file=NULL,change=FALSE)
{
	Vlist = xtcall(PBSmap)[Vnam]
	if (!is.null(file)) {
		if (!is.null(Vlist)) {
			for (i in Vnam) {
				if (any(is.na(Vlist[[i]])) || any(is.na(attributes(file)[[i]])) || 
					!all((Vlist[[i]]==attributes(file)[[i]])==TRUE) ) change=TRUE } } }
	return(change)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.changeV


.map.addArea  = function() { .map.map(addA=TRUE); }
.map.addIsob  = function() { .map.map(addI=TRUE); }
.map.addGrid  = function() { .map.map(addG=TRUE); }
.map.addTows  = function() { .map.map(addT=TRUE); }
.map.addBubb  = function() { .map.map(addB=TRUE); }
.map.addCell  = function() { .map.map(addC=TRUE); }
.map.addLege  = function() { .map.map(addL=TRUE); }

## Flush the cat down the console
.map.catf = function(...) { cat(...); flush.console() }


## .map.reColour------------------------2019-01-04
## Quick cell colour change
## ---------------------------------------------RH
.map.reColour = function(pdata=xtcall(PBSmap)$pdata)
{
	getWinVal(winName="window",scope="L")
	onelang = .win.get.lang()[1]  ## only called when user is fiddling around with interactive GUI
	if (!is.null(pdata)) {
		oldclrs = sapply(split(pdata$col,pdata$lev),unique); nclr = length(oldclrs)
		frap = colorRamp(c("white",bg,fg,"black"),space="Lab") # colour ramp function bounded by white and black
		clrs  = apply(frap(seq(.15,.9,len=nclr)),1,function(x){rgb(x[1],x[2],x[3],maxColorValue=255)})
		if (length(clrs)!=length(oldclrs)) return()
		attr(pdata,"clrs") = clrs
		names(clrs) = names(oldclrs)
		pdata$col=clrs[as.character(pdata$lev)]
		xtget(PBSmap); PBSmap$pdata <- pdata; xtput(PBSmap)
	}
	.map.addShapes(list(tows=disT,bubb=disB,cell=disC,lege=disL), onelang=onelang)
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.reColour


## .map.addAxis-------------------------2014-12-11
## Use PBSmapping's .addAxis function.
## ---------------------------------------------RH
.map.addAxis = function(side=1:4, xlim=par()$usr[1:2], ylim=par()$usr[3:4], 
     tckLab=FALSE, tck=0.014, tckMinor=0.5*tck, ...)
{
	tckLab   = rep(tckLab,   length.out = 4)
	tck      = rep(tck,      length.out = 4)
	tckMinor = rep(tckMinor, length.out = 4)

	# 1 is the horizontal axis, 2 is the vertical axis
	lim = list(xlim, ylim, xlim, ylim)
	rotate = list(0, 90, 0, 90)
	for (i in side) {
		if (((i %in% c(1, 3)) && (par()$xaxt != "n")) || ((i %in% c(2, 4)) && (par()$yaxt != "n"))) {
			ticks <- pretty(lim[[i]])
			ticksMinor <- pretty(c(0, diff(ticks)[1]))
			ticksMinor <- (sort(rep(ticks, length(ticksMinor))) + rep(ticksMinor, length(ticks)))
			ticks <- ticks[ticks > lim[[i]][1] & ticks < lim[[i]][2]]
			ticksMinor <- ticksMinor[ticksMinor > lim[[i]][1] & ticksMinor < lim[[i]][2]]
			if (!tckLab[i]) {
				tickLabels <- FALSE
			}
			else {
				tickLabels <- as.character(ticks)
			}
			axis(side = i, at = ticks, labels = tickLabels, tck = tck[i], srt = rotate[[i]], ...)
			axis(side = i, at = ticksMinor, labels = FALSE, tck = tckMinor[i], ...)
		}
	}
	invisible(NULL)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.map.addAxis


