# Taking cue from Roger Bivand's maptools:
.PBSmapxEnv <- new.env(FALSE, parent=globalenv())  # be sure to exportPattern("^\\.PBS") in NAMESPACE

.onLoad <- function(lib,pkg)
{
	pkg_info = utils::sessionInfo( package="PBSmapx" )$otherPkgs$PBSmapx
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- date()
	
	userguide_path <- system.file( "doc/PBSmapx-UG.pdf", package = "PBSmapx" )
	
	packageStartupMessage("
-----------------------------------------------------------
PBS Map Explore ", pkg_info$Version, " -- Copyright (C) 2007-2013 Fisheries and Oceans Canada

A complete user guide 'PBSmapx-UG.pdf' is located at 
", userguide_path, "

Packaged on ", pkg_date, "
Pacific Biological Station, Nanaimo

All available PBS packages can be found at
http://code.google.com/p/pbs-software/

Type 'creatMap()' on the command line to start the map GUI.
-----------------------------------------------------------

")
}
.onUnload <- function(libpath) {
	rm(.PBSmapxEnv)
}

# No Visible Bindings
# ===================
if(getRversion() >= "2.15.1") utils::globalVariables(names=c(
	".coast",".map.exit",
	"agrid",
	"bg","bo","byC",
	"cells","cex.leg","cex.txt",
	"disA","disB","disC","disG","disL","disT","dlim",
	"eN","emon","events","ex0","exneg",
	"fg","fid","Flevs","fn","fnam",
	"hsi","hsisob",
	"icol","isob","isobath",
	"land","leg.font","leg.loc",
	"m1","m2","m3","m4","m5","Mfile",
	"PBSmap","pdata","pmon","powr","projection","pset","psize","pval",
	"Q","Qfile","Qmon",
	"s1","s2","s3","s4","spn","spp","strSpp",
	"testdatC","tdata","tit.loc","track","tsize",
	"Vmin","vval",
	"xlim",
	"ylim",
	"zfld","zI","zlim","zone"
	), package="PBSmapx")

