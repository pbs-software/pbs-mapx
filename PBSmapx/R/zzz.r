# Taking cue from Roger Bivand's maptools:
.PBSmapxEnv <- new.env(FALSE, parent=globalenv())  # be sure to exportPattern("^\\.PBS") in NAMESPACE

.onAttach <- function(lib, pkg)
{
	pkg_info = utils::sessionInfo( package="PBSmapx" )$otherPkgs$PBSmapx
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- date()
	
	userguide_path <- system.file( "doc/PBSmapx-UG.pdf", package = "PBSmapx" )
	year <- substring(date(),nchar(date())-3,nchar(date()))

	packageStartupMessage("
-----------------------------------------------------------
PBS Map Explore ", pkg_info$Version, " -- Copyright (C) 2007-",year," Fisheries and Oceans Canada

A complete user guide 'PBSmapx-UG.pdf' is located at 
", userguide_path, "

Packaged on ", pkg_date, "
Pacific Biological Station, Nanaimo

All available PBS packages can be found at
https://github.com/pbs-software/pbs-mapx

Type 'createMap()' on the command line to start the map GUI.
-----------------------------------------------------------

")
}
.onUnload <- function(libpath) {
	rm(.PBSmapxEnv)
}

# No Visible Bindings
# ===================
if(getRversion() >= "2.15.1") utils::globalVariables(names=c(
	".change",".coast",".createFastIDdig",".createIDs",".map.exit",".validateEventData",
	"addBord","addLabel","addLabels","addLegend","addLines","addPoints","addPolys","agrid","as.EventData","as.PolySet",
	"bg","bo","byC","byrow",
	"calcArea","cells","cex.leg","cex.txt","clipPolys","combineEvents","convUL","createWin",
	"disA","disB","disC","disG","disL","disT","dlim",
	"eN","emon","events","ex0","exneg",
	"findCells","findPolys","fg","fid","Flevs","fn","fnam",
	"gear", "getPBSoptions","getWinAct","getWinVal","gtype",
	"hsi","hsisob",
	"icol","is.PolyData","isob","isobath",
	"land","lang","leg.font","leg.loc","lisp",
	"makeGrid","makeProps","Mfile",
	"packList","pbs.pset","PBSmap","pdata","plotMap","pmon","powr","projection","pset","psize","pval",
	"Q","Qfile","Qmon",
	"resetGraph",
	"setPBSoptions","setWinVal","showAlert","spn","spp","strSpp",
	"tcall","tdata","testdatC","tget","tit.loc","tprint","tput","track","tsize",
	"unpackList",
	"Vmin","vval",
	"xlim",
	"ylim",
	"zfld","zI","zlim","zone"
	), package="PBSmapx")

