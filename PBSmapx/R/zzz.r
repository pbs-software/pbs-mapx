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
PBS Map Explore ", pkg_info$Version, " -- Copyright (C) 2007-2012 Fisheries and Oceans Canada

A complete user guide 'PBSmapx-UG.pdf' is located at 
", userguide_path, "

Packaged on ", pkg_date, "
Pacific Biological Station, Nanaimo
-----------------------------------------------------------


")
}
