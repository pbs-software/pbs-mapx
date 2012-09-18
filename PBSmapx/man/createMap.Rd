\name{createMap}
\alias{createMap}
\title{Display an Interactive Map for Spatial Analysis}
\description{
  Display an interactive map of the BC coast that allows the user 
  to add predefined boundaries (management, surveys, isobaths) and 
  spatial tow information (e.g., catch, effort, CPUE) by species.
}
\usage{
createMap(hnam=NULL, ...) 
}
\arguments{
  \item{hnam}{string name of a history file}
  \item{...}{additional arguments for \code{createMap} (see \sQuote{Additional parameter control} below).}
}
\details{
  The function \code{createMap()} creates an interactive GUI that can be used
  to display fisheries data.

  \bold{GUI controls (top)}
  \tabular{ll}{
  \code{Coast}         \tab Coastline file (default: \code{nepacLL}).\cr
  \code{LL}            \tab Projection in Longitude-Latitude (degrees).\cr
  \code{UTM}           \tab Projection in Universal Transverse Mercator (km).\cr
  \code{Zone}          \tab UTM zone for conversion between LL and UTM.\cr
  \code{Window}        \tab Button to view the \emph{window description file}.\cr
  \code{R code}        \tab Button to view the R code that underlies \code{createMap}.\cr
  \code{SQL}           \tab Button to view a sample SQL query that gets data suitable for mapping.
  }
  \bold{GUI controls (left)}
  \tabular{ll}{
  \code{X}             \tab Longitude limits.\cr
  \code{Y}             \tab Latitude limits.\cr
  \code{Dep}           \tab Depth limits of fisheries data.\cr
  \code{YMD}           \tab Date limits of fisheries data.\cr
  \code{PMFC Major}    \tab Pacific Marine Fisheries Commission \code{major} areas.\cr
  \code{PMFC Minor}    \tab Pacific Marine Fisheries Commission \code{minor} areas.\cr
  \code{Localities}    \tab Fishing ground \code{locality} codes within PFMC minor areas.\cr
  \code{Slope RF}      \tab Slope rockfish assessment areas \code{srfa}.\cr
  \code{POP Proposed}  \tab Pacific ocean perch proposed assessment areas \code{popa}.\cr
  \code{HS Grid}       \tab Hecate Strait survey grid \code{hsgrid}.\cr
  \code{Longspine}     \tab Longspine thornyhead survey strata (WCVI) \code{ltsa}.\cr
  \code{QCS Synoptic}  \tab Queen Charlotte Sound synoptic survey strata \code{qcssa}.\cr
  \code{WCVI Synoptic} \tab West Coast of Vancouver Island synoptic survey strata \code{wcvisa}.\cr
  \code{HS Isobaths}   \tab Hecate Strait isobaths \code{hsisob}.\cr
  \code{Isobaths}      \tab British Columbia coast isobaths \code{isobath}.\cr
  \code{col}           \tab Base colour for isobaths.\cr
  \code{Grid Size}     \tab Grid cell size in X and Y units. \cr
                       \tab    \emph{Code monkey}: \code{viewCode("PBSmapx",".map.mgrid")}\cr
  \code{par.map}       \tab Allows user to tweak settings in \code{par.map} (see \sQuote{Additional parameter control} below).\cr
  \code{GOP}           \tab Grid Over Plot -- to force plotting of coloured grid cells over everything.\cr
  \code{LOP}           \tab Land Over Plot -- to force plotting of coloured land over everything.
  }
  \bold{GUI controls (right)}
  \tabular{ll}{
  \code{File}          \tab Data file name without extension (object, binary or ascii).\cr
  \code{Spp}           \tab Species code(s) separated by a comma (see \code{spn} for candidates).\cr
  \code{by Col}        \tab Is species catch in separate columns (checked) or is there one column \cr
                       \tab    for catch where the species is delimited by row? \cr
  \code{FID}           \tab Fishery ID: 1=Trawl, 2=Halibut, 3=Sablefish, 4=Dogfish-Lingcod, 5=H&L Rockfish.\cr
  \code{Z}             \tab Field name of summary values (third dimension in 3D analyses).\cr
                       \tab    The \code{>} button creates a popup box of choices comprising all available fields in \code{Qfile}.\cr
  \code{FN}            \tab Name of function that will summarize Z.\cr
  \code{track}         \tab Field name in qualified data file \code{Qfile} to track in final grid-cell results.\cr
                       \tab    The \code{>} button invokes a popup box of tracking choices comprising all available fields in \code{Qfile}.\cr
                       \tab    The \code{[]} button displays a popup box summarising the unique values for the tracked field.\cr
  \code{Tow position}  \tab Use (X,Y) coordinates of start or end of tow, or create a vectorized blend between the two. \cr
                       \tab Blend options: \cr
                       \tab    Uniform - distributed equally from start to finish; \cr
                       \tab    Normal  - distributed normally about the midpoint. \cr
  \code{Qhi}           \tab Use quantiles that emphasize high values, e.g, (0.5, 0.75, 0.9, 0.95), \cr
                       \tab    to create categories for summary data. \cr
                       \tab    \emph{Code monkey}: \code{viewCode("PBSmapx",".map.fcell")} \cr
  \code{Qeq}           \tab Use quantiles that distribute values equally, e.g, (0.2, 0.4, 0.6, 0.8). \cr
  \code{F}             \tab Use fixed break points to create categories for summary data. \cr
                       \tab    \emph{Code monkey}: \code{viewCode("PBSmapx",".map.fcell")} \cr
  \code{CFV per cell}  \tab Range in number of fishing vessels per summary grid cell.\cr
                       \tab    \code{min}: minimum number of CFVs per cell (used for public obfuscation).\cr
                       \tab    \code{max}: maximum number of CFVs per cell (for reporting purposes only).\cr
  \code{Cols}          \tab Colours for points, bubbles, and grid cells (button can re-colour tows, bubbles, or cells).\cr
                       \tab    \code{bg}: background or fill colour.\cr
                       \tab    \code{fg}: outline or border colour. \cr
  \code{tows}          \tab Show the (X,Y) positions of tows. \cr
                       \tab    \emph{Code monkey}: \code{viewCode("PBSmapx",".map.addShapes")} \cr
  \code{tsize}         \tab Size of points (\code{cex}) representing the positions of \code{tows}. \cr
  \code{grid}          \tab Show the base grid used to group event data. \cr
                       \tab    \emph{Code monkey}: \code{viewCode("PBSmapx",".map.mgrid")} \cr
  \code{bubbs}         \tab Show the (X,Y) positions of tows as bubbles proportional to the value of Z. \cr
                       \tab    \emph{Code monkey}: \code{viewCode("PBSmapx",".map.addShapes")} \cr
  \code{pow}           \tab Bubbles: transformation of Z using an exponential power \cr
                       \tab    (see \code{plotBubbles} function in PBSmodelling).\cr
  \code{psize}         \tab Bubbles: maximum size (proportional to the difference of X-width).\cr
  \code{cells}         \tab Show the coloured cells of summary data. \cr
                       \tab    \emph{Code monkey}: \code{viewCode("PBSmapx",".map.fcell")} \cr
  \code{excl 0}        \tab Exclude zero-values from the display grid (not the calculation).\cr
  \code{ex -ve}        \tab Exclude negative values from the display grid (not the calculation).\cr
  \code{land}          \tab Land colour.\cr
  \code{leg}           \tab If checked, show the legends (both title and colour-coding for cells).\cr
  \code{km}            \tab If checked, show the area (km\eqn{^2}) covered by grid cells.\cr
  \code{History}       \tab History widget for saving/viewing particular GUI settings.\cr
  \code{EPS}           \tab Re-plot the current display into an Encapsulated PostScript image file.\cr
  \code{PNG}           \tab Re-plot the current display into a Portable Networks Graphics image file.\cr
  \code{WMF}           \tab Re-plot the current display into a Windows MetaFile image file.\cr
  \code{Mfile}         \tab Refresh the Master catch file from the ASCII data source. \cr
                       \tab    \emph{Code monkey}: \code{viewCode("PBSmapx",".map.mfile")} \cr
  \code{Qfile}         \tab Refresh the Qualified catch file from the Master file. \cr
                       \tab    \emph{Code monkey}: \code{viewCode("PBSmapx",".map.qfile")} \cr
  \code{Mgrid}         \tab Refresh the background grid for data summaries. \cr
                       \tab    \emph{Code monkey}: \code{viewCode("PBSmapx",".map.mgrid")} \cr
  \code{Gevent}        \tab Refresh the events file from the Qualified catch file. \cr
                       \tab    \emph{Code monkey}: \code{viewCode("PBSmapx",".map.gevent")} \cr
  \code{Fcell}         \tab Refresh the summary cells from the events file. \cr
                       \tab    \emph{Code monkey}: \code{viewCode("PBSmapx",".map.fcell")} \cr
  \code{GO}            \tab Execute the mapping procedure with all current settings.
  }

  \bold{Additional parameter control}

  After a call to \code{createMap}, a list of additional \code{par} parameters is created (listed below).
  These additional parameters can be controlled from the GUI by hitting the \code{par.map} button,
  located near the lower left corner. The button uses the function \code{dataentry} from the \pkg{utils} 
  package, which opens a spread-sheet like editor. (Not all operating systems may be able to use this.)
  Alternatively, the user can control these parameters at any point during the interactive 
  mapping session using the \pkg{PBSmodelling} functions \code{getPBSoptions} and \code{setPBSoptions}.

  Manual control: On the command line, \code{getPBSoptions("par.map")} displays the list of additional
  parameters. One or more of the parameters can be changed by \code{setPBSoptions} using the following 
  format as an example:

  \code{setPBSoptions("par.map",list(mgp=c(3,1,0),cex.txt=1.2),sublist=TRUE)}

  \tabular{ll}{
  \code{plt}      \tab A vector of the form \code{c(x1,x2,y1,y2)} giving the coordinates \cr
                  \tab of the plot region as fractions of the current figure region. \cr
                  \tab Default: \code{c(0.08,0.99,0.08,0.99)}. \cr
  \code{mgp}      \tab The margin line (in \code{mex} units) for the axis title, axis labels and axis line. \cr
                  \tab Note that \code{mgp[1]} affects the title whereas \code{mgp[2:3]} affect the axes. \cr
                  \tab Default: \code{c(2.75,0.5,0)}. \cr
  \code{las}      \tab A numeric in \code{c(0,1,2,3)} specifying the style of axis labels: \cr
                  \tab \code{las=0} labels always parallel to the axis (default); \cr
                  \tab \code{las=1} labels always horizontal; \cr
                  \tab \code{las=2} labels always perpendicular to the axis; \cr
                  \tab \code{las=3} labels always vertical. \cr
  \code{cex.axis} \tab The magnification (default: 1.2) to be used for axis annotation relative to \cr
                  \tab the current setting of \code{cex}. \cr
  \code{cex.lab}  \tab The magnification (default: 1.75) to be used for x and y labels relative to \cr
                  \tab the current setting of \code{cex}. \cr
  \code{cex.txt}  \tab Not technically a \code{par} parameter but it denotes the magnification \cr
                  \tab (default: 0.9) of the upper right text relative to the current setting of \code{cex}. \cr
  \code{cex.leg}  \tab Not technically a \code{par} parameter but it denotes the magnification \cr
                  \tab (default: 0.9) of the lower left legend relative to the current setting of \code{cex}. \cr
  \code{pin}      \tab Reports the calculated dimensions (width, height) of the displayed plot, \cr
                  \tab which the function uses to scale an output file (where maximum dimensions are \cr
                  \tab 7.5in for \code{PNG} and 10in for \code{EPS} and \code{WMF}). Altering the \code{pin} has no effect.\cr
  \code{tit.loc}  \tab Specifies where the title box can be placed using one of \code{legend}'s keywords: \cr
                  \tab \code{c("bottomright", "bottom", "bottomleft", "left", "topleft",} \cr
                  \tab \code{  "top", "topright", "right", "center")}.\cr
  \code{leg.loc}  \tab Specifies position of colour-coded legend using relative \code{c(x,y)} coordinates \code{0:1}.\cr
  \code{leg.font} \tab Specifies legend font (e.g., 1 = normal, 2 = bold) within the \code{mono} family.
  }
}
\note{
  Users can use their own coastline as long as the data object follows
  the \pkg{PBSmapping} PolySet format.

  Regional-specific boundary files are obviously problematic, but if the user renames
  them to match the names of one of the GUI boundary PolySets (e.g., \code{major}),
  then the GUI should recognize the object.

  Users can take advantage of automatic labelling by including a comma-delimited file 
  called \code{pbs.lab} in the working directory. This file must have fields called 
  \code{EID}, \code{X}, \code{Y}, and \code{label}. The mapping routine will automatically 
  use the function \code{addLabels} with \code{placement="DATA"} if \code{pbs.map} exists.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBSdata]{testdatC}}, \code{\link[PBSdata]{testdatR}}, \code{\link[utils]{dataentry}} \cr
  \pkg{PBSmapping}: \code{\link[PBSmapping]{makeGrid}}, \code{\link[PBSmapping]{locateEvents}},
  \code{\link[PBSmapping]{combineEvents}}, \code{\link[PBSmapping]{findCells}} \cr
  \pkg{PBSmodelling}: \code{\link[PBSmodelling]{getPBSoptions}}, \code{\link[PBSmodelling]{setPBSoptions}}
}
\keyword{hplot}
\keyword{utilities}

