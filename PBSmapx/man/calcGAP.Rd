\name{calcGAP}
\alias{calcGAP}
\title{
  Calculate Gridded Area within a Polygon
}
\description{
  Calculate the gridded area within a polygon. Uses output from \code{createMap}.
}
\usage{
calcGAP(polyA, events, loc, pdata, polyID)
}
\arguments{
  \item{polyA}{PolySet object that contains at least some of the gridded output.}
  \item{events}{EventData object}
  \item{loc}{LocationSet object}
  \item{pdata}{PolyData object that summarises the gridded output}
  \item{polyID}{primary polygon IDs (PIDs) within PolySet \code{polyA}}
}
\details{
  The function uses the results in the summary object \code{PBSmap} which can either
  be generated via \code{createMap} or read in from a previous session  where 
  \code{PBSmap} was saved. The EventData object is then supplemented with fields from the 
  gridding analysis: \cr
  a) \code{PID} and \code{SID} from a LocationSet \cr
  b) \code{area} from a PolyData set.
  The amended object \code{events} is saved to a system file.
}
\value{
  Vector of polygon areas reported by \code{polyID}, including the total of these areas.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2013-01-23}
}
\seealso{
  \pkg{PBSmapping}: \code{\link[PBSmapping]{makeGrid}}, \code{\link[PBSmapping]{findCells}},
  \code{\link[PBSmapping]{combineEvents}}, \code{\link[PBSmapping]{makeProps}}
}
\examples{
\dontrun{
local(envir=.PBSmapxEnv,expr={
  data(major)
  getFile("PBSmap-5DE",tenv=penv()) # previously-saved PBSmap binary object
  events = PBSmap$events
  loc = PBSmap$locData
  pdata = PBSmap$pdata
  out = calcGAP(major,events,loc,pdata,8:9)
})
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

