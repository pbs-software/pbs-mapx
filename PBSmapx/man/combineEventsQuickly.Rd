\name{combineEventsQuickly}
\alias{combineEventsQuickly}
\title{Combine Measurements of Events Quickly}
\description{
  Combine measurements associated with events that occur in the same
  polygon. This function uses a revised technique from that used by
  \code{combineEvents} in \pkg{PBSmapping} to speed up the routine, 
  especially for large datasets (many events, many grid locations).
}
\usage{
combineEventsQuickly (events, locs, FUN, \dots, bdryOK = TRUE)
}
\arguments{
  \item{events}{\link[PBSmapping]{EventData} with at least four columns (\code{EID},
    \code{X}, \code{Y}, \code{Z}).}
  \item{locs}{\link[PBSmapping]{LocationSet} usually resulting from a call to
    \code{\link[PBSmapping]{findPolys}}.}
  \item{FUN}{a function that produces a scalar from a vector
    (e.g., \code{\link[base]{mean}}, \code{\link[base]{sum}}).}
  \item{\dots}{optional arguments for \code{FUN}.}
  \item{bdryOK}{Boolean value; if \code{TRUE}, include boundary points.}
}
\details{
  This function combines measurements associated with events that occur
  in the same polygon.  Each event (\code{EID}) has a corresponding
  measurement \code{Z}.  The \code{locs} data frame (usually output from
  \code{\link[PBSmapping]{findPolys}}) places events within polygons.  Thus, each
  polygon (\code{PID}, \code{SID}) determines a set of events within it,
  and a corresponding vector of measurements \code{Zv}.  The function
  returns \code{FUN(Zv)}, a summary of measurements within each polygon.
}
\value{
  \link[PBSmapping]{PolyData} with columns \code{PID}, \code{SID} (\emph{if in}
  \code{locs}), and \code{Z}.
}
\note{
  The primary difference between this routine and the one in \pkg{PBSmapping}
  is that location polygon ID (\code{PID, SID}) is split by location event ID
  (\code{EID}) rather than vice versa.

  The routine also uses the \pkg{PBSmapping} hidden function \code{.createIDs};
  which renders the polygon ID a numeric composite of \code{PID} and \code{SID}.
  Unfortunately, the \code{SID} as a decimal loses trailing zero digits;
  for example, \code{PID=149} and \code{SID=50} becomes \code{ID=149.5}.
  Fortunately, this can be rectified by code, which it is.
}
\author{
  Rowan Haigh, Research Biologist,\cr
  Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC.
}
\seealso{
  \code{\link[PBSmapx]{createMap}},
  \code{\link[PBSmapping]{combineEvents}},
  \code{\link[PBSmapping]{findCells}},
  \code{\link[PBSmapping]{locateEvents}},
  \code{\link[PBSmapping]{makeGrid}},
  \code{\link[PBSmapping]{makeProps}}.
}
\examples{
local(envir=.PBSmapxEnv,expr={
  N = 10000
  xlim=c(-131,-129); ylim=c(50.5,52); cells=c(0.1,0.075)
  ##--- Construct a grid
  gx = seq(xlim[1],xlim[2],cells[1]); gy = seq(ylim[1],ylim[2],cells[2])
  agrid = makeGrid(x=gx,y=gy,projection="LL",zone=9)
  events = as.EventData(data.frame(EID=1:N, X=runif(N,xlim[1],xlim[2]),
    Y=runif(N,ylim[1],ylim[2]), Z=rlnorm(N,1,2)), projection="LL",zone=9)
  ##--- Calculate the location dataset (events in grid)
  ldata = findCells(events,agrid)  ## location data
  ##--- Summarize the events using the location data
  pdata = combineEventsQuickly(events, ldata, FUN=mean)
  brks  = quantile(pdata$Z,c(0,0.5,0.75,0.9,0.95,1))
  pdata = makeProps(pdata, breaks=brks, propName="col",
    propVals=c("blue","green","yellow","orange","red"))
  ##--- Plot the results
  data(nepacLL)
  expandGraph(mfrow=c(1,1), mar=c(3,4,0.5,0.5), mgp=c(2,0.5,0))
  plotMap(nepacLL, xlim=xlim+c(-1,1), ylim=ylim+c(-0.5,0.5), 
    plt=NULL, col="gainsboro", cex.axis=1.2, cex.lab=1.5)
  addPolys(agrid, polyProps=pdata, border="#DFDFDF", lwd=0.2)
})
}
\keyword{manip}

