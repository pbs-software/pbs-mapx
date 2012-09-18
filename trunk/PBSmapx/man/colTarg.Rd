\name{colTarg}
\alias{colTarg}
\title{
  Colour Target Events
}
\description{
  Colour points depending on a target field from qualified data.
}
\usage{
colTarg(dat=testdatC, tfld="cfv", qfld="year", qval=NULL, 
   clrs=c("red","orange","yellow","green","blue"), border=0, add=TRUE, ...)
}
\arguments{
  \item{dat}{data frame of events with \code{X} and \code{Y} coordinates.}
  \item{tfld}{target field to group events}
  \item{qfld}{qualifier field to filter events}
  \item{qval}{values in qualifier field to select}
  \item{clrs}{colours to identify unique targets}
  \item{border}{border colour of points}
  \item{add}{logical: if \code{TRUE}, add to an existing plot, otherwise make a new plot}
  \item{\dots}{additional arguments to pass to the function \code{addPoints}}
}
\details{
  This function serves as a handy utility to add coloured points to an existing plot.
  If the user knows R well, he can do it using a variety of native functions also.
}
\value{
  Invisibly returns a list object with the following components:
  \item{tdat}{data.frame of qualified target data}
  \item{Ctar}{vector of colours named by target group}
  \item{qval}{vector of qualifier values used}
  \item{xlim}{vector limits of the x-axis plotted}
  \item{ylim}{vector limits of the y-axis plotted}
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\note{
  Function requested by Paul J. Starr (Canadian Groundfish Research and Conservation Society)
}
\seealso{
	\code{\link[PBSmapx]{createMap}} \cr
	\pkg{PBSmapping}: \code{\link[PBSmapping]{as.EventData}},
	\code{\link[PBSmapping]{addPoints}}, \code{\link[PBSmapping]{plotMap}} \cr
	\pkg{PBSdata}: \code{\link[PBSdata]{testdatC}}
}
\examples{
pbsfun = function(N=5){
   data(testdatC); data(nepacLL)
   cfvs = rev(sort(sapply(split(testdatC$EID,testdatC$cfv),length)))
   topN = names(cfvs)[1:N]
   dat = testdatC[is.element(testdatC$cfv,topN),]
   dat$year = as.numeric(substring(dat$date,1,4))
   clrs = rainbow(N)# sample(colors(),N)
   out = colTarg(dat=dat,tfld="cfv",qfld="year",qval=2001:2005,
         clrs="transparent",border="transparent",cex=0.75,add=FALSE)
   plotMap(nepacLL,xlim=out$xlim,ylim=out$ylim,col="moccasin")
   out = colTarg(dat=dat,tfld="cfv",qfld="year",qval=2001:2005,
         clrs=clrs,border="grey30",cex=0.75,add=TRUE)
   legend("bottomleft",inset=.10,title="Years 2001-2005",pch=21,
          pt.bg=clrs,legend=paste("CFV",topN),bty="n")
   invisible(out) }
pbsfun()
}
\keyword{aplot}
\keyword{utilities}
