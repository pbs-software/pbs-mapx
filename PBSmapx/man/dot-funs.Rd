\name{dot-funs}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{.fixNumIDs}
\alias{.win.get.lang}
\alias{.win.map.exit}
\title{
  Miscellaneous Dot Functions
}
\description{
  A ragbag of dotty funs to titillate your ocelot.
}
\usage{
.fixNumIDs(x, dig)
.win.get.lang()
.win.map.exit()
}
\arguments{
  \item{x}{\code{numeric} -- vector of real numbers}
  \item{dig}{\code{numeric} -- number of digits}
}
%- maybe also 'usage' for other objects documented here.
\details{
  \code{.fixNumIDs} -- quick fixer-upper to rectify `.createIDs' handling of SIDs

  \code{.win.get.lang} -- get language(s) for use in figure files.

  \code{.win.map.exit} -- GUI function calling \code{.map.exit()}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-12-19}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In \pkg{PBSmapx}:\cr
  \code{\link[PBSmapx]{createMap}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{interface}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M02_Extra}
% \concept{ ~cpt2 }
% Use only one concept per line.
