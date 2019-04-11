\name{sumT}
\alias{sumT}
\title{
  Sum in Tonnes
}
\description{
  Sum a vector of finite values (usually catch in kg) and divide by 1000.
}
\usage{
sumT(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{numeric} -- vector of finite values (usually catch in kg).}
}
\details{
  Utility function for summarising values in grid cells as thousands of something
  -- e.g. thousands of kilograms or more simply, tonnes.
}
\value{
  A numeric scalar value that is the sum of \code{x} measured in thousands of \code{x}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-04-11}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}% use one of  RShowDoc("KEYWORDS")

