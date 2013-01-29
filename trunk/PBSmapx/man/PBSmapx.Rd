\name{PBSmapx}
\docType{package}
\alias{PBSmapx}
\alias{PBSmapx-package}
\title{PBS Map Explore}

\description{
  The package \pkg{PBSmapx}, which is short-hand notation for \emph{PBS Map Explore}, 
  provides a convenient Graphical User Interface (GUI) tool for interacting with \pkg{PBSmapping}. 
  The primary mapping function \code{createMap} presents a menu system for plotting 
  topographical files as well as output from gridding and summary routines for geo-referenced 
  data such as catch, effort, and catch per unit effort (CPUE). The underlying R code is 
  somewhat convoluted; however, the user can largely ignore the code and simply manipulate 
  the entries in the GUI menu. Though designed for users at the Pacific Biological Station (PBS), 
  the GUI has proven useful for users at other locales.

  \pkg{PBSmapx} depends on the R package \pkg{PBStools}, which in turn depends on four 
  other R packages: \pkg{PBSmapping}, \pkg{PBSmodelling}, \pkg{PBSdata}, and \pkg{RODBC}. 
  We use \pkg{PBSmodelling} to implement the GUI. Originally, \pkg{PBSmapx} evolved over 
  time (2007--2012) within the R package \pkg{PBSfishery} (found on Google's Project 
  Hosting site: \url{http://code.google.com/p/pbs-fishery/}), along with useful datasets 
  (regional boundaries, key codes, example data) and handy utility functions. In April 2012, 
  we decided to split \pkg{PBSfishery} into three separate libraries -- \code{PBSmapx}, 
  \code{PBSdata}, and \code{PBStools} -- for distribution on CRAN.

  \pkg{PBSmapx} contains the following functions:
  \tabular{ll}{
  \bold{Mapping}    \tab \cr
  \code{createMap}  \tab \bold{GUI:} Map geo-referenced data.\cr
  \code{calcGAP}    \tab Calculate gridded area within a polygon.\cr
  \code{colTarg}    \tab Colour points depending on a target field.
  }

  Other package resources:
  \tabular{ll}{
  \code{../library/PBSmapx/doc}   \tab includes the User's Guide describing all functions.\cr
  \code{../library/PBSmapx/sql}   \tab SQL code files for querying remote databases.\cr
  \code{../library/PBSmapx/win}   \tab \pkg{PBSmodelling} \emph{window description files} for the main GUI.
  }
}
\seealso{
  \code{\link[PBSmapping]{nepacLL}}, \code{\link[PBStools]{getData}} \cr
  \code{\link[PBStools]{ServerParlance}} for differences between SQL Server and Oracle database organisation.\cr
  \code{\link[PBStools]{SQLcode}} for available SQL queries.\cr
}
\keyword{package}

