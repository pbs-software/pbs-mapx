## PBSmapx: Map explore GUI for PBSmapping ##
&copy; Fisheries and Oceans Canada (2007-2024)

The package **PBSmapx**, which is short-hand notation for _PBS Map Explore_, provides a convenient Graphical User Interface (GUI) tool for interacting with **PBSmapping**. The primary mapping function `createMap` presents a menu system for plotting topographical files as well as output from gridding and summary routines for geo-referenced data such as catch, effort, and catch per unit effort (CPUE). The underlying R code is somewhat convoluted; however, the user can largely ignore the code and simply manipulate the entries in the GUI menu. Though designed for users at the Pacific Biological Station (PBS), the GUI has proven useful for users at other locales. 

**PBSmapx** depends on the R package **PBStools**, which in turn depends on four other R packages: **PBSmapping**, **PBSmodelling**, **PBSdata**, and **RODBC**. We use **PBSmodelling** to implement the GUI. Originally, functions in **PBSmapx** evolved over time (2007-2012) within the R package **PBSfishery**, along with useful datasets (regional boundaries, key codes, example data) and handy utility functions. In April 2012, we decided to split **PBSfishery** into three separate libraries -- **PBStools**, **PBSmapx**, and **PBSdata** -- for public distribution (see <a href="https://github.com/pbs-software">pbs-software</a>). The three packages experience different rates of change, with **PBStools** undergoing frequent revision, while **PBSdata** and **PBSmapx** can remain unchanged for long periods of time.

Although **PBSmapx** is not available on <a href="https://cran.r-project.org/">CRAN</a> (Comprehensive R Archive Network), the package (Windows binary and source tarball) is built after using CRAN's rigorous `R CMD check --as-cran` routine (using R-devel on a **Windows 7** 64-bit system) and posted to <a href="https://drive.google.com/drive/folders/0B2Bkic2Qu5LGOGx1WkRySVYxNFU?usp=sharing">Google Drive</a>. Most of the time, the revision on <a href="https://github.com/pbs-software/pbs-mapx">GitHub</a> can be built (supposedly) in R using `devtools::install_github("pbs-software/pbs-mapx/PBSmapx")`; however, not every revision has been checked for CRAN worthiness.

In July 2018, the package was updated to use a new function in **PBStools** called `linguaFranca`, which translates a limited set of BC stock assessment english words that appear in figures into french. This feature was largely driven by <a href="http://www.dfo-mpo.gc.ca/csas-sccs/process-processus/translation-traduction-eng.html">CSAS' mandate</a> to make all scientific research documents accessible to anglophone and francophone readers.

As with any freely available product, there is no warranty or promise that **PBSmapx** will perform adequately for all circumstances. Additionally, coding errors are possible, and users should contact the package maintainer if bugs are detected.

Maintainer: <a href="mailto:rowan.haigh@dfo-mpo.gc.ca">Rowan Haigh</a>

<p align="right"><img src="DFOlogo_small.jpg" alt="DFO logo" style="height:30px;"></p> 
