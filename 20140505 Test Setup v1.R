# Matthew Pillmeier
# Temporary R Working File

# ----- Version -----
R.version
getRversion()
sessionInfo()
packageVersion("stats")
cat("\014")   # Same as 'Ctrl-L', clears Console

# ----- Library -----
# Default library specified in Windows Environment variable R_LIBS_USER
.libPaths()
#.libPaths("C://R/Libraries//RRO-3.2.2")  # Default
#.libPaths("C://R/Libraries//R-3.2.2")   #
#.libPaths("C://R/Libraries//R-2.15.3")   #
.Library
.Library.site

# ----- Rprofile -----
# Location: etc/Rprofile.site
# Removed the RRO default repository (located in etc\Rprofile.site) because it utilizes a specific snapshot 
# date and therefore prevents updates to the latest version
getOption('repos')

# ----- Packages -----
# Github Development Packages
# For some reason, the downloads from github produce the following SSL Certificate error. To ignore
# this warning, setup `httr` to pass to `RCurl` a flag to ignore verification.
#   Error in function (type, msg, asError = TRUE)  : 
#   SSL certificate problem, verify that the CA cert is OK. Details:
#   error:14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed
library(RCurl)
library(httr)
set_config( config( ssl_verifypeer = 0L ) )   # Ignores the SSL check; Was once 'ssl.verifypeer'
# For projects pulled down, insert 'sslVerify = false' into 'gitconfig' file
#https://github.com/hadley/devtools
#install_github("hadley/devtools")
library(devtools)
install_github("hadley/lineprof")


# Data.Table
# https://github.com/Rdatatable/data.table/
#install_github("Rdatatable/data.table", build_vignettes=FALSE)
library(data.table)
#ls("package:data.table")
#lsf.str("package:data.table")
#methods(class="data.table")

# Time
# http://www.rforge.net/fasttime/files/
#install.packages("C:/R/Downloaded/fasttime_1.0-0.tar.gz", repos = NULL, type = "source")
library(fasttime)   # fastPOSIXct assumes an input format of "%Y/%m/%d %H:%M/%S", no way to change it
#install_github("hadley/RcppDateTime")

# Data Manipulation
#library(reshape)
library(reshape2)

# Graphics
#install_github("hadley/ggplot2")
library(ggplot2)
library(gridExtra)
# R Shiny (https://github.com/rstudio/shiny)
install_github("rstudio/htmltools")
install_github("bokeh/rbokeh")
install_github("rstudio/shiny")
#library(shiny)
# R ggvis (https://github.com/rstudio/ggvis)
install_github("rstudio/ggvis", build_vignettes = FALSE)
#library(ggvis)
# R Charts
install_github("ramnathv/rCharts")
library(rCharts)
# Google Vis
library(googleVis)
# Plotly
install_github("ropensci/plotly")
library(plotly)
# Waffle Plots
install_github("hrbrmstr/waffle")
library(waffle)
# DiaGraphs
install_github('rich-iannone/DiagrammeR')
library(DiagrammeR)
# Network Graphs
library(networkD3)
library(igraph)
# Rattle (Beginners R GUI)
#install_bitbucket("kayontoga/rattle")
# Slidify
install_github("ramnathv/slidify")
# ggtree
install_github("GuangchuangYu/ggtree")
# Vegalite
install_github("hrbrmstr/vegalite")


# Documents
#install.packages(c("shiny", "shinyFiles", "rmarkdown"))
install_github("rstudio/shinybootstrap2")
install_github("trestletech/shinyAce")
install_github("ebailey78/shinyBS")
install_github("swarm-lab/editR")
library(editR)

# String Manipulation
#install_github('Rexamine/stringi')
library(stringi)  # Currently using CRAN version
#library(stringr)

# Excel
library(XLConnect)
library(xlsx)
install_github("hadley/readxl")
library(readxl)
library(openxlsx)

# Misc File Reads
install_github("hadley/readr")

# DB
library(RODBC)
library(RODBCext)
library(RSQLite)
library(RSQLite.extfuns)
.SQL92Keywords   # SQL Keywords
library(RMySQL)

# XML
install_github("hadley/xml2")
library(xml2)

# Operation Piping
# R magrittr (https://github.com/smbache/magrittr)
#install_github("smbache/magrittr")
library(magrittr)

# Functional Programming Tools
library(lambda.r)
install_github("hadley/purrr")
library(purrr)

# Parallel Computing
library(snow)
# Revobase was previously installed by default, now apparently left as a zip in the /etc directory
# Parallel support now in a separate install?
# install.packages("C:/R/RRO-3.1.2/etc/Revobase_7.3.0.zip")
library(RevoUtilsMath) # Intel MKL thread control available in RRO only
getMKLthreads()
#setMKLthreads(2)
# Parallel Tools
install_github("matloff/partools")
library(partools)
install.packages("foreach")
install.packages("iterators")
install.packages("doMC")
install.packages("doParallel")
install.packages("doSNOW")

install_github("nathanvan/parallelsugar")

# Hadoop
install_github("RevolutionAnalytics/RHadoop")


# Dplyr (https://github.com/hadley/dplyr)
#install_github("hadley/lazyeval")
#install_github("hadley/dplyr", build_vignettes=F)
library(dplyr)
library(tidyr)
install_github("MangoTheCat/tidyshiny")
install_github("hadley/dtplyr")

# R Pivot Table
install_github("ramnathv/htmlwidgets")
install_github("smartinsightsfromdata/rpivotTable")

# Scheduling
install_github("jwijffels/taskscheduleR")

# Rmetrics
#source("http://www.rmetrics.org/Rmetrics.R")
#install.Rmetrics()

# Rcpp
# http://www.rcpp.org/
#library(Rcpp)

# Rcpp11
# https://github.com/Rcpp11/Rcpp11
install_github("Rcpp11/Rcpp11")
install_github("Rcpp11/attributes")
#library(Rcpp11)
#library(attributes)

# R6 (OO System)
library(R6)

# Timing
library(microbenchmark)

# GenomicRanges
# Run R As Administrator ...
source("http://bioconductor.org/biocLite.R")
biocLite("GenomicRanges")
library(GenomicRanges)

# SVG
install_github("duncantl/SVGAnnotation")
#library(SVGAnnotation)

# Rmetrics
source("http://www.rmetrics.org/Rmetrics.R")
install.Rmetrics()

# Language Inspection
install_github("hadley/pryr")
library(pryr)

# Fast DF Write
install_github("wesm/feather/R")

# ----- Settings -----
search()
session_info()

# Timezone
Sys.timezone()
Sys.setenv(TZ='GMT')
#Sys.setenv(TZ="America/New_York")

# Options
options(max.print=100)
options(data.table.nomatch=0)
options(stringsAsFactors=F)
getOption("na.action")
getOption("data.table.nomatch")

# Snow Cluster
# `parLapply` did not show obvious gains
#num.cores <- 4
#cluster <- makeSOCKcluster(rep("localhost",num.cores))
#clusterEvalQ(cluster, library(data.table))
#clusterEvalQ(cluster, library(magrittr))
#clusterEvalQ(cluster, library(stringi))
#stopCluster(cluster)



#search()
#detach("package:dplyr")
#library(googleVis)
#library(ggVis)
build_github_devtools()

# Example code
a.dt <- data.table(a=rep(LETTERS[1:10],10), b=rnorm(100,0,1), c=1:100)
setkey(a.dt, a)
b.dt <- a.dt[, list(mean(b), sd(b)), by=a]

p1 <- ggplot(b.dt, aes(x=a, y=V1))
p1 <- p1 + geom_bar(stat='identity',fill='red')
p1

p2 <- ggplot(a.dt, aes(x=c, y=b))
p2 <- p2 + geom_line()
p2 <- p2 + geom_point()
p2 <- p2 + geom_smooth(se=F)
p2

p3 <- ggplot(a.dt, aes(x=b))
p3 <- p3 + geom_density()
p3 <- p3 + facet_wrap(~ a)
p3

p.sub.1 <- arrangeGrob(p1, p2, nrow=1)
p.all <- arrangeGrob(p.sub.1, p3, nrow=2)
print(p.all)

setkey(All.Hrly.dt, Year, Month, PKPRD_2)
res.1 <- microbenchmark(t.1 <- All.Hrly.dt[,mean(`LMP.DA.NY-K`, na.rm = T), by=list(Year, Month, PKPRD_2)], times=1000L)
res.2 <- microbenchmark(t.2 <- All.Hrly.dt %>% 
          select(`LMP.DA.NY-K`, Year, Month, PKPRD_2) %>%
          group_by(Year, Month, PKPRD_2) %>%
          summarize(V1=mean(`LMP.DA.NY-K`, na.rm=T)),
          times=1000L)

plt <- ggplot2::qplot(y=time, data=res.1, colour=expr)
plt <- plt + ggplot2::scale_y_log10()
print(plt)

x <- 10
my.f <- function() {
  i <- x
  t.f <- function() {
    i <<- i + 1
    i
  }
}

s.f <-my.f()
x <- 20
s.f()
x

install.packages('RMySQL',type='source')
readRegistry("SOFTWARE\\MySQL AB", hive="HLM", maxdepth=2)

library(RODBC)
ch <- odbcConnect("sakila", "A00113700", "Password1")
sqlTables(ch)

library(dplyr)
my_db <- src_mysql("sakila", user="A00113700", password="Password1")
my_tbl <- tbl(my_db, "actor")

library(RMySQL)
library(data.table)
library(magrittr)
my_db <- dbConnect(MySQL(), username='A00113700', password='Password1', dbname='test')
dbListTables(my_db)
t.1.dt <- data.table(a=1:10, b=LETTERS[1:10], c=ISOdate(2014, 10, 11:20, 0, tz='GMT'))
str(t.1.dt)
dbWriteTable(my_db, 't_1', t.1.dt)
t.2.dt <- data.table(a=round(runif(1e6)*100,0), 
                     b=LETTERS[sample(1:26, 1e6, T)], 
                     c=ISOdate(2014, sample(1:12, 1e6, T), sample(1:28, 1e6, T), 0, tz='GMT'))
setkey(t.2.dt, c)
t.2.dt[,d:=strftime(c, "%Y-%m-%d"), by=c]
t.2.dt[,c:=NULL]
str(t.2.dt)
dbWriteTable(my_db, 't_2', t.2.dt, row.names=FALSE)

res <- dbSendQuery(my_db, 'select count(*) from t_2')
t.3.df <- fetch(res, n=-1)
dbClearResult(res)

res <- dbSendQuery(my_db, "select * from t_2")
res <- dbSendQuery(my_db, "select distinct(d) As Date, count(a) As Cnt, avg(a) As Mean from t_2 group by d order by d")
t.4.dt <- fetch(res,n=-1)
setDT(t.4.dt)
t.4.dt
dbClearResult(res)

dbDisconnect(my_db)

rm(list=ls())
gc()


# Aligne Price Formulas
setwd("X:/Zainet/odbs")
getwd()

pcurve.dt <- fread("MRP_PCURVE.rpt", sep="|")
pcurve.dt[,V9:=NULL]
pcurve.dt %>% str

pricedtl.dt <- fread("MRP_PRICEDTL.rpt", sep="|")
pricedtl.dt[,V5:=NULL]
pricedtl.dt %>% str

file.name <- "MRP_PFORMULA.rpt"
pformula.char.1 <- readChar(file.name, file.info(file.name)$size)
pformula.char.2 <- stri_replace_all_fixed(pformula.char.1, pattern="\r\n", replacement="")
pformula.char.3 <- stri_replace_all_regex(pformula.char.2, "([^|]*\\|[^|]*\\|[^|]*\\|[^|]*\\|[^|]*\\|[^|]*\\|)", "$1\r\n")
pformula.dt <- fread(pformula.char.3, sep="|",colClasses=list(character=2:5, integer=c(1,6)))
pformula.dt[, V7:=NULL]
pformula.dt %>% str


# Join the tables
pcurve.dt %>% setkey(PCURVE_IDX)
pricedtl.dt %>% setkey(PRICEDTL_PPOINT)
prc.crv.dt <- pricedtl.dt[pcurve.dt]

prc.crv.dt %>% setkey(PRICEDTL_FORMULA)
pformula.dt %>% setkey(PFORMULA_IDX)
prc.all.dt <- pformula.dt[prc.crv.dt]
prc.all.dt %>% str

t <- lapply(c("pcurve.dt", "pricedtl.dt", "pformula.dt", "prc.all.dt"), . %>% get %T>%  (. %>% nrow %>% print))

prc.all.dt %>% setkey("PCURVE_PSET", "PCURVE_MKT", "PCURVE_COMP")
prc.all.dt[J("MARKET", "PJMDAY", "MANIT2"), list(PFORMULA_NAME, PFORMULA_DSC, PFORMULA_TEXT)]
prc.all.dt[J("MARKET", "PJMDAY", "OCCOQ2"), list(PFORMULA_NAME, PFORMULA_DSC, PFORMULA_TEXT)]
prc.all.dt[J("MARKET", "PJMDAY", "BRNSWK"), list(PFORMULA_NAME, PFORMULA_DSC, PFORMULA_TEXT)]
prc.all.dt[J("MARKET", "PJMDAY", "DOMZON"), list(PFORMULA_NAME, PFORMULA_DSC, PFORMULA_TEXT)]
prc.all.dt[J("MARKET", "NEPOOL", "UNTIL"), list(PFORMULA_NAME, PFORMULA_DSC, PFORMULA_TEXT)]
prc.all.dt[J("ONPK", "NEPOOL", "STOWE"), list(PFORMULA_NAME, PFORMULA_DSC, PFORMULA_TEXT)]
prc.all.dt[J("MARKET", "NEPOOL", "CMEEC"), list(PFORMULA_NAME, PFORMULA_DSC, PFORMULA_TEXT)]
prc.all.dt[J("ONPK", "NEPOOL", "SUEZ"), list(PFORMULA_NAME, PFORMULA_DSC, PFORMULA_TEXT)]
prc.all.dt[J("ONPK", "NEPOOL", "CMEEC"), list(PFORMULA_NAME, PFORMULA_DSC, PFORMULA_TEXT)]
prc.all.dt[,list(PFORMULA_NAME, PFORMULA_DSC, PFORMULA_TEXT)]


prc.mkts.dt <- prc.all.dt[,list(GRP=.GRP), keyby=list(PCURVE_MKT, PCURVE_COMP, PCURVE_PSET)]
prc.mkts.dt[,GRP:=NULL]
prc.mkts.dt[J("POLR", "PPLR10")]


wordcensus <- function(basename,ndigs) {
  # find the file chunk to be handled by this worker
  fname <- filechunkname(basename,ndigs)
  words <- scan(fname,what="")
  # determine which words occur how frequently in this chunk
  tapply(words,words,length, simplify=FALSE)
}

fullwordcount <- function(cls,basename,ndigs) {
  setclsinfo(cls) # give workers ID numbers, etc.
  clusterEvalQ(cls,library(partools))
  # have each worker execute wordcensus()
  counts <- clusterCall(cls,wordcensus,basename,ndigs)
  # coalesce the output for the overall counts
  addlistssum <- function(lst1,lst2) addlists(lst1,lst2,sum)
  Reduce(addlistssum,counts)
}

test <- function() {
  cls <- makeCluster(2)
  # make a test file, 2 chunks
  cat("How much wood","Could a woodchuck chuck",file="test.1",sep="\n")
  cat("If a woodchuck could chuck wood?",file="test.2")
  # set up cluster
  cls <- makeCluster(2)
  # find the counts
  fullwordcount(cls,"test",1)
}



library(plotly)
# Fill in with your personal username and API key
# or, use this public demo account
py <- plotly(username='R-Demo-Account', key='yu680v5eii')

trace1 <- list(
  x = c(1, 2, 3), 
  y = c(4, 3, 2), 
  type = "scatter"
)
trace2 <- list(
  x = c(20, 30, 40), 
  y = c(30, 40, 50), 
  xaxis = "x2", 
  yaxis = "y2", 
  type = "scatter"
)
data <- list(trace1, trace2)
layout <- list(
  yaxis2 = list(
    domain = c(0.6, 0.95), 
    anchor = "x2"
  ), 
  xaxis2 = list(
    domain = c(0.6, 0.95), 
    anchor = "y2"
  )
)
response <- py$plotly(data, kwargs=list(layout=layout, filename="simple-inset", fileopt="overwrite"))
url <- response$url


library(R6)

env.tree.f <- function(x) {
  par.env <- parent.env(x)
  print(attr(par.env, "name"))
  
  if(!identical(par.env, emptyenv())) {
    env.tree.f(par.env)
  }
}

library(DiagrammeR)
grViz("boxes.dot")


library(data.table)
library(magrittr)
library(stringi)
getwd()
setwd("C:/Temp/ZEMA/2015_05_05b/FH/FH/")
lf <- list.files()
lf.dt <- as.data.table(lf)
setnames(lf.dt, "lf", "File_Name")
lf.dt[,`:=`(c('Run_Type', 'ProfileNameSC', 'ProviderSC', 'TariffSC', 'RunTypeSC',
              'RF_Cmmdty', 'RF_Und_Src', 'RF_Mod_1', 'RF_Mod_2', 'RF_Mod_3', 'RF_Basis'),
            as.list(stri_split_regex(File_Name, "_|\\.")[[1]][c(1,2,3,4,5,7,8,9,10,11,12)])), by=File_Name]
lf.dt[,`:=`('AsOf', as.IDate(stri_match_all_regex(File_Name, "(\\d{2}-\\d{2}-\\d{4})\\.csv$")[[1]][2], "%m-%d-%Y", tz='GMT')), by=File_Name]
lf.dt %>% str
      

# Test GBM
# Illustrates that the price level itself directly affects the GMaR (in this case using a single unit long position)
library(data.table)
library(magrittr)
library(string)
library(ggplot2)

sigma <- 0.5
t <- 1
t.dt <- data.table(Sim=1:10000)
t.dt[,`:=`(as.character(seq(1,100, 0.5)), as.list(seq(1,100,0.5)))]
t.2.dt <- melt(t.dt, id.vars = "Sim", variable.name = "Stress", variable.factor = F, value.name = "Prc@t0")
t.2.dt[, `Prc@t1`:=`Prc@t0`*exp(-0.5*(sigma)^2*t+(sigma)*(rnorm(.N))*sqrt(t))]
t.2.dt[,PrcDiff:=`Prc@t1`-`Prc@t0`]
t.3.dt <- t.2.dt[,list(`Prc@t1`=sort(`Prc@t1`) %>% `[`(floor(.N*0.05))), by=list(Stress, `Prc@t0`)]
t.3.dt[,GMaR:=`Prc@t1`-`Prc@t0`]

t.3.dt[,`:=`(`Chg%`=GMaR/`Prc@t0`, `ChgLn%`=log(`Prc@t1`/`Prc@t0`))]

a <- t.3.dt %>% ggplot(aes(x=`Prc@t0`, y=GMaR)) + geom_point() + geom_line(); a
b <- t.3.dt %>% ggplot(aes(x=`Prc@t0`, y=`Chg%`)) + geom_point() + geom_line() + coord_cartesian(ylim=c(-0.9, 0)); b
c <- t.3.dt %>% ggplot(aes(x=`Prc@t0`, y=`ChgLn%`)) + geom_point() + geom_line() + coord_cartesian(ylim=c(-1, 0)); c

# Lacima Profile Testing
.libPaths("C://R/Libraries//RRO_3.2.2")
library(data.table)
library(magrittr)
library(ggplot2)
setwd("C:/Temp/Lacima")

as.IDate.mdY.1.f <- . %>% as.IDate(., "%m/%d/%Y", tz="America/New_York")

prof.dt <- fread("20151123 PJM West v1.csv", header=T)

prof.2.dt <- melt(prof.dt, id.vars = "Date", variable.name = "HE.Chr", variable.factor = F, value.name = "Prc")
prof.2.dt[,HE:=as.numeric(HE.Chr)]
prof.2.dt[,HE.Chr:=NULL]
prof.2.dt[,Date.IDt:=as.IDate.mdY.1.f(Date)]
prof.2.dt[,":="(Mnth=month(Date.IDt), 
                WkDay=(wday(Date.IDt)+5)%%7+1,
                WeekDay=strftime(Date.IDt, "%a"))]
prof.2.dt[,Pk:=0L]
prof.2.dt[CJ(WkDay=c(1:5), HE=c(8:23)),Pk:=1, on=c("WkDay", "HE")]
setorder(prof.2.dt, Date.IDt, HE)
prof.2.dt %>% head(24)

# Setup
all.pk.avg.dt <- prof.2.dt[,list("All.Pk.Avg"=mean(Prc)), by=Pk]
seas.pk.avg.dt <- prof.2.dt[,list("Seas.Pk.Avg"=mean(Prc)), keyby=list(Mnth, Pk)]
dly.seas.pk.avg.dt <- prof.2.dt[,list("Dly.Seas.Pk.Avg"=mean(Prc)), by=list(Mnth, WkDay, Pk)]
prd.dly.seas.pk.avg.dt <- prof.2.dt[,list("Prd.Dly.Seas.Pk.Avg"=mean(Prc)), by=list(Mnth, WkDay, HE, Pk)]

# Seasonal Calc
seas.pk.ratio.dt <- all.pk.avg.dt[seas.pk.avg.dt, on="Pk"]
seas.pk.ratio.dt[,Ratio:=Seas.Pk.Avg/All.Pk.Avg]
seas.pk.ratio.dt[,Adj:=mean(Ratio),by=Pk]
seas.pk.ratio.dt[,Adj.Ratio:=Ratio/Adj]
seas.pk.ratio.dt[,mean(Adj.Ratio),by=All.Pk.Avg]
dcast(seas.pk.ratio.dt, Mnth ~ Pk, value.var="Adj.Ratio", fun.aggregate=list(mean, sd))

# Weekday Calc
dly.seas.pk.ratio.dt <- dly.seas.pk.avg.dt[seas.pk.avg.dt, on=c("Mnth", "Pk")]
dly.seas.pk.ratio.dt[,Ratio:=Dly.Seas.Pk.Avg/Seas.Pk.Avg]
dly.seas.pk.ratio.dt[,Adj:=mean(Ratio), by=c("Mnth", "Pk")]
dly.seas.pk.ratio.dt[,Adj.Ratio:=Ratio/Adj]
dly.seas.pk.ratio.dt[,mean(Adj.Ratio), by=c("Mnth", "Pk")]
dcast(dly.seas.pk.ratio.dt, Mnth+WkDay ~ Pk, value.var = "Adj.Ratio", fun.aggregate = list(mean, sd))

# Period Calc
prd.dly.seas.pk.ratio.dt <- prd.dly.seas.pk.avg.dt[dly.seas.pk.avg.dt, on=c("Mnth", "WkDay", "Pk")]
prd.dly.seas.pk.ratio.dt[,Ratio:=Prd.Dly.Seas.Pk.Avg/Dly.Seas.Pk.Avg]
dcast(prd.dly.seas.pk.ratio.dt, Mnth+WkDay+Pk ~ HE, value.var = "Ratio", fun.aggregate = list(mean, sd))

# Seasonal * Weekday
seas.pk.ratio.dt[dly.seas.pk.ratio.dt, on=c("Mnth", "Pk")]

# Transco Analysis
library(data.table)
library(magrittr)
library(ggplot2)
library(XLConnect)
setwd("C:/Temp/Lacima")

as.IDate.mdY.1.f <- . %>% as.IDate(., "%m/%d/%Y", tz="America/New_York")

Tr.Prcs.1.dt <- readWorksheetFromFile("20151207 Transco Spot Pricing v1.xls",sheet="Sheet1")
Tr.Prcs.1.dt <- as.data.table(Tr.Prcs.1.dt)

Tr.Prcs.1.dt[,Date.IDt:=as.IDate.mdY.1.f(Date)]
Tr.Prcs.1.dt[,Date:=NULL]

Tr.Prcs.2.dt <- melt(Tr.Prcs.1.dt, id.vars=c("Date.IDt"), variable.name="Loc", variable.factor = F, value.name = "Prc", value.factor = F)

setkey(Tr.Prcs.2.dt, Prc)
Tr.Prcs.3.dt <- Tr.Prcs.2.dt[!J(-999999), on="Prc"]
Tr.Prcs.4.dt <- dcast.data.table(Tr.Prcs.3.dt, Date.IDt ~ Loc, value.var = "Prc")

setkey(Tr.Prcs.3.dt, Loc)


a <- ggplot(Tr.Prcs.3.dt[J(c("Transco_Zn_1", "Transco_Zn_2"))], aes(x=Date.IDt, y=Prc, color=Loc)) + geom_line() + geom_point()
a

b <- ggplot(Tr.Prcs.4.dt, aes(x=Transco_Zn_1, y=Transco_Zn_2)) + geom_line() + geom_point()
b

b.reg <- lm(data=Tr.Prcs.4.dt[,list(Transco_Zn_1, Transco_Zn_2)], Transco_Zn_2 ~ Transco_Zn_1)
b.reg %>% summary


c <- ggplot(Tr.Prcs.4.dt, aes(x=Transco_Zn_3, y=Transco_Zn_2)) + geom_line() + geom_point()
c

c.reg <- lm(data=Tr.Prcs.4.dt[,list(Transco_Zn_3, Transco_Zn_2)], Transco_Zn_2 ~ Transco_Zn_3)
c.reg %>% summary

Tr.Prcs.4.dt[,Mnth:=month(Date.IDt)]
Tr.Prcs.4.dt[,lm(Transco_Zn_2 ~ Transco_Zn_3, .SD) %>% summary.lm %>% `$`("r.squared"), by=Mnth]
Tr.Prcs.4.dt[,lm(Transco_Zn_2 ~ Transco_Zn_1, .SD) %>% summary.lm %>% `$`("r.squared"), by=Mnth]

r.sqrd.1.dt <- Tr.Prcs.4.dt[,list(Zn_1=lm(Transco_Zn_2 ~ Transco_Zn_1, .SD) %>% summary.lm %>% `$`("r.squared"),
                                  Zn_3=lm(Transco_Zn_2 ~ Transco_Zn_3, .SD) %>% summary.lm %>% `$`("r.squared")),
                            by=Mnth]
r.sqrd.2.dt <- melt(r.sqrd.1.dt, id.vars="Mnth", variable.name="Ref_Zone", variable.factor = F, value.name = "R_Sqrd", value.factor = F )
d <- ggplot(r.sqrd.2.dt, aes(x=Mnth, y=R_Sqrd, color=Ref_Zone)) + geom_line() + geom_point()
d


# Oil2 Analysis
library(data.table)
library(magrittr)
library(ggplot2)
library(XLConnect)
setwd("C:/Temp/Lacima")

as.IDate.mdY.1.f <- . %>% as.IDate(., "%m/%d/%Y", tz="America/New_York")

Oil2.Prcs.1.dt <- readWorksheetFromFile("20151207 Oil2 Spot Pricing v1.xls",sheet="Sheet1")
Oil2.Prcs.1.dt <- as.data.table(Oil2.Prcs.1.dt)

Oil2.Prcs.1.dt[,Date.IDt:=as.IDate.mdY.1.f(Date)]
Oil2.Prcs.1.dt[,Date:=NULL]

Oil2.Prcs.2.dt <- melt(Oil2.Prcs.1.dt, id.vars=c("Date.IDt"), variable.name="Loc", variable.factor = F, value.name = "Prc", value.factor = F)

setkey(Oil2.Prcs.2.dt, Prc)
Oil2.Prcs.3.dt <- Oil2.Prcs.2.dt[!J(-999999), on="Prc"]
Oil2.Prcs.3.dt[,`:=`(Yr=year(Date.IDt), Mnth=month(Date.IDt))]

Oil2.Prcs.3.dt[,transpose(list(range(Prc))), by=Loc]
Oil2.Prcs.3.dt[,list(Mean=mean(Prc), SD=sd(Prc)), by=Loc]

Oil2.Prcs.4.dt <- dcast(Oil2.Prcs.3.dt, Date.IDt + Yr + Mnth ~ Loc, value.var = "Prc")


a <- ggplot(Oil2.Prcs.3.dt[!J(c("ULSD")),on="Loc"][J(2015),on="Yr"], aes(x=Prc, color=Loc)) + geom_density()
a

b <- ggplot(Oil2.Prcs.3.dt[!J(c("ULSD")),on="Loc"][J(2010:2015),on="Yr"], aes(Prc, color=Loc)) + stat_ecdf()
b

c <- ggplot(Oil2.Prcs.4.dt, aes(x=ULSD, y=Jet_Kero_Basis, color=as.factor(Yr))) + geom_point()
c


b <- ggplot(Oil2.Prcs.4.dt, aes(sample=Jet_Kero_Basis)) + stat_qq()
b

c <- ggplot(data.table(X=rnorm(n=1000)), aes(sample=X)) + stat_qq()
c

norm.dt <- data.table(X=rnorm(n=1000000))
norm.dt[,sum(ifelse(X<=-1,1,0))/.N]

Oil2.Prcs.5.dt <- copy(Oil2.Prcs.3.dt)
Oil2.Prcs.5.dt[,Ln.Prc:=0]
Oil2.Prcs.5.dt[!J("ULSD"),Ln.Prc:=log(Prc+1), on="Loc"]

a <- ggplot(Oil2.Prcs.5.dt[!J("ULSD"),on="Loc"][J(2015),on="Yr"], aes(x=Ln.Prc, color=Loc)) + geom_density()
a


library(XLConnect)
library(data.table)
library(magrittr)
library(ggplot2)
library(lubridate)
library(fasttime)
library(stringi)

setwd("C:/Temp/Lacima")
ps.prcs.df <- readWorksheetFromFile("20151214 PS Hist Basis v1.xlsx", sheet="Sheet1")
ps.prcs.dt <- data.table(ps.prcs.df)

setnames(ps.prcs.dt, c("Date", 1:24))

ps.prcs.2.dt <- melt(ps.prcs.dt, id.vars="Date", variable.name="HE.Ch", variable.factor = FALSE, value.name = "Prc", value.factor = F)
ps.prcs.2.dt[J(-999999),list(Date,HE.Ch), on="Prc", nomatch=0]
ps.prcs.2.dt[J(-999999),Prc:=NA, on="Prc", nomatch=0]

ps.prcs.2.dt[,Date.IDt:=as.IDate(Date, "%Y-%m-%d", tz="America/New_York")]
ps.prcs.2.dt[,HE:=as.numeric(HE.Ch)]
ps.prcs.2.dt[,Date.Tm.Ch:= stri_paste(Date, HE.Ch, sep=" ")]
ps.prcs.2.dt[,Date.Tm.Pct:=parse_date_time(Date.Tm.Ch, "%Y-%m-%d %H")]

ps.prcs.3.dt <- ps.prcs.2.dt[Date.IDt>= as.IDate("6/1/2010", "%m/%d/%Y", tz="America/New_York") &
                               Date.IDt<= as.IDate("5/31/2015", "%m/%d/%Y", tz="America/New_York")]

ps.prcs.3.dt[,range(Prc)]

ps.prcs.dist.dt <- ps.prcs.3.dt[,list(Prc)]
setorder(ps.prcs.dist.dt, "Prc")
ps.prcs.dist.dt[,Prct:=(1:.N)/.N]
ps.prcs.dist.dt[J(c(0.0025, 0.005, 0.01, 0.05, 0.95, 0.99, 0.995, 0.9975)), Prc, on="Prct", roll=T, by=.EACHI]

a <- ggplot(ps.prcs.3.dt, aes(x=Prc)) + stat_density() + coord_cartesian(xlim=c(-18, 56));a
b <- ggplot(ps.prcs.3.dt, aes(x=log(Prc+31))) + stat_density();b

qrtly.seas.cat.dt <- data.table(Date=c("1/1/2015", "3/15/2015", "6/15/2015", "9/15/2015", "12/15/2015"), 
                                Season=c("Winter", "Spring", "Summer", "Fall", "Winter"))
qrtly.seas.cat.dt[,Date.IDt:=as.IDate(Date, "%m/%d/%Y", tz="America/New_York")]
qrtly.seas.cat.dt[,':='(c("Mnth", "Day"), list(month(Date.IDt), mday(Date.IDt)))]
qrtly.seas.cat.dt[,':='(Date=NULL, Date.IDt=NULL)]

ps.prcs.4.dt <- copy(ps.prcs.3.dt)
ps.prcs.4.dt[,':='(c("Mnth", "Day"), list(month(Date.IDt), mday(Date.IDt)))]

setkey(qrtly.seas.cat.dt, Mnth, Day)
setkey(ps.prcs.4.dt, Mnth, Day)
ps.prcs.5.dt <- qrtly.seas.cat.dt[ps.prcs.4.dt, roll=T]

pk.dt <- data.table(HE=c(1,8,24), Pk=c("Off", "On", "Off"))

setkey(ps.prcs.5.dt, HE)
setkey(pk.dt, HE)
ps.prcs.6.dt <- pk.dt[ps.prcs.5.dt, roll=T]

calc.ln.rtrns.f <- function(sd.dt) {
  
}

setkey(ps.prcs.6.dt, Date.IDt, HE)
ps.prcs.7.dt <- ps.prcs.6.dt[,by=list(Season, Pk)]

