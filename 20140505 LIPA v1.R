# Matthew Pillmeier
# LIPA Analysis


# ----- Packages -----
# Github Development Packages
# For some reason, the downloads from github produce the following SSL Certificate error. To ignore
# this warning, setup `httr` to pass to `RCurl` a flag to ignore verification.
#   Error in function (type, msg, asError = TRUE)  : 
#   SSL certificate problem, verify that the CA cert is OK. Details:
#   error:14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed
library(RCurl)
library(httr)
set_config( config( ssl.verifypeer = 0L ) )   # Ignores the SSL check
library(devtools)

# Data.Table
#install_github("data.table", "Rdatatable", build_vignettes=FALSE)
library(data.table)
#ls("package:data.table")
#lsf.str("package:data.table")
#methods(class="data.table")

# Time
#install.packages("C:/Users/A00113700/Downloads/fasttime_1.0-0.tar.gz", repos = NULL, type = "source")
library(fasttime)   # fastPOSIXct assumes an input format of "%Y/%m/%d %H:%M/%S", no way to change it

# Data Manipulation
library(reshape2)

# Graphics
library(ggplot2)
library(gridExtra)

# String Manipulation
#install_github('Rexamine/stringi')
library(stringi)  # Currently using CRAN version
#library(stringr)

# Excel
library(XLConnect)

# DB
library(RSQLite)
library(RSQLite.extfuns)
#.SQL92Keywords   # SQL Keywords

# Operation Piping
#install_github("smbache/magrittr")
library(magrittr)

# Parallel Computing
library(snow)

# Dplyr
#devtools::install_github("hadley/dplyr")
#library(dplyr)

# Rmetrics
#source("http://www.rmetrics.org/Rmetrics.R")
#install.Rmetrics()

# Rcpp11 (https://github.com/Rcpp11/Rcpp11)
#devtools::install_github("Rcpp11/Rcpp11")
#devtools::install_github("Rcpp11/attributes") # Failed first few times, reloaded devtools, then works, why?
#library(Rcpp11)
#library(attributes)

# Notifications
#library(beepr)
#library(gmailR)
#library(RPushbullet)


# ----- Settings -----
R.version
search()
#.Platform
#detach("package:magrittr")

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

# ---------- LIPA Hybrid Model Investigation ----------
src.dir <- "C://Temp/LIPA"

# ----- Transmission -----
# Import Interface
setwd(stri_join(src.dir,"/Transmission/Interface/NYISO Data"))
csv.files <- list.files(pattern="^\\d{8}ExternalLimitsFlows.csv$")
Read.NYISO.Int.f <- function(csv) {
  library(data.table)
  
  # Read
  csv.dt <- fread(csv)
  
  # Filter
  Mapping.dt <- data.table(`Interface Name`=c("SCH - NPX_1385", "SCH - NPX_CSC", "SCH - PJM_NEPTUNE"),
                           Interface=c("NE-1385", "NE-CSC", "PJM-Neptune"))
  setkey(Mapping.dt, `Interface Name`)
  setkey(csv.dt, `Interface Name`)
  csv.2.dt <- Mapping.dt[csv.dt, nomatch=0]
  csv.2.dt[,":="(`Interface Name`=NULL, `Point ID`=NULL, `Positive Limit (MWH)`=NULL, `Negative Limit (MWH)`=NULL)]
  setnames(csv.2.dt, "Flow (MWH)", "Flow")
  
  return(csv.2.dt)

}
NYISO.Trans.Int.dt <- lapply(csv.files, Read.NYISO.Int.f) %>% rbindlist(use.names=T)
#NYISO.Trans.Int.dt <- parLapply(cluster, csv.files, Read.NYISO.Int.f) %>% rbindlist(use.names=T)   # Faster ...?

# Import PAR Flows
setwd(stri_join(src.dir, "/Transmission/PAR/NYISO Data"))
csv.files <- list.files(pattern="^\\d{8}ParFlows.csv$")
Read.NYISO.ParFlow.f <- function(csv) {
  library(data.table)
  
  # Read
  csv.dt <- fread(csv)
  
  # Filter
  Mapping.dt <- data.table(`Point ID`=c(25678, 25679,25607, 25593),
                           Interface=c(rep("Y49",2), rep("90x",2)))
  setkey(Mapping.dt, `Point ID`)
  setkey(csv.dt, `Point ID`)
  csv.2.dt <- csv.dt[Mapping.dt, nomatch=0]
  csv.2.dt[J(25593), `Flow (MWH)`:=-`Flow (MWH)`]   # Measured as an input to LI
  csv.2.dt[,":="(`Point ID`=NULL)]
  setnames(csv.2.dt, "Flow (MWH)", "Flow")
  
  csv.3.dt <- csv.2.dt[,list(Flow=sum(Flow)), by=list(Timestamp, Interface)]
    
  return(csv.3.dt)
}
NYISO.Trans.ParFlow.dt <- lapply(csv.files, Read.NYISO.ParFlow.f) %>% rbindlist(use.names=T)
#NYISO.Trans.ParFlow.dt <- parLapply(cluster, csv.files, Read.NYISO.ParFlow.f) %>% rbindlist(use.names=T)

NYISO.Trans.dt <- list(NYISO.Trans.Int.dt, NYISO.Trans.ParFlow.dt) %>% rbindlist(use.names=T)

# Format Dates/Times
# Faster w/o 'by'; Benefits of single vector evaluation exceed benefits of smaller data set that you get
# from the 'by' doing the calculation once per unique 'by' value.
NYISO.Trans.dt[,POSIXct:=as.POSIXct(`Timestamp`, "%m/%d/%Y %H:%M", tz='GMT')]
NYISO.Trans.dt[,Date:=as.IDate(POSIXct)]
NYISO.Trans.dt[,Time:=as.ITime(POSIXct)]
NYISO.Trans.dt[,":="(Year=year(Date), Month=month(Date), Day=mday(Date)), by=Date]
NYISO.Trans.dt[,":="(c("Hour", "Min", "Sec"), 
                     stri_match_all_regex(as.character(Time), "^(\\d{2}):(\\d{2}):(\\d{2})$") %>% 
                       extract2(1) %>%    # Equiv. to `[[`(1)
                       extract(2:4) %>%   # Equiv. to `[`(2:4)
                       as.integer %>% 
                       as.list),
               by=Time]
NYISO.Trans.dt[,":="(HE=Hour+1L), by=Hour]
NYISO.Trans.dt[,":="(`Timestamp`=NULL)]

# Fix Duplicate Entries
setkey(NYISO.Trans.dt, Interface, Year, Month, Day, Hour, Min, Sec, Date, Time, POSIXct, HE)
Fix.Dups.f <- function(x) {
  # When there is variability, select the highest absolute valued value
  
  library(magrittr)
  
  x.rng <- range(x)
  if(x.rng[1]==x.rng[2]) {
    x.rng %>% extract(1) %>% return
  } else {
      # Remove 0s and select value with largest Abs Value
      x.diff.0 <- setdiff(x,0)
      x.diff.0 %>% abs %>% order(decreasing=T) %>% extract(1) %>% extract(x.diff.0,.) %>% return
  }
}
NYISO.Trans.fltr.dt <- NYISO.Trans.dt[,.N, keyby=list(Interface, Year, Month, Day, Hour, Min, Sec, Date, Time, POSIXct, HE)][N>1][,N:=NULL]
NYISO.Trans.2a.dt <- NYISO.Trans.dt[NYISO.Trans.fltr.dt,
                                    #parLapply(cluster, .SD, fix.dups), # Slow
                                    lapply(.SD, Fix.Dups.f),            # Faster
                                    by=list(Interface, Year, Month, Day, Hour, Min, Sec, Date, Time, POSIXct, HE), 
                                    .SDcols=c("Flow")]
NYISO.Trans.2b.dt <- NYISO.Trans.dt[!NYISO.Trans.fltr.dt]
NYISO.Trans.2.dt <- list(NYISO.Trans.2a.dt, NYISO.Trans.2b.dt) %>% rbindlist(use.names=T)

# Hourly Aggregation
setkey(NYISO.Trans.2.dt, Interface, Year, Month, Day)
NYISO.Trans.2.All.Days.dt <- NYISO.Trans.2.dt[,list(GRP=.GRP), by=list(Interface, Year, Month, Day)]
All.Hours.0.Min.dt <- expand.grid(GRP=1:NYISO.Trans.2.All.Days.dt[,max(GRP)],
                                  Hour=0:23,
                                  Min=0)
setDT(All.Hours.0.Min.dt)

setkey(All.Hours.0.Min.dt, GRP)
setkey(NYISO.Trans.2.All.Days.dt, GRP)
NYISO.Trans.2.All.Days.Hours.Min.0.dt <- NYISO.Trans.2.All.Days.dt[All.Hours.0.Min.dt]

NYISO.Trans.2.All.Days.Hours.Min.0.dt[,GRP:=NULL]
NYISO.Trans.2.All.Days.Hours.Min.0.dt[,POSIXct:=ISOdate(Year, Month, Day, Hour, Min, 0, tz="GMT"), by=list(Year, Month, Day, Hour, Min)]
NYISO.Trans.2.All.Days.Hours.Min.0.dt[,":="(Year=NULL, Month=NULL, Day=NULL, Hour=NULL, Min=NULL)]

setkey(NYISO.Trans.2.All.Days.Hours.Min.0.dt, Interface, POSIXct)
setkey(NYISO.Trans.2.dt, Interface, POSIXct)
NYISO.Trans.2.All.Days.Hours.Min.0.fill.dt <- NYISO.Trans.2.dt[NYISO.Trans.2.All.Days.Hours.Min.0.dt, roll=Inf]
NYISO.Trans.2.All.Days.Hours.Min.0.fill.dt[,":="(Year=year(POSIXct), Month=month(POSIXct), Day=mday(POSIXct), Hour=hour(POSIXct), Min=0L), by=POSIXct]
NYISO.Trans.2.All.Days.Hours.Min.0.fill.dt[,":="(Sec=NULL, Date=NULL, Time=NULL, POSIXct=NULL, HE=NULL)]

setkey(NYISO.Trans.2.dt, Interface, Min)
interfaces <- NYISO.Trans.2.dt[,unique(Interface)]
NYISO.Trans.2.All.Days.Hours.Min.Else.dt <- NYISO.Trans.2.dt[!CJ(interfaces, 0), .SD, .SDcols=-c("Sec", "Date", "Time", "POSIXct", "HE")]

NYISO.Trans.3.dt <- list(NYISO.Trans.2.All.Days.Hours.Min.0.fill.dt, NYISO.Trans.2.All.Days.Hours.Min.Else.dt) %>% rbindlist(use.names=T)

Agg.Hrly.f <- function(sd.dt) {
  # Weighted Average Function
  Calc.WA.f <- function(x) {
    crossprod(time.delta, x)/sum(time.delta)
  }
  
  setorderv(sd.dt, "Min", order=1)
  
  if(dim(sd.dt)[1]>1) {
    time.st <- sd.dt[,Min]
    time.end <- c(time.st[2:length(time.st)], 60)
    time.delta <- time.end - time.st
  
    ret.dt <- sd.dt[,lapply(.SD, Calc.WA.f), .SDcols=-c("Min")]
    return(ret.dt)
  } else {
    return(sd.dt[,lapply(.SD, as.numeric), .SDcols=-c("Min")])  # Convert to double
  }
}

setkey(NYISO.Trans.3.dt, Interface, Year, Month, Day, Hour)
NYISO.Trans.4.dt <- NYISO.Trans.3.dt[,Agg.Hrly.f(.SD), 
                                     by=list(Interface, Year, Month, Day, Hour), 
                                     .SDcols=c("Min", "Flow")]

# Rename-2
NYISO.Trans.Melt.dt <- melt(NYISO.Trans.4.dt, 
                            measure.vars=c("Flow"), 
                            variable.name="Measurement",
                            value.name="Value",
                            variable.factor=FALSE)
NYISO.Trans.Melt.dt[,Name:=stri_join("Trans.",Measurement, ".", Interface), by=list(Interface, Measurement)]
NYISO.Trans.Melt.dt[,":="(Interface=NULL, Measurement=NULL)]
NYISO.Trans.5.dt <- dcast.data.table(NYISO.Trans.Melt.dt, Year + Month + Day + Hour ~ Name, value.var="Value")
NYISO.Trans.5.dt[,POSIXct:=ISOdate(Year, Month, Day, Hour, 0, 0, tz='GMT'), by=list(Year, Month, Day, Hour)]
NYISO.Trans.5.dt[,Date:=as.IDate(POSIXct)]
NYISO.Trans.5.dt[,HE:=Hour+1L, by=Hour]
NYISO.Trans.5.dt[,":="(Year=NULL, Month=NULL, Day=NULL, Hour=NULL, POSIXct=NULL)]

# ----- Load -----
# Import Committed
setwd(stri_join(src.dir, "/Load/NYISO Data/Committed"))
csv.files <- list.files(pattern="^\\d{8}zonalBidLoad.csv$")
NYISO.CO.Load.dt <- lapply(csv.files, fread) %>% rbindlist(use.names=T)
setnames(NYISO.CO.Load.dt, "Energy Bid Load", "Load")
NYISO.CO.Load.dt[,":="(PTID=NULL, `Time Zone`=NULL, Src="CO")]
setkey(NYISO.CO.Load.dt, Name)
NYISO.CO.Load.dt <- NYISO.CO.Load.dt[J(c("LONGIL", "DUNWOD", "N.Y.C.", "HUD VL"))]

# Import Real-time
setwd(stri_join(src.dir, "/Load/NYISO Data/RT"))
csv.files <- list.files(pattern="^\\d{8}palIntegrated.csv$")
NYISO.RT.Load.dt <- lapply(csv.files, fread) %>% rbindlist(use.names=T)
setnames(NYISO.RT.Load.dt, "Integrated Load", "Load")
NYISO.RT.Load.dt[,":="(PTID=NULL, `Time Zone`=NULL, Src="RT")]
setkey(NYISO.RT.Load.dt, Name)
NYISO.RT.Load.dt <- NYISO.RT.Load.dt[J(c("LONGIL", "DUNWOD", "N.Y.C.", "HUD VL"))]

# Import Forecasted
setwd(stri_join(src.dir, "/Load/NYISO Data/Forecast"))
csv.files <- list.files(pattern="^\\d{8}isolf.csv$")
Read.NYISO.Forecast.f <- function(csv) {
  library(stringi)
  library(magrittr)
  library(data.table)
  
  # Use file name to determine the forecast to keep
  csv.dt <- fread(csv)
  csv.split.l <- stri_match_all_regex(csv, "^(\\d{4})(\\d{2})(\\d{2})([^.]*).csv$")
  csv.idate <- csv.split.l %>% unlist %>% extract(2:4) %>% stri_join(collapse="-") %>% as.IDate
  csv.dt[,Date:=as.IDate(`Time Stamp`, "%m/%d/%Y"), by=`Time Stamp`]
  csv.dt[,AsOf:=csv.idate]
  csv.dt <- csv.dt[Date==AsOf]
  csv.dt[,":="(Date=NULL, AsOf=NULL, NYISO=NULL)]
  
  return(csv.dt)
}
NYISO.FC.Load.dt <- lapply(csv.files, Read.NYISO.Forecast.f) %>% rbindlist(use.names=T)
NYISO.FC.Load.dt <- melt(NYISO.FC.Load.dt, id.vars=c('Time Stamp'), variable.name="Name", value.name="Load", variable.factor=F)
NYISO.FC.Load.dt[,":="(`Time Stamp`=stri_join(`Time Stamp`, ":00"), Name=toupper(Name), Src="FC")]
setkey(NYISO.FC.Load.dt, Name)
NYISO.FC.Load.dt <- NYISO.FC.Load.dt[J(c("LONGIL", "DUNWOD", "N.Y.C.", "HUD VL"))]

NYISO.Load.dt <- list(NYISO.CO.Load.dt, NYISO.RT.Load.dt, NYISO.FC.Load.dt) %>% rbindlist(use.names=T)

# Rename
Mapping.dt <- data.table(Name=c("WEST", "GENESE", "CENTRL", "NORTH", "MHK VL", "CAPITL", "HUD VL", "MILLWD", "DUNWOD", "N.Y.C.", "LONGIL"),
                         Zone=stri_join("NY-", LETTERS[1:11]))
setkey(Mapping.dt, Name)
setkey(NYISO.Load.dt, Name)
NYISO.Load.dt <- Mapping.dt[NYISO.Load.dt]
NYISO.Load.dt[,Name:=NULL]

# Format Dates/Times
NYISO.Load.dt[,POSIXct:=as.POSIXct(`Time Stamp`, "%m/%d/%Y %H:%M:%S", tz='GMT')]
NYISO.Load.dt[,':='(Date=as.IDate(POSIXct), Time=as.ITime(POSIXct))]
NYISO.Load.dt[,HE:=hour(Time)+1, by=Time]
NYISO.Load.dt[,':='(`Time Stamp`=NULL, POSIXct=NULL, Time=NULL)]

# Average Duplicate Fall DST Values
Load.Src <- NYISO.Load.dt[,unique(Src)]
Load.Zone <- NYISO.Load.dt[,unique(Zone)]
Load.Date <- NYISO.Load.dt[,unique(Date)]
Load.HE <- NYISO.Load.dt[,unique(HE)]
setkey(NYISO.Load.dt, Src, Zone, Date, HE)

NYISO.Load.2.dt <- NYISO.Load.dt[CJ(Load.Src, Load.Zone, Load.Date, Load.HE), 
                                list(Load=mean(Load)), 
                                by=list(Src, Zone, Date, HE), 
                                nomatch=0]

# Cast
NYISO.Load.2.dt[,Zone:= stri_join("Load", Src, Zone, sep="."), by=list(Src, Zone)]
NYISO.Load.2.dt[,Src:=NULL]
NYISO.Load.Cast.dt <- dcast.data.table(NYISO.Load.2.dt,
                                       Date + HE ~ Zone,
                                       value.var="Load")


# ----- RT/DA LMPs -----
# Import
setwd(stri_join(src.dir, "/Prices"))
LMP.dt <- fread("LMP.csv")

setnames(LMP.dt, "Hr", "HE")
setnames(LMP.dt, "($/MWh)", "LMP")
LMP.dt[,":="(MARKET_CDE=NULL, PRICE_TYPE=NULL)]

setkey(LMP.dt, SOURCE_TYPE)
LMP.dt[J("Day Ahead"), Src:="DA"]
LMP.dt[J("Real Time"), Src:="RT"]
LMP.dt[,SOURCE_TYPE:=NULL]

# Remove Empty Rows
setkey(LMP.dt, LMP)
LMP.dt <- LMP.dt[!is.na(LMP)]

# Rename Locations
Mapping.dt <- data.table(LOCATION=c(".Z.CONNECTICUT - LOAD ZONE", "HUD VL", "N.Y.C.", "LONGIL", "JCPL~ZONE", "DUNWOD"),
                         Zone=c("NE-CT", "NY-G", "NY-J", "NY-K", "PJM-JCPL", "NY-I"))
setkey(Mapping.dt, LOCATION)
setkey(LMP.dt, LOCATION)
LMP.dt <- Mapping.dt[LMP.dt]
LMP.dt[,LOCATION:=NULL]

# Format Dates/Times
LMP.dt[,Date:=as.IDate(DATE, "%m/%d/%Y")]
LMP.dt[,DATE:=NULL]

# Average Fall DST Values
setkey(LMP.dt, HE)
LMP.dt[J(25), HE:=2]

LMP.Src <- LMP.dt[,unique(Src)]
LMP.Zone <- LMP.dt[,unique(Zone)]
LMP.Date <- LMP.dt[,unique(Date)]
LMP.HE <- LMP.dt[,unique(HE)]
setkey(LMP.dt, Src, Zone, Date, HE)

LMP.2.dt <- LMP.dt[CJ(LMP.Src, LMP.Zone, LMP.Date, LMP.HE), 
                   list(LMP=mean(LMP)), 
                   by=list(Src, Zone, Date, HE), 
                   nomatch=0]

# Cast
LMP.2.dt[,Zone:= stri_join("LMP", Src, Zone, sep="."), by=list(Src,Zone)]
LMP.2.dt[,Src:=NULL]
LMP.Cast.dt <- dcast.data.table(LMP.2.dt,
                                Date + HE ~ Zone,
                                value.var="LMP")

# ----- GDA -----
# Import
setwd(stri_join(src.dir, "/Prices"))
GDA.dt <- fread("GDA.csv")

setnames(GDA.dt, "PRICE ($/MMBTU)", "Price")
setnames(GDA.dt, "COMPONENT", "Zone")
GDA.dt[,":="(COMMODITY=NULL, MARKET=NULL)]

# Remove Empty Rows
setkey(GDA.dt, Price)
GDA.dt <- GDA.dt[!is.na(Price)]

# Format Dates
GDA.dt[,Date:=as.IDate(PRICING_DATE, "%m/%d/%Y")]
GDA.dt[,PRICING_DATE:=NULL]

# Cast
GDA.dt[,Zone:=stri_join("GDA","DA", Zone, sep="."), by=Zone]
GDA.Cast.dt <- dcast.data.table(GDA.dt,
                                Date ~ Zone,
                                value.var="Price")


# ----- US Marketscan Oil -----
# Import
setwd(stri_join(src.dir, "/Prices"))
USM.dt <- fread("Platts Oil.csv")

# Format Dates
USM.dt[,Date2:=as.IDate(Date, "%m/%d/%Y")]
USM.dt[,Date:=NULL]
setnames(USM.dt, "Date2", "Date")

# Melt
USM.2.dt <- melt(USM.dt,
                 id.vars="Date",  
                 variable.name="Type",
                 value.name="Price",
                 variable.factor=F)

# Remove Dates with NA Prices
setkey(USM.2.dt, Price)
USM.2.dt <- USM.2.dt[!is.na(Price)]

# Cast
USM.2.dt[,Type:=stri_join("Oil", "DA", Type, sep="."), by=Type]
USM.Cast.dt <- dcast.data.table(USM.2.dt,
                                Date ~ Type,
                                value.var="Price")

# ----- Create Merged Hourly/Daily Datasets -----
setkey(NYISO.Load.Cast.dt, Date, HE)
setkey(LMP.Cast.dt, Date, HE)
Load.LMP.dt <- merge(NYISO.Load.Cast.dt, LMP.Cast.dt, all=T, nomatch=NA)

setkey(Load.LMP.dt, Date)
setkey(GDA.Cast.dt, Date)
Load.LMP.GDA.dt <- merge(Load.LMP.dt, GDA.Cast.dt, all=T, nomatch=NA)

setkey(Load.LMP.GDA.dt, Date)
setkey(USM.Cast.dt, Date)
Load.LMP.GDA.USM.dt <- merge(Load.LMP.GDA.dt, USM.Cast.dt, all=T, nomatch=NA)

setkey(Load.LMP.GDA.USM.dt, Date, HE)
setkey(NYISO.Trans.5.dt, Date, HE)

# Hourly
All.Hrly.dt <- merge(Load.LMP.GDA.USM.dt, NYISO.Trans.5.dt, all=T, nomatch=NA)
All.Hrly.dt <- All.Hrly.dt[!is.na(HE)]

# Holidays
setwd(src.dir)
holidays.dt <- fread("R Holidays.csv")
holidays.dt[,Date:=as.IDate(DT_C, "%m/%d/%Y")]
holidays.dt[,DT_C:=NULL]

setkey(All.Hrly.dt, Date)
setkey(holidays.dt, Date)
All.Hrly.dt[,Holiday:=0L]
All.Hrly.dt[holidays.dt, Holiday:=1L]

# Pk Periods
setwd(src.dir)
pkprds.dt <- fread("R PkPrds.csv")
All.Hrly.dt[,":="(REGION="PJM", WDAY=(wday(Date)+5L)%%7L+1L)] # Move from a Sun=1 to a Mon=1 numbering scheme
setkey(All.Hrly.dt, Holiday)
All.Hrly.dt[J(1), WDAY:=8L]
setkey(pkprds.dt, REGION, WDAY, HE)
setkey(All.Hrly.dt, REGION, WDAY, HE)
All.Hrly.dt <- pkprds.dt[All.Hrly.dt, nomatch=NA]
All.Hrly.dt[,":="(REGION=NULL, Holiday=NULL)]

# Daily
All.Hrly.Melt.dt <- melt(All.Hrly.dt, 
                         id.vars=c("Date", "WDAY", "HE", "PKPRD_4","PKPRD_3", "PKPRD_2"), 
                         variable.name="Type", 
                         value.name="Value",
                         variable.factor=F)
All.Hrly.Melt.dt[,Src:=stri_match_all_regex(Type, "^([^\\.]+)\\.(.*)")[[1]][2], by=Type]

Hrly.to.Dly.f <- function(Src, x) {
  if(Src[1]=="Load" | Src[1]=="Trans") {
    return(sum(x, na.rm=T))
  } else {
    return(mean(x, na.rm=T))
  }
}

# PkPrd 1 - RTC
All.Dly.PkPrd.1.Melt.dt <- All.Hrly.Melt.dt[,list(Value=Hrly.to.Dly.f(Src, Value)), by=list(Type, Date)]
All.Dly.PkPrd.1.Melt.dt[,":="(PkPrdType=1, PkPrdValue="RTC")]

# PkPrd 2 - Pk, OffPk
All.Dly.PkPrd.2.Melt.dt <- All.Hrly.Melt.dt[,.SD,.SDcols=-c("PKPRD_3", "PKPRD_4")][,list(Value=Hrly.to.Dly.f(Src, Value)), by=list(Type, Date, PKPRD_2)]
All.Dly.PkPrd.2.Melt.dt[,PkPrdType:=2]
setnames(All.Dly.PkPrd.2.Melt.dt, "PKPRD_2", "PkPrdValue")

# PkPrd 3 - Wkday Pk, Wkend Pk, All OffPk
All.Dly.PkPrd.3.Melt.dt <- All.Hrly.Melt.dt[,.SD,.SDcols=-c("PKPRD_2", "PKPRD_4")][,list(Value=Hrly.to.Dly.f(Src, Value)), by=list(Type, Date, PKPRD_3)]
All.Dly.PkPrd.3.Melt.dt[,PkPrdType:=3]
setnames(All.Dly.PkPrd.3.Melt.dt, "PKPRD_3", "PkPrdValue")

# PkPrd 4 - Wkday Pk, Wkend Pk, Wkday OffPk, Wkend OffPk
All.Dly.PkPrd.4.Melt.dt <- All.Hrly.Melt.dt[,.SD,.SDcols=-c("PKPRD_2", "PKPRD_3")][,list(Value=Hrly.to.Dly.f(Src, Value)), by=list(Type, Date, PKPRD_4)]
All.Dly.PkPrd.4.Melt.dt[,PkPrdType:=4]
setnames(All.Dly.PkPrd.4.Melt.dt, "PKPRD_4", "PkPrdValue")

All.Dly.PkPrd.Melt.dt <- rbindlist(list(All.Dly.PkPrd.1.Melt.dt, All.Dly.PkPrd.2.Melt.dt, 
                                        All.Dly.PkPrd.3.Melt.dt, All.Dly.PkPrd.4.Melt.dt), 
                                   use.names=T)

All.Dly.dt <- dcast.data.table(All.Dly.PkPrd.Melt.dt,
                               Date + PkPrdType + PkPrdValue~ Type,
                               value.var="Value")

# ----- Assign Useful Date/Time Values -----
# F: Assign Monthly Indicator Variables v1
# This method simply returns a list of the values to assign and therefore requires the assignment explicitly
# provide the names to assign to.
Mnth.Indicator.1.f <- function(Month) {
  ind.values <- rep(0,12)
  ind.values[Month] <- 1
  return(as.list(ind.values[1:11]))
}

# F: Assign Monthly Indicator Variables v2
# This method is more general as it allows the function to define both the names and values to be used in
# the assignment. However, it requires the function be called twice.
# As a note, if you want to use assign the result of formulas, then the 2nd list entry is a list of 
# eval(parse(text=...)) statements.
Mnth.Indicator.2.f <- function(BY.l) {
  mnth.names <- 1:11 %>% ISOdate(2014, ., 1, 0, tz='GMT') %>% strftime("%b")
  ind.values <- rep(0,12)
  ind.values[BY.l$Month] <- 1
  
  return(list(A=mnth.names, B=as.list(ind.values[1:11])))
}

HE.Indicator.2.f <- function(BY.l) {
  he.names <- 1:23 %>% stri_join("HE",.)
  ind.values <- rep(0,24)
  ind.values[BY.l$HE] <- 1
  
  return(list(A=he.names, B=as.list(ind.values[1:23])))
}

# Hourly 
All.Hrly.dt[,":="(Year=year(Date), Month=month(Date), Day=mday(Date))]
All.Hrly.dt[,DateTime:=ISOdate(Year, Month, Day, HE-1, tz='GMT')]
#All.Hrly.dt[,c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"):=Mnth.Indicator.1.f(Month), by=Month]
#All.Hrly.dt[,(1:11 %>% ISOdate(2014, ., 1, 0, tz='GMT') %>% strftime("%b")):=Mnth.Indicator.1.f(Month), by=Month]
All.Hrly.dt[,":="(Mnth.Indicator.2.f(.BY)[[1]], Mnth.Indicator.2.f(.BY)[[2]]), by=Month]
All.Hrly.dt[,":="(HE.Indicator.2.f(.BY)[[1]], HE.Indicator.2.f(.BY)[[2]]), by=HE]

All.Hrly.dt[,":="(`Sprd.RT.NY-K.NE-CT`=`LMP.RT.NY-K`-`LMP.RT.NE-CT`,
                  `Sprd.RT.NY-K.NY-J`=`LMP.RT.NY-K`-`LMP.RT.NY-J`,
                  `Sprd.RT.NY-K.PJM-JCPL`=`LMP.RT.NY-K`-`LMP.RT.PJM-JCPL`,
                  `Sprd.RT.NY-K.NY-I`=`LMP.RT.NY-K`-`LMP.RT.NY-I`)]

All.Hrly.dt[,":="(`Sprd.DA.NY-K.NE-CT`=`LMP.DA.NY-K`-`LMP.DA.NE-CT`,
                  `Sprd.DA.NY-K.NY-J`=`LMP.DA.NY-K`-`LMP.DA.NY-J`,
                  `Sprd.DA.NY-K.PJM-JCPL`=`LMP.DA.NY-K`-`LMP.DA.PJM-JCPL`,
                  `Sprd.DA.NY-K.NY-I`=`LMP.DA.NY-K`-`LMP.DA.NY-I`)]

#All.Hrly.dt[,":="(`Gen.RT`=`Load.RT.NY-K` - `Trans.Flow.PJM-Neptune` - `Trans.Flow.NE-CSC` -
#                           `Trans.Flow.NE-1385` - `Trans.Flow.90x` - 2*`Trans.Flow.Y49`)]

# Daily
All.Dly.dt[,":="(Year=year(Date), Month=month(Date), Day=mday(Date))]
All.Dly.dt[,DateTime:=ISOdate(Year, Month, Day, 0, tz='GMT')]
All.Dly.dt[,":="(Mnth.Indicator.2.f(.BY)[[1]], Mnth.Indicator.2.f(.BY)[[2]]), by=Month]

All.Dly.dt[,":="(`Sprd.RT.NY-K.NE-CT`=`LMP.RT.NY-K`-`LMP.RT.NE-CT`,
                 `Sprd.RT.NY-K.NY-J`=`LMP.RT.NY-K`-`LMP.RT.NY-J`,
                 `Sprd.RT.NY-K.PJM-JCPL`=`LMP.RT.NY-K`-`LMP.RT.PJM-JCPL`,
                 `Sprd.RT.NY-K.NY-I`=`LMP.RT.NY-K`-`LMP.RT.NY-I`)]

All.Dly.dt[,":="(`Sprd.DA.NY-K.NE-CT`=`LMP.DA.NY-K`-`LMP.DA.NE-CT`,
                 `Sprd.DA.NY-K.NY-J`=`LMP.DA.NY-K`-`LMP.DA.NY-J`,
                 `Sprd.DA.NY-K.PJM-JCPL`=`LMP.DA.NY-K`-`LMP.DA.PJM-JCPL`,
                 `Sprd.DA.NY-K.NY-I`=`LMP.DA.NY-K`-`LMP.DA.NY-I`)]

#All.Dly.dt[,":="(`Gen.RT`=`Load.RT.NY-K` - `Trans.Flow.PJM-Neptune` - `Trans.Flow.NE-CSC` -
#                          `Trans.Flow.NE-1385` - `Trans.Flow.90x` - `Trans.Flow.Y49`)]

# ----- Export Structures to SQLite -----
Adj.Names.For.SQLite.Exp.f <- function(x.dt) {
  library(data.table)
  library(magrittr)
  
  x.dt %>% names %>% 
    stri_replace_all_fixed(".", "ABC") %>% 
    stri_replace_all_fixed("-", "DEF") %>% 
    stri_replace_all_fixed(" ", "GHI") %>% 
    stri_replace_all_fixed("#", "JKL") %>% 
    setnames(x.dt, old=names(x.dt), new=.)
}

setwd(src.dir)
sqlite <- dbDriver("SQLite")
LIPA.db <- dbConnect(sqlite, "LIPA DB.sqlite")

dbListTables(LIPA.db)

#dbRemoveTable(LIPA.db, "Hrly")
#dbRemoveTable(LIPA.db, "Dly")

All.Hrly.dt[,":="(Date.str=strftime(Date, "%Y-%m-%d"),
                  DateTime.str=strftime(DateTime, "%Y-%m-%d %H:%M:%S"))]
Adj.Names.For.SQLite.Exp.f(All.Hrly.dt)
dbWriteTable(LIPA.db, "Hrly", All.Hrly.dt[,.SD, .SDcols=-c("Date", "DateTime")])

All.Dly.dt[,":="(Date.str=strftime(Date, "%Y-%m-%d"),
                 DateTime.str=strftime(DateTime, "%Y-%m-%d %H:%M:%S"))]
Adj.Names.For.SQLite.Exp.f(All.Dly.dt)
dbWriteTable(LIPA.db, "Dly", All.Dly.dt[,.SD, .SDcols=-c("Date", "DateTime")])

dbListTables(LIPA.db)

dbGetQuery(LIPA.db, "select count(*) from Hrly")
res <- dbSendQuery(LIPA.db, "select count(*) from Hrly")
res <- dbSendQuery(LIPA.db, "select * from Dly where Dly.Year=2008")
res.dt <- fetch(res, n=1000)
setDT(res.dt)
res.dt %>% dim
dbClearResult(res)

dbDisconnect(LIPA.db)


# ----- Import Structures from SQLite -----
Adj.Names.For.SQLite.Imp.f <- function(x.dt) {
  library(data.table)
  library(magrittr)
  
  x.dt %>% names %>%
    stri_replace_all_fixed("ABC", ".") %>% 
    stri_replace_all_fixed("DEF", "-") %>% 
    stri_replace_all_fixed("GHI", " ") %>%
    stri_replace_all_fixed("JKL", "#") %>%
    setnames(x.dt, old=names(x.dt), new=.)
}

setwd(src.dir)
sqlite <- dbDriver("SQLite")
LIPA.db <- dbConnect(sqlite, "LIPA DB.sqlite")

dbListTables(LIPA.db)

All.Hrly.dt <- dbReadTable(LIPA.db, "Hrly")
setDT(All.Hrly.dt)
Adj.Names.For.SQLite.Imp.f(All.Hrly.dt)
All.Hrly.dt[,DateTime:=fastPOSIXct(DateTime.str, tz='GMT')]
All.Hrly.dt[,Date:=as.IDate(fastPOSIXct(Date.str, tz='GMT'), tz='GMT')]
All.Hrly.dt[,":="(Date.str=NULL, DateTime.str=NULL, stringsAsFactors=NULL)]

All.Dly.dt <- dbReadTable(LIPA.db, "Dly")
setDT(All.Dly.dt)
Adj.Names.For.SQLite.Imp.f(All.Dly.dt)
All.Dly.dt[,DateTime:=fastPOSIXct(DateTime.str, tz='GMT')]
All.Dly.dt[,Date:=as.IDate(fastPOSIXct(Date.str, tz='GMT'), tz='GMT')]
All.Dly.dt[,":="(Date.str=NULL, DateTime.str=NULL, stringsAsFactors=NULL)]

dbDisconnect(LIPA.db)


# ----- Analysis -----
# Price Analysis: Monthly Zn-K vs Zn-J
setkey(All.Dly.dt, PkPrdType, PkPrdValue)
t.dt <- All.Dly.dt[J(2, "OnPk"), lapply(.SD, mean, na.rm=T), .SDcols=c("LMP.DA.NY-K", "LMP.DA.NY-J"), by=list(Year, Month, PkPrdValue)]
t.dt[,Sprd:=`LMP.DA.NY-K` - `LMP.DA.NY-J`]
t.melt.dt <- melt(t.dt, id.vars=c("Year", "Month", "PkPrdValue"), variable.name="Hub", value.name="LMP")
t.melt.dt[,Date:=ISOdate(Year, Month, 1, 0, 0, 0, tz='GMT')]
p1 <- ggplot(t.melt.dt, aes(x=Date, y=LMP, color=factor(Hub))) + geom_point() + geom_line(); p1

lm.t <- All.Dly.dt[J(2, "OnPk")] %>%
  lm(`LMP.DA.NY-K` ~ `LMP.DA.NY-J` + `LMP.DA.PJM-JCPL` + `LMP.DA.NY-G` + `Load.FC.NY-K` + `Load.CO.NY-K` +
       Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov +
       `GDA.DA.TRANS6` + `GDA.DA.IRQZN2` + 
       `Oil.DA.NYH ULSD Barge` + `Oil.DA.NYH #6 0.3 HP Cargo`,
     data=.)
summary(lm.t)



# Load vs Price: Hrly
setkey(All.Hrly.dt, PKPRD_2, Year, Month)
All.Hrly.dt[J("OnPk")] %>% ggplot(aes(y=`LMP.RT.NY-K`, x=`Load.RT.NY-K`)) + geom_density2d() + facet_wrap(~Month)


# Transmission: Hrly Flows
setkey(All.Hrly.dt, PKPRD_2, Year)
All.Hrly.dt %>% ggplot(aes(x=Date, y=`Trans.NY.Flow.PJM-Neptune`)) + geom_point() + facet_wrap(~PKPRD_2)
All.Hrly.dt %>% ggplot(aes(x=Date, y=`Trans.NY.Flow.NE-CSC`)) + geom_point() + facet_wrap(~PKPRD_2)
All.Hrly.dt %>% ggplot(aes(x=Date, y=`Trans.NY.Flow.NE-1385`)) + geom_point() + facet_wrap(~PKPRD_2)
All.Hrly.dt %>% ggplot(aes(x=Date, y=`Trans.NY.Flow.90x`)) + geom_point() + facet_wrap(~PKPRD_2)
All.Hrly.dt %>% ggplot(aes(x=Date, y=`Trans.NY.Flow.Y49`)) + geom_point() + facet_wrap(~PKPRD_2)

# Transmission: Hrly NY-K/NY-I Spread vs Y49 Flow
All.Hrly.dt %>% ggplot(aes(x=`Sprd.RT.NY-K.NY-I`, y=`Trans.NY.Flow.Y49`)) + geom_point()

# Transmission: Hrly Flow Distribution
setkey(All.Hrly.dt, PKPRD_2, Year, Month)
All.Hrly.dt[J("OnPk", 2011)] %>% ggplot(aes(x=`Trans.NY.Flow.Y49`)) + geom_density()
All.Hrly.dt[J("OnPk")] %>% ggplot(aes(x=`Trans.NY.Flow.PJM-Neptune`)) + geom_density()
All.Hrly.dt[J("OnPk")] %>% ggplot(aes(x=`Trans.NY.Flow.NE-CSC`)) + geom_density()
All.Hrly.dt[J("OnPk")] %>% ggplot(aes(x=`Trans.NY.Flow.NE-1385`)) + geom_density()
All.Hrly.dt[J("OnPk")] %>% ggplot(aes(x=`Trans.NY.Flow.90x`)) + geom_density()

# Transmission: NE-CSC TS per 3x PkPrd
setkey(All.Dly.dt, PkPrdType)
All.Dly.dt[J(3), list(Date, PkPrdValue, `Trans.NY.Flow.90x`)] %>% 
  ggplot(aes(x=Date, y=`Trans.NY.Flow.90x`)) + geom_point() + facet_wrap(~PkPrdValue)

# NY-K/I vs Prices
All.Hrly.dt %>% ggplot(aes(x=`Load.RT.NY-I`, y=`Load.RT.NY-K`)) + geom_point() + geom_abline(intercept=0, slope=4, color="red")
All.Hrly

All.Hrly.dt[,":="(`Load.RT.NY-K/I`=`Load.RT.NY-K`/`Load.RT.NY-I`)]
All.Hrly.dt[,":="(`LMP.RT.NY-K/I`=`LMP.RT.NY-K`/`LMP.RT.NY-I`)]
All.Hrly.dt %>% ggplot(aes(x=`LMP.RT.NY-K/I`, y=`Load.RT.NY-K/I`)) + geom_point()

All.Hrly.dt[,":="(`Load.RT/FC.NY-K`=`Load.RT.NY-K`/`Load.FC.NY-K`)]
All.Hrly.dt[,":="(`LMP.RT/DA.NY-K`=`LMP.RT.NY-K`/`LMP.DA.NY-K`)]
All.Hrly.dt %>% ggplot(aes(y=`LMP.RT/DA.NY-K`, x=`Load.RT/FC.NY-K`)) + geom_density2d()

lm.t <- All.Hrly.dt[J("OnPk", 2014)] %>%
  lm(`LMP.RT.NY-K` ~ `LMP.RT.NY-I`, data=.)
summary(lm.t)

lm.t <- All.Hrly.dt %>%
  lm(`LMP.RT.NY-K` ~ `LMP.RT.NY-I` + `Load-Flow.RT`,
     data=.)
summary(lm.t)

lm.t <- All.Hrly.dt %>%
  lm(`LMP.RT.NY-K` ~ `LMP.RT.NY-I` + `Load-Flow.RT` + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov +
                        HE1 + HE2 + HE3 + HE4 + HE5 + HE6 + HE7 + HE8 + HE9 + HE10 + HE11 + HE12 +
                        HE13 + HE14 + HE15 + HE16 + HE17 + HE18 + HE19 + HE20 + HE21 + HE22 + HE23,
    data=.)
summary(lm.t)


All.Hrly.dt[CJ("OnPk", c(2008, 2009:2014)), 
            lapply(.SD, function(x) {quantile(x, probs=c(0.01, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 0.99), na.rm = T)}), 
            .SDcols=c("Trans.NY.Flow.PJM-Neptune", "Trans.NY.Flow.NE-CSC")]

setkey(All.Hrly.dt, Year, Month)
All.Hrly.dt[J(2013, 1)] %>% ggplot(aes(x=Date, y=`Sprd.DA.NY-K.NY-J`)) + geom_point()



All.Hrly.dt[J("OnPk")] %>%
  ggplot(aes(y=`Trans.NY.Flow.PJM-Neptune`, x=`Sprd.DA.NY-K.PJM-JCPL`)) + geom_point(alpha=0.2)

All.Hrly.dt %>%
  ggplot(aes(x=Date, y=`Load-Flow.RT`)) + geom_point()

All.Hrly.dt[CJ("OnPk",c(2008, 2010:2014) )] %>%
  ggplot(aes(x=`Trans.NY.Flow.PJM-Neptune`, color=as.factor(Year))) + geom_density()

p1 <- ggplot(All.Dly.dt[J(3, "5x16")], aes(x=Date, y=`Trans.NY.Flow.PJM-Neptune`)) + geom_point(); p1
p1 <- ggplot(All.Dly.dt[J(3, "5x16")], aes(x=Date, y=`Trans.NY.Flow.NE-CSC`)) + geom_point(); p1
p1 <- ggplot(All.Dly.dt[J(3, "5x16")], aes(x=Date, y=`Trans.NY.Flow.NE-1385`)) + geom_point(); p1

p1 <- ggplot(All.Dly.dt[J(3, "5x16")], aes(y=`Trans.NY.Flow.PJM-Neptune`, x=`Sprd.DA.NY-K.PJM-JCPL`)) + geom_point() + coord_cartesian(xlim=c(-350,0));p1

All.Hrly.dt %>% setkey(Year)
t.1 <- All.Hrly.dt[J(2008:2014)] %>%
         lm(`LMP.RT.NY-K` ~ `Sprd.RT.NY-K.PJM-JCPL`*`Trans.NY.Flow.PJM-Neptune` +
                            `Sprd.RT.NY-K.NE-CT`*`Trans.NY.Flow.NE-CSC` + 
                            `Sprd.RT.NY-K.NE-CT`:`Trans.NY.Flow.NE-1385` +
                            `Load.RT.NY-K` +
                            `GDA.DA.TRANS6` +
                            `Oil.DA.NYH ULSD Barge` + 
                            Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, .)
summary(t.1)
t.1 %>% extract2("residuals") %>% length
t.1.res <- t.1 %>% extract("residuals") %>% as.data.table %>% setnames("residuals", "Res")
p1 <- t.1.res %>%
        ggplot(aes(x=Res)) %>% 
        + geom_density() %>%
        + coord_cartesian(xlim=c(-50,100))
p1

t.1.res[,quantile(Res, probs=c(0.01, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 0.99))]

# -- Zone-K vs Zone-J --
setkey(All.Hrly.dt, PKPRD_3)

# Percent of time Zone-K is importing from Zone-J based on LMP
All.Hrly.dt[J("5x16"),list(DA=sum(`LMP.DA.NY-K`>=`LMP.DA.NY-J`)/.N, RT=sum(`LMP.RT.NY-K`>=`LMP.RT.NY-J`)/.N), by=list(Year, PKPRD_3)]
t.1.dt <- All.Hrly.dt[J("7x8"),list(DA=sum(`LMP.DA.NY-K`>=`LMP.DA.NY-J`)/.N, RT=sum(`LMP.RT.NY-K`>=`LMP.RT.NY-J`)/.N), by=list(Year, Month, PKPRD_3)]
t.2.dt <- melt(t.1.dt, measure.vars=c("DA", "RT"), variable.name="DA-RT", value.name="Value", variable.factor=F)
t.2.dt[,Date:=as.IDate(stri_join(Year,Month,1,sep="-"))]

p.1 <- ggplot(t.2.dt, aes(x=Date, y=Value, color=factor(`DA-RT`))) + geom_point() + geom_line() + geom_smooth()
p.1

# Average spread conditional on import
t.1.dt <- All.Hrly.dt[J("5x16"),
            list(DA=mean((`LMP.DA.NY-K`>=`LMP.DA.NY-K`)*(`LMP.DA.NY-K`-`LMP.DA.NY-G`)), 
                 RT=mean((`LMP.RT.NY-K`>=`LMP.RT.NY-K`)*(`LMP.RT.NY-K`-`LMP.RT.NY-G`))),
            by=list(Year, Month, PKPRD_3)]
t.2.dt <- melt(t.1.dt, measure.vars=c("DA", "RT"), variable.name="DA-RT", value.name="Value", variable.factor=F)
t.2.dt[,Date:=as.IDate(stri_join(Year,Month,1,sep="-"))]

p.1 <- ggplot(t.2.dt, aes(x=Date, y=Value, color=factor(`DA-RT`))) + geom_point() + geom_line() 
p.1 <- p.1 + geom_smooth(se=F) + coord_cartesian(ylim=c(0,25))
p.1

setkey(All.Dly.dt, PkPrdType, PkPrdValue)
p.1 <- ggplot(All.Dly.dt[J(3, "5x16")], aes(x=`LMP.DA.NY-J`, y=`LMP.DA.NY-K`, color=factor(Year)))
p.1 <- p.1 + geom_point()
p.1 <- p.1 + geom_abline(intercept=0, slope=1)
p.1

setkey(All.Dly.dt, PkPrdType, PkPrdValue, Year)
lm.1 <- All.Dly.dt[CJ(3, "5x16", 2008:2014)] %>% 
          lm(`LMP.DA.NY-K` ~ `LMP.DA.NY-J` + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + 
               `Load.FC.NY-K` + `GDA.DA.TRANS6` + `Oil.DA.NYH ULSD Barge`,.)
summary(lm.1)
AIC(lm.1)


p1 <- ggplot(All.Dly.dt, aes(x=DateTime)) + 
      geom_point(aes(y=`Load.RT.NY-K`), color='red', alpha=0.5) +
      geom_point(aes(y=`Load.FC.NY-K`), color='blue', alpha=0.5)
p1

p1 <- ggplot(All.Hrly.dt, aes(x=`Load.FC.NY-K`, y=`Load.RT.NY-K`, color=factor(Month))) 
p1 <- p1 + geom_point(alpha=0.1) 
p1 <- p1 + geom_abline(aes(intercept=0, slope=1))
p1 <- p1 + facet_wrap(~Year)
p1

p1 <- ggplot(All.Hrly.dt, aes(x=`LMP.DA.NY-K`, y=`LMP.RT.NY-K`, color=factor(Month))) 
p1 <- p1 + geom_point(alpha=0.5) 
p1 <- p1 + geom_abline(aes(intercept=0, slope=1))
p1 <- p1 + facet_wrap(~Year)
p1 <- p1 + xlim(c(0, 100)) + ylim(c(0,100))
p1

p1 <- ggplot(All.Hrly.dt, aes(x=`Load.RT.NY-K`, y=`LMP.RT.NY-K`, color=factor(Year)))
p1 <- p1 + geom_point(alpha=0.1) + ylim(c(0,500))
p1

p1 <- ggplot(All.Hrly.dt, aes(x=`Load.RT.NY-K`, y=`LMP.RT.NY-K`, color=factor(Year)))
p1 <- p1 + geom_point(alpha=1) + ylim(c(0,500))
p1 <- p1 + facet_wrap(~Month)
p1

p1 <- ggplot(All.Dly.dt, aes(x=`GDA.TRANS6`, y=`LMPDA.NY-K`, color=factor(Year))) + geom_point(alpha=0.5)
p1 <- p1 + facet_wrap(~Month) + ylim(c(0, 150)) + xlim(c(0, 10))
p1

setkey(All.Dly.dt, Year)
t.1 <- All.Dly.dt %>% 
       lm(`LMP.DA.NY-K` ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + 
            `LMP.DA.NY-J`  + `LMP.DA.NE-CT` + `Load.RT.NY-K` + `GDA.DA.TRANS6` +
            `LMP.DA.NY-G` + `Load.RT.NY-K` : `GDA.DA.TRANS6`, .)
summary(t.1)
AIC(t.1)

t.1 <- All.Hrly.dt %>%
  lm(`LMP.DA.NY-K` ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + 
      `LMP.DA.NY-J`,.)
summary(t.1)
AIC(t.1)

t.1 <- All.Dly.dt %>%
       lm(`LMP.DA.NY-K` ~ Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + 
          `LMP.DA.NY-J`,.)
summary(t.1)
AIC(t.1)

t.1 <- All.Dly.dt %>%
  lm(`LMPDA.NY-K` ~ `LoadRT.NY-K` : `GDA.TRANS6`, .)
summary(t.1)
AIC(t.1)



p1 <- ggplot(All.Dly.dt[J(1:2)], aes(x=`GDA.TRANS6`, y=`DALMP.NY-K`)) + geom_point(alpha=0.5) + xlim(c(0,25))
p1

setkey(All.Dly.dt, Year)
p1 <- ggplot(All.Dly.dt[J(2013:2014)], aes(x=Date, y=`Load.NY-K`)) + geom_point() + geom_smooth()
p1

All.Hrly.dt[,DateTime:=ISOdate(year = year(Date), month = month(Date), day = mday(Date), hour = HE-1, tz='GMT')]
All.Hrly.dt[,Year:=year(Date)]
setkey(All.Hrly.dt, Year)
p1 <- ggplot(All.Hrly.dt[J(2011:2014)], aes(x=DateTime, y=`Load.NY-K`)) + geom_point(alpha=0.5)
p1

# ---------- Test ----------
new.cntr.f <- function() {
  i <- 0
  
  cntr.f <- function(...) {
    i <<- i + 1
    i
  }
  
  cntr.f
}


setkey(All.Hrly.dt, Year, PKPRD_2)
All.Hrly.dt[J(2010:2012, "OnPk"), list(GmL=`GDA.DA.TRANS6` * `Load.FC.NY-K`, LMP=`LMP.DA.NY-K`)] %>%
  ggplot(aes(x=GmL, y=LMP)) + geom_point()


All.Hrly.dt %>% setkey(Year, Month)
All.Hrly.dt[!J(2014,7), list(`90x Inefficient Flow %`=sum((`Sprd.RT.NY-K.NY-J`>0 & `Trans.NY.Flow.90x` <0) | (`Sprd.RT.NY-K.NY-J`<0 & `Trans.NY.Flow.90x` >0), na.rm=T)/.N), by=list(Date=ISOdate(Year, Month, 1, 0, tz='GMT'))]  %>% 
  ggplot(aes(x=Date, y=`90x Inefficient Flow %`)) + 
  geom_point() + geom_line() + 
  coord_cartesian(ylim=c(0,1)) + 
  ggtitle("90x RT Inefficient Flow %")


# ----- Saturn -----
setwd(stri_join(src.dir, "/Saturn"))
Strn.Sim.dt <- fread("20140813 LIPA 50 Draw ZnK Prices v1.csv", sep = ",", header = T)
Strn.Sim.dt %>% str
Strn.Sim.dt <- Strn.Sim.dt[,.SD, .SDcols=-c(9)]
Strn.Sim.dt[,":="(`Date Str`=NULL, Date=NULL)]
Strn.Sim.m.dt <- melt(Strn.Sim.dt,
                      id.vars=c("Draw", "Year", "Month", "Day", "Wkday", "PkType"), 
                      variable.name="HE",
                      value.name="LMP",
                      variable.factor=F)
Strn.Sim.m.dt[,HE:=as.numeric(HE)]
Strn.Sim.m.dt[,DateTime:=ISOdate(Year, Month, Day, HE-1, tz='GMT')]

Strn.Sim.m.dt %>% setkey(PkType, Month)

# Plot each PkType-Month density 
plot.f <- function(BY.l, SD.dt) {
  p1 <- ggplot(SD.dt, aes(x=LMP)) + geom_density() + ggtitle(stri_join(BY.l[[1]], BY.l[[2]], sep=" - "))
  print(p1)
  TRUE
}
Strn.Sim.m.dt[CJ(c("OnPk", "OffPk"), 1:12), plot.f(.BY, .SD), by=.EACHI]

# Shapes
# Saturn
Strn.Sim.m.dt[,Mean:=mean(LMP), by=list(Draw, Year, Month, Day, PkType)]
Strn.Sim.m.dt[,Shape:=LMP/Mean]
Strn.Sim.Shp.dt <- Strn.Sim.m.dt[,list(Shape2=median(Shape, na.rm=T)), by=list(Month,HE,PkType)]
Strn.Sim.Shp.dt %>%
  ggplot(aes(x=HE,  y=Shape2, color=as.factor(PkType))) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~Month) + 
  ggtitle("Saturn Daily Shapes (Draws: 1-10)")

# Hist DA
All.Hrly.dt %>% setkey(Year)
LIPA.DA.dt <- All.Hrly.dt[,list(Year, Month, Day, HE, PKPRD_4, `LMP.DA.NY-K`)]
LIPA.DA.dt %>% setkey(PKPRD_4)
LIPA.DA.dt[J(c("5x16", "5x8")), PkType:="OnPk"]
LIPA.DA.dt[J(c("2x16", "2x8")), PkType:="OffPk"]
LIPA.DA.dt[,PKPRD_4:=NULL]
LIPA.DA.dt[,Mean:=mean(`LMP.DA.NY-K`), by=list(Year, Month, Day, PkType)]
LIPA.DA.dt[,Shape:=`LMP.DA.NY-K`/Mean]
LIPA.DA.Shp.dt <- LIPA.DA.dt[,list(Shape2=median(Shape, na.rm=T)), by=list(Month, HE, PkType)]
LIPA.DA.Shp.dt %>% setkey(PkType)
LIPA.DA.Shp.dt %>%
  ggplot(aes(x=HE,  y=Shape2, color=as.factor(PkType))) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~Month) +
  ggtitle("Historical DA Shapes")

# Hist RT
All.Hrly.dt %>% setkey(Year)
LIPA.RT.dt <- All.Hrly.dt[,list(Year, Month, Day, HE, PKPRD_4, `LMP.RT.NY-K`)]
LIPA.RT.dt %>% setkey(PKPRD_4)
LIPA.RT.dt[J(c("5x16", "5x8")), PkType:="OnPk"]
LIPA.RT.dt[J(c("2x16", "2x8")), PkType:="OffPk"]
LIPA.RT.dt[,PKPRD_4:=NULL]
LIPA.RT.dt[,Mean:=mean(`LMP.RT.NY-K`), by=list(Year, Month, Day, PkType)]
LIPA.RT.dt[,Shape:=`LMP.RT.NY-K`/Mean]
LIPA.RT.Shp.dt <- LIPA.RT.dt[,list(Shape2=median(Shape, na.rm=T)), by=list(Month, HE, PkType)]
LIPA.RT.Shp.dt %>% setkey(PkType)
LIPA.RT.Shp.dt %>%
  ggplot(aes(x=HE,  y=Shape2, color=as.factor(PkType))) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~Month) +
  ggtitle("Historical RT Shapes")


Strn.Sim.m.dt %>% setkey(Year)
Strn.Sim.m.dt[J(2015)] %>%
  ggplot(aes(x=DateTime, y=LMP)) + geom_point(alpha=0.05) + coord_cartesian(ylim=c(0,300))
Strn.Sim.m.dt[J(2015)] %>%
  ggplot(aes(x=LMP)) + geom_density() + coord_cartesian(xlim=c(0,300)) + 
  ggtitle("Saturn 2015 LMP PDF")


Strn.Sim.m.dt %>%
  ggplot(aes(x=DateTime, y=LMP)) + geom_point()

All.Hrly.dt %>% setkey(Year)
All.Hrly.dt[J(2013)] %>%
  ggplot(aes(x=DateTime, y=`LMP.DA.NY-K`)) + geom_point(alpha=0.5) + 
  coord_cartesian(ylim=c(0,300))

All.Hrly.dt[J(2013)] %>%
  ggplot(aes(x=`LMP.DA.NY-K`)) + geom_density() + coord_cartesian(xlim=c(0,300)) + 
  ggtitle("Historical 2013 LMP PDF")

All.Hrly.dt[!is.na(`LMP.DA.NY-K`)][J(2013)] %>%
  ggvis(~`LMP.DA.NY-K`) %>% layer_densities()

All.Hrly.dt[J(2013)] %>%
  ggvis() %>% compute_density(~`LMP.DA.NY-K`,na.rm=T)



# Maintenance Schedule Processing
src.dir <- "C://Temp/LIPA"
setwd(stri_join(src.dir, "/Gen"))

maint.dt <- fread("Maintenance Schedule.csv")
maint.flt.dt <- melt(maint.dt, id.vars=c("YEAR", "UNIT"), variable.name="Type", value.name="Date.str",value.factor = F, variable.factor = F)
setkey(maint.flt.dt, Date.str)
maint.flt.dt[!J(""), Date.t:= as.IDate(Date.str, "%m/%d/%Y")]
maint.flt.dt <- maint.flt.dt[!is.na(Date.t)]
maint.flt.dt[,Date.str:=NULL]
maint.flt.dt[,Date:=as.IDate(ISOdate(YEAR, month(Date.t), mday(Date.t), hour=0, tz='GMT'))]
maint.flt.dt[,Date.t:=NULL]
maint.flt.dt[,`:=`(c("Type.1", "Type.2"), as.list(stri_split_regex(Type, "-")[[1]])), by=Type]
maint.flt.dt[,Type:=NULL]
maint.2.dt <- dcast.data.table(maint.flt.dt, UNIT + YEAR + Type.2 ~ Type.1, value.var = "Date")
maint.2.dt[,':='(YEAR=NULL, Type.2=NULL)]
setcolorder(maint.2.dt, c(1,3,2))
maint.2.dt[,PS:=stri_join("                       ", strftime(OUT, format="[%YM%mD%d]  ", tz='GMT'), IN-OUT, " -1", " -")]
setkey(maint.2.dt, UNIT, OUT)
write.csv(maint.2.dt, "PS Maint Days.csv")



