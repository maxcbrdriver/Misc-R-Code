.libPaths("C:/R/Libraries/RRO_3.2.2")

library(data.table)
library(magrittr)
library(RODBC)
library(stringi)

source("C:/R/Secure v1.R")


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

as.IDate.1.f <- . %>% as.IDate(., "%m/%d/%Y", tz='GMT')
as.IDate.2.f <- . %>% as.IDate(., "%m-%d-%Y", tz='GMT')
as.IDate.3.f <- . %>% as.IDate(., "%Y-%m-%d", tz='GMT')

as.IDate.f <- function(dates.v, fast=T) {
  require(stringi)
  require(data.table)
  
  dates.dt <- date.table(Date=dates.v)
  
  if(fast==T) {
    # Make Simplifying Assumptions
    # 1) All dates are of the same format, therefore inspect 1st date only
    dates.dt[,ISNA:=is.na(Date)]
    if(dates.dt[,.N-sum(ISNA)]==0) 
      return NA
    } else {
      
    }
    
  } else {
    # 
  }
  
  
  
}


lac.odbc <- odbcConnect("Lacima_Prod", uid = ps.login, pwd=ps.pwd)

lac.tbls <- sqlTables(lac.odbc)
lac.tbls.dt <- as.data.table(lac.tbls)
lac.tbls.dt %>% setkey("TABLE_SCHEM")
lac.tbls.dt[J("dbo"),.N]

lac.tbls.dt %>% setkey("TABLE_TYPE")
lac.tbls.dt[J("TABLE"),.N]
lac.tbls.dt[J("TABLE"), . %>% , by="TABLE_NAME"]



lac.ear.out <- sqlQuery(lac.odbc, "select * from raEarningsAtRiskOutput") %>% as.data.table
lac.ear.out %>% setkey("EarValue")
lac.ear.out[!J(NA)] %>% head(10)
lac.ear.out %>% str

lac.rf.out.dt <- sqlQuery(lac.odbc, "select * from raRiskFactor") %>% as.data.table
lac.rft.out.dt <- sqlQuery(lac.odbc, "select * from raRiskFactorType") %>% as.data.table
lac.rset.out.dt <- sqlQuery(lac.odbc, "select * from raSetting") %>% as.data.table
lac.rsetv.out.dt <- sqlQuery(lac.odbc, "select * from raSettingValue") %>% as.data.table
lac.rparam.out.dt <- sqlQuery(lac.odbc, "select * from raParameter") %>% as.data.table  # Parameter estimates
lac.rprmset.out.dt <- sqlQuery(lac.odbc, "select * from raParameterConfiguration") %>% as.data.table  # Parameter Setting

# ----- SQL Queries -----

library(lubridate)
library(data.table)
library(stringi)
library(magrittr)
library(RODBC)
library(RODBCext)

options(data.table.nomatch=0)
options(stringsAsFactors=F)

# ZEMA files have tz="US/Pacific", but we want to convert to EST
Sys.setenv(TZ='America/New_York')

as.IDate.mdY.1.f <- . %>% as.IDate(., "%m/%d/%Y", tz="America/New_York")
as.IDate.mdY.2.f <- . %>% as.IDate(., "%m-%d-%Y", tz="America/New_York")

as.IDate.Ymd.1.f <- . %>% as.IDate(., "%Y/%m/%d", tz="America/New_York")
as.IDate.Ymd.2.f <- . %>% as.IDate(., "%Y-%m-%d", tz="America/New_York")

source("C:/R/Secure v1.R")

lac.odbc <- odbcConnect("Lacima_Prod", uid = ps.login, pwd = ps.pwd)


# Get Lacima Prices - Spot
# Function: get.lac.prc.sp.f
# Parameters:
#   
get.lac.prc.sp.f <- function(lac.ch, RF.c, dateSt.x, dateEnd.x) {
  require(stringi)
  require(magrittr)
  require(RODBC)
  require(RODBCext)
  require(data.table)

  # -- Process Inputs
  # Risk Factors
  if(!is.character(RF.c)) return(data.table(NULL))
   
  # Start/End Dates
  date.to.c.f <- function(date.x) {
    if("IDate" %chin% class(date.x)) {
      date.c <- strftime(date.x, "%m/%d/%Y")
    } else {
      date.c <- date.x
    }
  }
  
  date.x.l <- list(DateSt=dateSt.x, DateEnd=dateEnd.x)
  date.c.l <- lapply(date.l, date.to.c.f)
  
  
  # Generate SQL Query
  select.c <- stri_paste("SELECT ",
                         "RF.RiskFactorName As RF, ",
                         "HistData.HistoricalDate As QDate, ",
                         "HistData.Period As HE, ",
                         "HistData.DataValue As Price ",
                         sep="")
  
  from.c <- stri_paste("FROM ",
                       "[lacima].[dbo].[raHistoricalData] HistData ",
                       sep=" ")
  
  
  inner.join.c <- stri_paste("INNER JOIN [lacima].[dbo].[raRiskFactor] RF ",
                              "ON HistData.raRiskFactorID = RF.raRiskFactorID ",
                             sep=" ")

  where.c <- stri_paste("WHERE ", 
                        "RF.RiskFactorName In ('", stri_paste(RF.c, collapse="', '"), "') And ",
                        "HistData.HistoricalDate >= '", date.c.l$DateSt, "' And ",
                        "HistData.HistoricalDate < '", date.c.l$DateEnd,"'",
                        sep="")
    
  order.by.c <- stri_paste("ORDER BY ",
                           "RF.RiskFactorName, ",
                           "HistData.HistoricalDate, ",
                           "HistData.Period ", 
                           sep= "")
  
  sql.c <- stri_paste(select.c, from.c, inner.join.c, where.c, order.by.c, sep="")
  
  # Query the DB
  sql.rslt <- sqlQuery(lac.odbc, sql.c)
  
  sql.rslt
  
}

my.sql <- get.lac.prc.sp.f(lac.odbc, c("Pwr_DA_MF__L__PJM_West", "Gas_GD_MF__L_GDD_Hub_HH"), as.IDate.mdY.1.f("12/1/2015"), as.IDate.mdY.1.f("1/1/2016"))
rslts <- sqlQuery(lac.odbc, my.sql)
rslts.dt <- as.data.table(rslts)
rslts.dt %>% str



lac.rvRF.out.dt <- sqlQuery(lac.odbc, "select * from rvRiskFactor") %>% as.data.table

find.tbl.cols.f <- function(lac.tbl, regx) {
  sql.cols.dt <- sqlColumns(lac.odbc, lac.tbl) %>% as.data.table
  sql.cols.dt[,Match:=stri_match_first_regex(COLUMN_NAME, regx, opts_regex= stri_opts_regex(case_insensitive = T)) %>% is.na %>% not]
  sql.cols.dt %>% setkey(Match)
  sql.cols.dt[J(T), unique(TABLE_NAME)]
}

rslt <- sapply(lac.tbls.dt[J("TABLE"), TABLE_NAME], function(x) find.tbl.cols.f(x, "type"))
rslt[!is.na(rslt)]



# Simulated Prices
library(data.table)
library(magrittr)
library(stringi)
library(ggplot2)

as.IDate.1.f <- . %>% as.IDate(., "%m/%d/%Y", tz='GMT')

setwd("C:/Temp/Lacima/Tests/20150929")

# Forward Simulation File Processing
Gas.F.dt <- fread("Gas_GD_MF__L_GDD_Algon_CG_Forward_Simulation_2015-09-25T162625.csv", header=T)
Gas.F.2.dt <- melt(Gas.F.dt, id.vars = c("Simulation", "Date", "Period"), variable.name = "SimPeriod", value.name = "SimValue", variable.factor = F, value.factor = F)

Gas.F.2.dt[,Date.IDt:=as.IDate.1.f(Date), by=Date]
Gas.F.2.dt[,':='(Date.Yr=year(Date.IDt), Date.Mon=month(Date.IDt)), by=Date.IDt]
Gas.F.2.dt[,Date.FOM.IDt:=as.IDate(paste(Date.Yr[1], Date.Mon[1], 1, sep="-"), "%Y-%m-%d", tz='GMT'), by=Date.IDt]
Gas.F.2.dt %>% setkey(SimPeriod)
Gas.F.2.dt[!J("Spot"), SimPeriodDate:=transpose(stri_extract_all_regex(SimPeriod, "^([^/]*)")), by=SimPeriod]
Gas.F.2.dt[!J("Spot"), SimPeriodDate.IDt:=as.IDate(SimPeriodDate, "%d-%b-%y", tz='GMT'), by=SimPeriodDate]
Gas.F.2.dt[J("Spot"), ':='(SimPeriodDate="Spot", SimPeriodDate.IDt=NA)]

# Spot Statistics
Gas.F.2.dt %>% setkey(SimPeriod, Date.FOM.IDt)
a.dt <- Gas.F.2.dt[J("Spot"), list(Mean=mean(SimValue),SD=sd(SimValue), Med=median(SimValue), N=.N), by=Date.FOM.IDt]
a.dt %>% View
a <- a.dt %>% ggplot(aes(x=Date.FOM.IDt, y=Mean)) + geom_line() + geom_point(); a
b <- a.dt %>% ggplot(aes(x=Date.FOM.IDt, y=SD)) + geom_line() + geom_point(); b


# Spot Distribution Graphs
Gas.F.2.dt %>% setkey(SimPeriod, Date.Mon)
a <- ggplot(Gas.F.2.dt[J("Spot")], aes(x=SimValue)) + geom_density() + facet_wrap(~Date.Mon) + coord_cartesian(xlim=c(0,20)); a
Gas.F.2.dt[J("Spot"), list(mean(SimValue), sum(SimValue<=5)/.N, .N), by=Date.Mon]


# 1st Simulation Forward Statistics 
Gas.F.2.dt %>% setkey(Date.IDt)
Gas.F.2.dt[J(as.IDate.1.f("9/24/2015")), SimValue, by=list(Simulation, SimPeriod)][,list(Mean=mean(SimValue), SD=sd(SimValue)), by=SimPeriod]


# Terminal Forward Statistics
Gas.F.2.dt %>% setkey(SimPeriod, SimPeriodDate.IDt, Date.IDt)
a.dt <- Gas.F.2.dt[!J("Spot")] %>%
  '['(Date.IDt==(SimPeriodDate.IDt-1), list(TermFrwd=SimValue), by=list(SimPeriodDate.IDt, Simulation)) %>%
  '['(,list(Mean=mean(TermFrwd),
            SD=sd(TermFrwd), 
            P5=sort(TermFrwd)[ceiling(0.05*.N)], 
            P50=sort(TermFrwd)[ceiling(0.50*.N)],
            P95=sort(TermFrwd)[ceiling(0.95*.N)]),
      keyby=SimPeriodDate.IDt)

a.dt %>% View
a <- a.dt %>% ggplot(aes(x=SimPeriodDate.IDt, y=Mean)) + geom_line() + geom_point(); a
b <- a.dt %>% ggplot(aes(x=SimPeriodDate.IDt, y=SD)) + geom_line() + geom_point(); b


# Spot Simulation File Processing
Gas.S.dt <- fread("Gas_GD_Sum_1_M_IF_Algon_CG_Spot_Simulation_2015-09-25T162648.csv", header=F, skip = 1L, colClasses = c('numeric', 'character', 'numeric', 'numeric'))
Gas.S.dt %>% setnames(c("Simulation", "Date", "Period", "SimValue"))

Gas.S.dt[,Date.IDt:=as.IDate.1.f(Date), by=Date]
Gas.S.dt[,':='(Date.Yr=year(Date.IDt), Date.Mon=month(Date.IDt)), by=Date.IDt]
Gas.S.dt[,Date.FOM.IDt:=as.IDate(paste(Date.Yr[1], Date.Mon[1], 1, sep="-"), "%Y-%m-%d", tz='GMT'), by=Date.IDt]

# Spot Simulation Statistics
x.dt <- Gas.S.dt[, list(Mean=mean(SimValue),SD=sd(SimValue), Med=median(SimValue), N=.N), by=Date.FOM.IDt]
x.dt %>% View
a <- x.dt %>% ggplot(aes(x=Date.FOM.IDt, y=Mean)) + geom_line() + geom_point(); a
b <- x.dt %>% ggplot(aes(x=Date.FOM.IDt, y=SD)) + geom_line() + geom_point(); b


# MVR Test
setwd("C:/Temp/Lacima/Tests/20151001/")

Zn.K.dt <- fread("Pwr_DA_MVR____NY_Zn_K_Spot_Simulation_2015-10-01T152321.csv", header=F, skip=1L)
Zn.J.dt <- fread("Pwr_DA_MF__L__NY_Zn_J_Spot_Simulation_2015-10-01T152317.csv", header=F, skip=1L)
JCPL.dt <- fread("Pwr_DA_Sum_1_MS__PJM_JCPL_Spot_Simulation_2015-10-01T152316.csv", header=F, skip=1L)

Zn.X.ls <- list(Zn_J=Zn.J.dt, Zn_K=Zn.K.dt, JCPL=JCPL.dt)
Zn.X.dt <- rbindlist(Zn.X.ls, idcol = T)
setnames(Zn.X.dt, c("Zone", "Sim", "Date", "HE", "Price"))
Zn.X.dt[, Date.IDt:=as.IDate.1.f(Date), by=Date]
Zn.X.dt[,Date:=NULL]

Zn.X.cast.dt <- dcast(Zn.X.dt, Sim + Date.IDt + HE ~ Zone, value.var = "Price")

Zn.X.cast.dt[,':='('ln_Zn_J'=log(Zn_J), 'ln_Zn_K'=log(Zn_K), 'ln_JCPL'=log(JCPL))]
Zn.X.cast.dt[,lm(Zn_K ~ Zn_J + JCPL) %>% summary]

# PFE Testing
setwd("D:/Lacima/Users/MRP/Lacima/Tests/20151105 PFE v1/")

sim.spot.dt <- fread("Pwr_DA_SF_A_L__PJM_West_Spot_Simulation_2015-11-16T163510.csv", header=F, skip = 1)
sim.spot.dt %>% setnames(c("Sim", "Date", "HE", "Prc"))
sim.spot.dt[,Date.IDt:=as.IDate.1.f(Date), by=Date]
#sim.spot.dt[,DateTime:=stri_paste(strftime(Date.IDt, "%Y%m%d"), stri_pad(HE, width=2, pad=0)) %>% as.numeric]
setorder(sim.spot.dt, Date.IDt, HE)
sim.spot.dt[,Idx:=1:.N, by=Sim]
spot.p <- ggplot(sim.spot.dt[,list(SD=sd(Prc)), by=Idx], aes(x=Idx, y=SD)) + geom_point() + geom_line()
spot.p

spot.p.2 <- ggplot(sim.spot.dt[,list(SD=sd(Prc)), by=Date.IDt], aes(x=Date.IDt, y=SD)) + geom_point() + geom_line()
spot.p.2

sim.spot.dt[,range(Date.IDt)]

sim.ctrct.dt <- fread("9_30_2015 - MRP_PFE_Test_1 - sim data.csv", header=T)
sim.ctrct.dt[,lapply(.SD, mean), by=Horizon, .SDcols=c("Mark-to-Market", "Payoffs", "Unpaid Settlements")]
sim.ctrct.dt[,lapply(.SD, sd), by=Horizon, .SDcols=c("Mark-to-Market", "Payoffs", "Unpaid Settlements")]
sim.ctrct.dt[,':='(Ek=`Mark-to-Market`+Payoffs + `Unpaid Settlements`)]
sim.ctrct.dt %>% setkey(Horizon)
sim.ctrct.dt[J("11/30/2016"), `Mark-to-Market`, by=Simulation]



sim.frwd.dt <- fread("Pwr_DA_MF__L__PJM_West_Forward_Simulation_2015-11-16T163507.csv", header=T)
names(sim.frwd.dt)
sim.frwd.dt %>% head
setkey(sim.frwd.dt, Date, Simulation, Period)
sim.frwd.dt[J("9/30/2015", 1,1)]
sim.frwd.dt[J("10/1/2015"),sd(`01-Nov-15/1 to 30-Nov-15/24`), by=list(Date, Period)]
sim.frwd.dt[,uniqueN(Date)] * 24 * 10
sim.frwd.dt[,.N]
