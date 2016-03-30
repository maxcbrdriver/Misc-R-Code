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

library(data.table)
library(lubridate)
library(stringi)
library(magrittr)
library(RODBC)
library(RODBCext)
library(ggplot2)
library(moments)

options(data.table.nomatch=0)
options(stringsAsFactors=F)

# 'GMT' avoids DST complications
Sys.setenv(tz='GMT')

as.IDate.mdY.1.f <- . %>% as.IDate(., "%m/%d/%Y")
as.IDate.mdY.2.f <- . %>% as.IDate(., "%m-%d-%Y")

as.IDate.Ymd.1.f <- . %>% as.IDate(., "%Y/%m/%d")
as.IDate.Ymd.2.f <- . %>% as.IDate(., "%Y-%m-%d")

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
  require(lubridate)

  # -- Process Inputs
  # Risk Factors
  if(!is.character(RF.c)) return(data.table(NULL))
   
  # Start/End Dates
  date.to.c.f <- function(date.x) {
    if("IDate" %chin% class(date.x)) {
      date.c <- strftime(date.x, "%m/%d/%Y", tz="America/New_York")
    } else {
      date.c <- date.x
    }
  }
  
  date.x.l <- list(DateSt=dateSt.x, DateEnd=dateEnd.x)
  date.c.l <- lapply(date.x.l, date.to.c.f)
  
  # Generate SQL Query
  select.c <- stri_paste("SELECT ",
                         "RF.RiskFactorName As RF, ",
                         "HistData.HistoricalDate As QDate, ",
                         "HistData.Period As HE, ",
                         "Hldy.Description As Holiday, ",
                         "HistData.DataValue As Price ",
                         sep="")
  
  from.c <- stri_paste("FROM ",
                       "[lacima].[dbo].[raHistoricalData] HistData ",
                       sep=" ")
  
  
  inner.join.c <- stri_paste("INNER JOIN [lacima].[dbo].[raRiskFactor] RF ",
                              "ON HistData.raRiskFactorID = RF.raRiskFactorID ",
                             sep=" ")
  
  left.outer.join.c <- stri_paste("LEFT OUTER JOIN [lacima].[dbo].[raHoliday] Hldy ",
                                    "ON HistData.raRiskFactorID = Hldy.raRiskFactorId And ",
                                       "HistData.HistoricalDate = Hldy.HolidayDate ",
                                  sep="")

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
  
  sql.c <- stri_paste(select.c, from.c, inner.join.c, left.outer.join.c, where.c, order.by.c, sep="")
  
  # Query the DB
  sql.rslt.dt <- sqlQuery(lac.odbc, sql.c) %>% as.data.table
  setnames(sql.rslt.dt, "QDate", "QDate.PCDt")
  
  # Add Peak Indicators
  sql.rslt.dt[,WDay:=(wday(QDate.PCDt)+5)%%7+1] # {M-F} = 1:5; {Sat, Sun, Hldy} = 6:8
  sql.rslt.dt[!is.na(Holiday), WDay:=8]
  sql.rslt.dt[,Pk:=0L]
  sql.rslt.dt[CJ(WDay=c(1:5), HE=c(8:23)),Pk:=1L, on=c("WDay", "HE")]
  
  # Add Date/Time
  sql.rslt.dt[,QDate.Hr.PCDt:=QDate.PCDt+HE*60^2]
  
  # Add 'Quarterly Seasons Peak 2' indicators
  qrtly.seas.pk.2.dt <- data.table(Season=c("Winter", "Spring", "Summer", "Fall", "Winter"),
                                   QDate.2k.c=c("1/1/2000", "3/15/2000", "6/15/2000", "9/15/2000", "12/15/2000"))
  qrtly.seas.pk.2.dt[,QDate.2k.IDt:=as.IDate.mdY.1.f(QDate.2k.c)]
  qrtly.seas.pk.2.dt[,QDate.2k.c:=NULL]
  
  sql.rslt.dt[,QDate.2k.IDt:=stri_paste(month(QDate.PCDt), mday(QDate.PCDt), 2000, sep="/") %>% as.IDate.mdY.1.f]
  
  setkey(sql.rslt.dt, QDate.2k.IDt)
  setkey(qrtly.seas.pk.2.dt, QDate.2k.IDt)
  sql.rslt.2.dt <- qrtly.seas.pk.2.dt[sql.rslt.dt, roll=T]
  sql.rslt.2.dt[,QDate.2k.IDt:=NULL]
  
  # Return
  setcolorder(sql.rslt.2.dt, neworder = c("RF", "QDate.PCDt", "QDate.Hr.PCDt", "WDay", "HE", "Pk", "Holiday", "Season", "Price"))
  sql.rslt.2.dt
}

# -- PS Basis Analysis --
# Get Spot Prices
lac.prc.sp.dt <- get.lac.prc.sp.f(lac.odbc, c("Pwr_DA_SF_A_N_Basis_PJM_PSEG"), as.IDate.mdY.1.f("1/1/2010"), as.IDate.mdY.1.f("12/31/2015"))
lac.prc.sp.dt %>% str

# Price range and percentiles
lac.prc.sp.dt[,range(Price)]
perc.v <- c(0,0.25, 0.5,1,2,5,95,98,99,99.5, 99.75, 100)
setkey(lac.prc.sp.dt, RF, QDate.Hr.PCDt)
lac.prc.sp.dt[,lapply(perc.v, function(x) sort(Price)[floor(x/100*.N) %>% max(1)] %>% min(.N)) %>% 
                  set_names(stri_paste(perc.v, "%", sep="")), by=list(RF, Season, Pk)]

# Test Dates
lac.prc.sp.dt[lac.prc.sp.dt[,list(N=2:.N, Diff=QDate.Hr.PCDt[2:.N]>QDate.Hr.PCDt[1:(.N-1)])][J(FALSE),on="Diff"][,N]]
lac.prc.sp.dt[,identical(order(QDate.Hr.PCDt), order(QDate.PCDt, HE))]

# Price Density Graphs
a <- ggplot(lac.prc.sp.dt, aes(x=Price)) + stat_ecdf() + coord_cartesian(xlim=c(50, 500), ylim=c(0.97,1)); a
a <- ggplot(lac.prc.sp.dt, aes(x=Price)) + facet_wrap(~ Season + Pk, scales="free") + stat_ecdf(); a
a <- ggplot(lac.prc.sp.dt, aes(x=Price)) + facet_wrap(~ Season + Pk, scales="free") + geom_density(); a

# Log Returns
setorder(lac.prc.sp.dt, RF, QDate.Hr.PCDt)
lac.prc.adj <- lac.prc.sp.dt[,sort(Price)[floor(0.005*.N)]]
lac.prc.sp.dt[,Ln.Rtrn:=c(0, log((Price[2:.N]+lac.prc.adj) / (Price[1:(.N-1)]+lac.prc.adj)))]
lac.prc.sp.2.dt <- lac.prc.sp.dt[2:.N][!is.nan(Ln.Rtrn)]

perc.v <- c(0,0.25, 0.5,1,2,5,95,98,99,99.5, 99.75, 100)
setkey(lac.prc.sp.2.dt, RF, Ln.Rtrn)
lac.prc.sp.2.dt[,lapply(perc.v, function(x) sort(Ln.Rtrn)[floor(x/100*.N) %>% max(1)] %>% min(.N)) %>% 
                  set_names(stri_paste(perc.v, "%", sep="")), by=list(RF, Season, Pk)]


# Ln Return Graphs
b <- ggplot(lac.prc.sp.2.dt, aes(x=Price, y=Ln.Rtrn)) + geom_line() + geom_point(); b
b <- ggplot(lac.prc.sp.2.dt, aes(x=Ln.Rtrn)) + geom_density() + coord_cartesian(xlim=c(-1,1)); b
b <- ggplot(lac.prc.sp.2.dt, aes(x=Ln.Rtrn)) + facet_wrap(~ Season + Pk, scales="fixed") + geom_density() + coord_cartesian(xlim=c(-1,1)); b

# Stats
lac.prc.sp.2.dt[,lapply(c(mean, sd, skewness, kurtosis), function(x) x(Ln.Rtrn)), by=list(Season, Pk)]

# Jumps?
lac.prc.sp.2.dt[,lapply(c(0:10), function(x) sum(abs(Ln.Rtrn) >(x*sd(Ln.Rtrn)))) %>% set_names(0:10), by=list(Season, Pk)]
lac.prc.sp.2.dt[,lapply(c(0:10), function(x) (100*(sum(abs(Ln.Rtrn) >(x*sd(Ln.Rtrn)))/.N - 2*(1-pnorm(x))))) %>% set_names(0:10), by=list(Season, Pk)]




lac.prc.sp.dt %>% setkey(QDate)
lac.prc.sp.dt[,Prc.Adj:=Price+15]
lac.prc.sp.dt[,Ln.Rtrn:=c(0,log(Price[2:.N]/Price[1:(.N-1)]))]





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


## ----- Read in T:\ Lacima Files
library(data.table)
library(magrittr)

setwd("T:/")
lac.trds.new.dt <- fread("LAC_TRADES_NEW.rpt", sep = "|", header=T)
lac.trds.new.dt %>% str


## ----- Get EaR Output
library(data.table)
library(RODBC)
library(magrittr)
library(stringi)
library(ggplot2)

options(data.table.nomatch=0)
options(stringsAsFactors=F)

source("C:/R/Secure v1.R")

sql.qry <- stri_paste("Select rvEaR.raEarningsAtRiskID, rvEaROutput.BucketStartDate, ",
                      "rvEaROutput.BucketEndDate, rvEaROutput.OutputTypeName, rvEaROutput.Percentile, ",
                      "rvEaROutput.EarValue, rvEaROutput.DistributionProperty, ",
                      "rvEaROutput.ContractBucketType, rvEaROutput.ContractBucketValue, ",
                      "rvEaROutput.IntradaySplit, rvEaROutput.AggregationTypeDecription ",
                      "From rvEaR Inner Join ",
                      "rvEaROutput On rvEaR.raEarningsAtRiskID = rvEaROutput.raEarningsAtRiskID ",
                      "Where rvEaR.raEarningsAtRiskID = 101 And rvEaROutput.BucketType = 'Monthly'",
                      sep="")

lac.odbc <- odbcConnect("Lacima_Prod", uid = ps.login, pwd=ps.pwd)

a.t <- Sys.time()
sql.rslt.dt <- sqlQuery(lac.odbc, sql.qry) %>% as.data.table  # ~1/2 hour to run 
b.t <- Sys.time()
b.t - a.t

sql.copy.dt <- copy(sql.rslt.dt)
sql.rslt.dt <- copy(sql.copy.dt)

# Convert Factor cols to Character
fctr.to.ch.f <- function(x) if("factor" %chin% class(x)) as.character(x) else x
sql.rslt.dt <- sql.rslt.dt[,lapply(.SD, fctr.to.ch.f)]


#sql.rslt.dt[,":="(c("TT2I", "RF"), transpose(stri_split(ContractBucketValue, fixed="\\"))), by=ContractBucketValue]
sql.rslt.dt[,":="(c("Output", "RF"), transpose(stri_match_all_regex(OutputTypeName, "^\\s*([^(\\s]+)([\\s(]*([^)]*)[)])?"))[c(2,4)]), by=OutputTypeName]
sql.rslt.dt[,RF:=stri_replace_all_fixed(RF, " ", "_")]
sql.rslt.dt[,Zkey:=stri_match_first_regex(ContractBucketValue, "^\\d+") %>% as.numeric, by=ContractBucketValue]
sql.rslt.dt[,BucketStartDate.IDt:=as.IDate(BucketStartDate, "%Y-%m-%d", tz="GMT"), by=BucketStartDate]


sql.rslt.dt[!is.na(EarValue)][!J(0), transpose(list(range(EarValue))), by=list(DistributionProperty, AggregationTypeDecription, Output), on="EarValue"]
sql.rslt.dt[!is.na(EarValue)][,transpose(list(range(EarValue))), by=list(DistributionProperty, AggregationTypeDecription, Output)]

setkey(sql.rslt.dt, DistributionProperty, AggregationTypeDecription, Output)
sql.rslt.2.dt <- sql.rslt.dt[J("Mean", "PayoffDate", "Volume")]

#sql.rslt.2.dt <- sql.rslt.dt[J("Mean", "PayoffDate", "Volume"), 
#                             on=c("DistributionProperty", "AggregationTypeDecription", "Output")]

setkey(sql.rslt.2.dt, EarValue)
sql.rslt.3.dt <- sql.rslt.2.dt[!J(0)]
#sql.rslt.3.dt <- sql.rslt.2.dt[!J(0),on="EarValue"]

sql.rslt.4.dt <- sql.rslt.3.dt[,list(Zkey, RF, BucketStartDate.IDt, IntradaySplit, EarValue)]

sql.rslt.5.dt <- dcast(sql.rslt.4.dt, Zkey + RF + BucketStartDate.IDt ~ IntradaySplit, value.var = "EarValue", fun.aggregate = sum)
setnames(sql.rslt.5.dt, old=c("Baseload", "Peak", "OffPeak"), new=c("Lac.Baseload", "Lac.Peak", "Lac.OffPeak"))
setnames(sql.rslt.5.dt, old=c("Baseload", "Peak"), new=c("Lac.Baseload", "Lac.Peak"))


setwd("D:/Lacima/Users/MRP/")
#write.csv(sql.rslt.3.dt, file = "20160120 Hedge Positions by ContractID v1.csv", quote = F, row.names = F)

# ----- Read Aligne File 
aligne.1.dt <- fread("20160120 Hedge Positions from Aligne v1.csv", header = T)

aligne.2.dt <- aligne.1.dt[,list(Zkey, `RF_1 (Bskt)`, `+1DMO`, `POW SEGMENT2`, `TRADE TYPE2 (Itemize)`, `DELTA (Raw)`)]
setnames(aligne.2.dt, c("Zkey", "RF", "DMO", "PS2", "TT2I", "Delta"))
aligne.2.dt[,RF.Cmmdty:=stri_extract_first_regex(RF, "^[^_]*"), by=RF]

setkey(aligne.2.dt, RF.Cmmdty)
aligne.3.dt <- aligne.2.dt[J(c("Pwr", "Gas", "Coal")),
                           list(Delta=sum(Delta %>% stri_replace_all_fixed(",", "") %>% as.numeric)), 
                           by=list(Zkey, RF, DMO, PS2, TT2I)]

aligne.3.dt[,DMO.IDt:=as.IDate(DMO, "%d-%b-%y", tz="GMT"), by=DMO]
aligne.3.dt[,DMO:=NULL]

aligne.4.dt <- dcast(aligne.3.dt, Zkey + RF + DMO.IDt + TT2I ~ PS2, value.var = "Delta", fun.aggregate = sum)
setnames(aligne.4.dt, old=c("PEAK", "OFF PEAK", "RTC"), new=c("Al.Peak", "Al.OffPeak", "Al.Baseload"))
aligne.4.dt[,Al.Baseload:=ifelse(Al.Peak !=0 | Al.OffPeak != 0, (Al.Peak + Al.OffPeak), Al.Baseload)]

setwd("D:/Lacima/Users/MRP/")
bad.trds.1.dt <- fread("20160126 Electric Fixed Swap w Changing volumes v1.csv", header=T)
bad.trds.1.dt %>% setkey(ALIGNE_TNUM)

# Join
setkey(aligne.4.dt, Zkey, RF, DMO.IDt)
setkey(sql.rslt.5.dt, Zkey, RF, BucketStartDate.IDt)
join.1.dt <- aligne.4.dt[sql.rslt.5.dt, nomatch=0]

setkey(join.1.dt, Zkey)
join.2.dt <- join.1.dt[!bad.trds.1.dt,lapply(.SD, sum), by=list(RF, TT2I, DMO.IDt), .SDcols=5:10]
join.3.dt <- copy(join.1.dt)

join.2.dt[,":="(Baseload.Diff=Al.Baseload-Lac.Baseload, Peak.Diff=Al.Peak-Lac.Peak, OffPeak.Diff=Al.OffPeak-Lac.OffPeak)]
join.2.dt[,":="(Baseload.Diff.Prct=(Al.Baseload-Lac.Baseload)/Al.Baseload, 
                Peak.Diff.Prct=(Al.Peak-Lac.Peak)/Al.Peak,
                OffPeak.Diff.Prct=(Al.OffPeak-Lac.OffPeak)/Al.OffPeak)]

join.3.dt[,":="(Baseload.Diff=Al.Baseload-Lac.Baseload, Peak.Diff=Al.Peak-Lac.Peak, OffPeak.Diff=Al.OffPeak-Lac.OffPeak)]
join.3.dt[,":="(Baseload.Diff.Prct=(Al.Baseload-Lac.Baseload)/Al.Baseload, 
                Peak.Diff.Prct=(Al.Peak-Lac.Peak)/Al.Peak,
                OffPeak.Diff.Prct=(Al.OffPeak-Lac.OffPeak)/Al.OffPeak)]

join.2.dt[,.N,by=list(RF, TT2I)]

# Swaptions only
join.1.dt[,":="(Baseload.Diff.Prct=(Al.Baseload-Lac.Baseload)/Al.Baseload, 
                Peak.Diff.Prct=(Al.Peak-Lac.Peak)/Al.Peak)]
join.3.dt <- copy(join.1.dt)
# End Swaptions



plot.1.f <- function(sd.dt, by.l) {
  p.1.gg <- ggplot(sd.dt, aes(x=DMO.IDt, y=Baseload.Diff.Prct, color=TT2I)) + geom_point() + geom_line()
  p.1.gg <- p.1.gg + coord_cartesian(ylim=c(-1,1))
  p.1.gg <- p.1.gg + ggtitle(by.l$RF)
  
  plot(p.1.gg)
  1
  
}

setkey(join.2.dt, RF, TT2I)
pdf("MRP Baseload 3.pdf")
join.2.dt[,plot.1.f(.SD, .BY), by=list(RF)]
dev.off()


plot.2.f <- function(sd.dt, by.l) {
  require(ggplot2)
  require(stringi)
  
  p.2.gg <- ggplot(sd.dt, aes(x=DMO.IDt, y=Baseload.Diff)) + geom_point() + geom_line()
  p.2.gg <- p.2.gg + coord_cartesian(ylim=c(-1,1))
  p.2.gg <- p.2.gg + ggtitle(stri_paste(by.l$RF, by.l$TT2I, by.l$Zkey, sep=" - "))
  
  plot(p.2.gg)
  1
  
}

setkey(join.3.dt, RF, TT2I, Zkey)
pdf("MRP Baseload Swaptions 2.pdf")
#join.3.dt[J("Pwr_DA_MF__L__PJM_West"),plot.2.f(.SD, .BY), by=list(RF, TT2I, Zkey)]
join.1.dt[,plot.2.f(.SD, .BY), by=list(RF, TT2I, Zkey)]
dev.off()
