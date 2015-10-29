library(data.table)
library(magrittr)
library(RODBC)
library(stringi)

source("C:/GitHub/Misc-R-Code/Secure v1.R")


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


lac.odbc <- odbcConnect("Lacima_Dev", uid = ps.login, pwd=ps.pwd)

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

