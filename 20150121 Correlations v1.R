# Libraries
library(data.table)
library(magrittr)
library(fasttime)
library(XLConnect)
library(reshape2)
library(ggplot2)
library(stringi)

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

# Working Dir
setwd("C:/Temp/Correlations/")

as.IDate.f <- function(date.ch) {
  as.IDate(date.ch, "%m/%d/%Y", tz='GMT')
}

prices.dt <- fread("20150121 Prices v1.csv")
prices.dt[,':='(PRICING_DATE.Dt=as.IDate.f(PRICING_DATE), FORWARD_DATE.Dt=as.IDate.f(FORWARDMONTH))]
prices.dt[,':='(PRICING_DATE=NULL, FORWARDMONTH=NULL)]

prices.2.dt <- melt.data.table(prices.dt, measure.vars = c("ON_PEAK", "OFF_PEAK", "RTC"), variable.name = "PK_PRD", value.name = "PRICE")

prices.dt[,MKT.CDY:=paste(MARKET, COMPONENT, )]
prices.dt %>% str





