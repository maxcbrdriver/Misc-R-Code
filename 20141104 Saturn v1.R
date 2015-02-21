# Process Aligne Saturn reports

# Packages
library(data.table)
library(stringi)
library(magrittr)
library(fasttime)
library(reshape2)
library(ggplot2)


#----- Settings -----
R.version
getRversion()
search()

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

to.date.f <- . %>% as.IDate("%m/%d/%Y", tz='GMT')
to.dbl.f <- . %>% stri_replace_all_fixed(",", "") %>% as.double

setwd("X:\\Zainet\\odbs")

# ----- Saturn Input Trade Parsing -----
Strn.All.Trds.dt <- fread("Strn_All_Trds.rpt", sep="|")
Strn.All.Trds.dt[,V47:=NULL]
Strn.All.Trds.dt %>% str
Strn.All.Trds.dt[,.N]

Strn.All.Trds.Cols <- c("LAST_UPDATE","SOURCE", "CREATED_ON", "VOIDED", "FIRST_DAY", "LAST_DAY", "PERIOD")
Strn.All.Trds.dt[,":="(stri_paste(Strn.All.Trds.Cols, "_DT"),
                       lapply(.SD, to.date.f)), 
                 .SDcols=Strn.All.Trds.Cols]
Strn.All.Trds.dt[,Strn.All.Trds.Cols:=NULL, with=FALSE]
Strn.All.Trds.dt[,":="(VOLUME=to.dbl.f(VOLUME), HEATRATE=to.dbl.f(HEATRATE))]


Strn.Miss.Trds.dt <- fread("Strn_Miss_Trds.rpt", sep="|")
Strn.Miss.Trds.dt[,V33:=NULL]
Strn.Miss.Trds.dt %>% str
Strn.Miss.Trds.dt[,.N]

Strn.Miss.Trds.Cols <- c("Report Date", "Trade Date", "Incep Date", "Void Date", "Del Date Start", "Del Date End")
Strn.Miss.Trds.dt[,":="(stri_paste(Strn.Miss.Trds.Cols, " IDt"),
                       lapply(.SD, to.date.f)), 
                 .SDcols=Strn.Miss.Trds.Cols]
Strn.Miss.Trds.dt[,Strn.Miss.Trds.Cols:=NULL, with=FALSE]


# Traders
Strn.All.Trds.dt[,sort(unique(TRADER))]

Strn.All.Trds.dt %>% setkey(COUNTERPARTY)
Strn.All.Trds.dt[J("APEXOIL"), sort(unique(REFERENCE))]

Strn.All.Trds.dt %>% setkey(REFERENCE)
Strn.All.Trds.dt[J(c("CKI279","CKI280"))]

# JCPL vs HH Spreads
Strn.All.Trds.dt[,sort(unique(STYLE))]
Strn.All.Trds.dt %>% setkey(STYLE, MARKET_1_ATTR_3, MARKET_2_ATTR_3)
JC.NG.1.dt <- Strn.All.Trds.dt[J("HRate", "JCPL", "NG")]
JC.NG.1.dt %>% setkey(POSITION)
JC.NG.1.dt[J("Sale"), PwrQty:=-VOLUME]
JC.NG.1.dt[J("Purch"), PwrQty:=VOLUME]
JC.NG.1.dt[J("Sale"), GasQty:=HEATRATE*VOLUME]
JC.NG.1.dt[J("Purch"), GasQty:=-HEATRATE*VOLUME]

JC.NG.1.dt[SOURCE_DT<=to.date.f("11/6/2014"),list(Agg.GasQty.TBtu=sum(GasQty)/1e9), keyby=list(PERIOD_DT)]


# LIPA Books
Strn.All.Trds.dt[,sum(unique(BOOK) %in% c("LIFO"))]
Strn.All.Trds.dt %>% setkey(BOOK)
Strn.All.Trds.dt[J("LIFO"), unique(REFERENCE)]
Strn.All.Trds.dt[J("LIFO"), list(REFERENCE, FIRST_DAY_DT, LAST_DAY_DT)]

# Trade Duplication?
Strn.All.Trds.dt %>% setkey(NULL)
Strn.All.Trds.dt %>% key

Strn.All.Trds.Uniq.dt <- unique(Strn.All.Trds.dt)
(Strn.All.Trds.dt %>% nrow) == (Strn.All.Trds.Uniq.dt %>% nrow)

Strn.All.Trds.dt %>% str
Strn.All.Trds.dt %>% setkey(REFERENCE, PEAKNESS, PERIOD_DT, STRIKE, HEATRATE, VOLUME)
Strn.All.Trds.Uniq.dt <- unique(Strn.All.Trds.dt)
(Strn.All.Trds.dt %>% nrow) == (Strn.All.Trds.Uniq.dt %>% nrow)

t.1 <- Strn.All.Trds.dt[,.N, keyby=list(REFERENCE)]
t.2 <- Strn.All.Trds.Uniq.dt[,.N, keyby=list(REFERENCE)]
t.3 <- t.1[t.2]
t.3[, Diff:= N-i.N]
t.3 %>% setkey(Diff)
t.4 <- t.3[Diff>0]

Strn.All.Trds.dt %>% key
t.4 %>% setkey(REFERENCE)
t.5 <- Strn.All.Trds.dt[t.4]
t.5 %>% setkey(REFERENCE, PERIOD_DT, TYPE, STYLE)
t.5


# Missing?
Strn.Miss.Trds.dt %>% str
Strn.Miss.Trds.dt[,sort(unique(TNUM))]


# ----- Trade Types -----
setwd("S://Risk Management/MRP/06 - Aligne/02 - Configuration Dumps/2014/20141208/")
Trd.Types.dt <- fread("MRP_FIND_TRDTYPS.rpt", sep="|",verbose = T)
Trd.Types.dt %>% str
Trd.Types.dt[,V34:=NULL]

Trd.Types.Cols <- c("REPORT_DATE", "TRADE_DATE", "VOID_DATE", "VOID_SYSTEM_DATE", "DEL_DATESTART", "DEL_DATEEND")
Trd.Types.dt[,":="(stri_paste(Trd.Types.Cols, "_IDt"),
                        lapply(.SD, to.date.f)), 
                  .SDcols=Trd.Types.Cols]
Trd.Types.dt[,Trd.Types.Cols:=NULL, with=FALSE]

# How often are VOIDs back dated, i.e. VOID SYSTEM DATE > VOID DATE?
Trd.Types.dt[,BACK_DATED:=(VOID_SYSTEM_DATE_IDt > VOID_DATE_IDt)]
Trd.Types.dt %>% setkey(BACK_DATED)

funcs.c <- c("mean", "min", "max", "median", "sd", "length")
Trd.Types.dt[J(T), unlist(lapply(funcs.c, function(x) {f <- get(x); r <- f(VOID_SYSTEM_DATE_IDt - VOID_DATE_IDt); names(r) <- x; r}))]

funcs.l <- list(Mean="mean", Min="min", Max="max", Median="median", SD="sd", Length="length")
funcs.l <- funcs.c %>% as.list %>% `names<-`(funcs.c)
Trd.Types.dt[J(T), lapply(funcs.l, function(x) {f <- get(x); f(VOID_SYSTEM_DATE_IDt - VOID_DATE_IDt)})]

Trd.Types.dt %>% setkey(OHM_GT_TRD, TRADE_TYPE2_I, CDY1_ATTR1, CDY2_ATTR1)
Trd.Types.dt[,sort(unique(TRADE_TYPE2_I))]
Trd.Types.dt[J(1), range(TRADE_DATE_IDt)]

Trd.Types.dt %>% setkey(OHM_GT_TRD)
Trd.Types.Summ.dt <- Trd.Types.dt[J(1),.N, by=list(TRADE_TYPE2_C, TRADE_TYPE2_I, CDY1_ATTR1, CDY2_ATTR1)] %>% 
  setorder(TRADE_TYPE2_C, TRADE_TYPE2_I)
View(Trd.Types.Summ.dt)

# Trade Type Summary 1
t.dt <- Trd.Types.dt[,.N, keyby=list(TRADE_TYPE2_I, TRADE_TYPE2_C, CDY1_ATTR1, CDY2_ATTR1, OPT_AVERAGE)]
t.dt[, Prct:=N/sum(N)]
write.csv(t.dt, "Trade Types.csv")

t1.dt <- Trd.Types.dt[,.N, keyby=list(CDY1_ATTR1, MKT1, CDY1)]
t2.dt <- Trd.Types.dt[,.N, keyby=list(CDY2_ATTR1, MKT2, CDY2)]
setnames(t1.dt, c("CDY_ATTR1", "MKT", "CDY", "N"))
setnames(t2.dt, c("CDY_ATTR1", "MKT", "CDY", "N"))
t.all.dt <- rbindlist(list(t1.dt, t2.dt))
t.all.2.dt <- t.all.dt[,list(N=sum(N)), by=list(CDY_ATTR1, MKT, CDY)]
t.all.2.dt %>% setorder(CDY_ATTR1, MKT, CDY)
write.csv(t.all.2.dt, "Market Comps.csv")

# Trade Type Summary 2
Trd.Types.dt %>% setkey(NULL)
Trd.Types.dt[,KEEP:= (TRADE_DATE_IDt >= to.date.f("1/1/2014") & VOID_DATE_IDt == to.date.f("12/31/2099")) & BOOK_ATTR2!="GAS SUP"]
Trd.Types.dt %>% setkey(KEEP)
t.dt <- Trd.Types.dt[J(T)]

t.2.dt <- t.dt[,list(N=.N, ExTNUM=TNUM[1], TrdDate=max(TRADE_DATE_IDt)), by=list(TRADE_TYPE2_I, TRADE_TYPE2_C, CDY1_ATTR1, CDY2_ATTR1, OPT_TYPE, OPT_AVERAGE, FIX1_METH, FIX1_METHD, FIX1_METHV, FIX1_TYPE, FIX2_METH, FIX2_METHD, FIX2_METHV, FIX2_TYPE)]
setorder(t.2.dt, CDY1_ATTR1, CDY2_ATTR1, TRADE_TYPE2_I)
write.csv(t.2.dt, "Trades 2.csv")

# ----- PRCFWD -----
Prc.Entry.dt <- fread("MRP_PRC_FWD.rpt", sep="|",verbose = T)
Prc.Entry.dt %>% str
Prc.Entry.dt[,V20:=NULL]

Prc.Entry.Cols <- c("REPORT_DATE", "PRICE_DATE", "ALIGNE_REC_VOID_DATE")
Prc.Entry.dt[,":="(stri_paste(Prc.Entry.Cols, "_IDt"),
                   lapply(.SD, to.date.f)), 
             .SDcols=Prc.Entry.Cols]
Prc.Entry.dt[,Prc.Entry.Cols:=NULL, with=FALSE]


Prc.Entry.Cnts.dt <- Prc.Entry.dt[,.N, keyby=list(PRICESET, MARKET, COMPONENT, PRC_REC_TYPE)]
setorderv(Prc.Entry.Cnts.dt, "N", order=-1)
Prc.Entry.Cnts.dt[,lapply(.SD, stri_trim_both), .SDcols=c("PRICESET", "MARKET", "COMPONENT")]

Prc.Entry.dt %>% setkey(PRICESET, MARKET, COMPONENT, PRC_REC_TYPE)
lookup.dt <- data.table(PRICESET=c("MARKET", "BASIS"), 
                        MARKET=c("PJM", "PJM"), 
                        COMPONENT=c("WEST", "JCPL"), 
                        PRC_REC_TYPE=c("F", "F"))
Prc.Entry.dt[lookup.dt,.N]
