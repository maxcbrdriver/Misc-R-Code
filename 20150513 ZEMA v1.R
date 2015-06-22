library(magrittr)
library(stringi)
library(data.table)


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


# Part 1 - Fix Spot Power
getwd()
setwd("C:/")
inp.files <- list.files(path = "C:/Temp/ZEMA/20150512/Process/Input/", full.names = T)

# DST Import
DST.dt <- fread("C:/Temp/DST Start End v1.csv", header=T)
DST.dt[,":="(St=as.IDate(`DST Start`, "%m/%d/%Y", tz='GMT'), 
             End=as.IDate(`DST End`, "%m/%d/%Y", tz='GMT'))]
DST.dt[,":="(`DST Start`=NULL, `DST End`=NULL)]

# DST Adjustment Structures
DST.Fill.dt <- copy(DST.dt)
DST.Fill.dt[,":="(Year=NULL, End=NULL)]
setkey(DST.Fill.dt, St)

DST.Adj.dt <- melt(DST.dt, id.vars = c("Year"), variable.name = "Transition", value.name = "Date")
DST.Adj.dt[,Adj.Value:=ifelse(Transition=="St", 1,0)]
DST.Adj.dt[,":="(Year=NULL, Transition=NULL)]
DST.Adj.dt[,HE:=3L]
DST.Adj.dt[,DateTime:=as.integer(paste0(strftime(Date, "%Y%m%d", tz='GMT'), stri_pad_left(HE, min_length=2, pad="0")))]
DST.Adj.dt[,":="(Date=NULL, HE=NULL)]
setcolorder(DST.Adj.dt, c("DateTime", "Adj.Value"))
setkey(DST.Adj.dt, DateTime)

#file.i <- inp.files[1]

for(file.i in inp.files) {
  # Import Price Files
  pwr.dt <- fread(file.i, header = T)
  pwr.melt.dt <- melt(pwr.dt, id.vars = c("Date", "RiskFactor"), variable.name = "HE", value.name = "Price")
  setkey(pwr.melt.dt, Price)
  pwr.melt.dt[J(-999999), Price:=NA]
  setkey(pwr.melt.dt)
  pwr.melt.dt[,Date.IDate:=as.IDate(Date, "%m/%d/%Y", tz='GMT'), by=Date]
  pwr.melt.dt[,HE.Int:=as.integer(as.character(HE)), by="HE"]
  pwr.melt.dt[,":="(Date=NULL, HE=NULL)]
  setnames(pwr.melt.dt, c("Date.IDate","HE.Int"), c("Date", "HE"))
  pwr.melt.dt[,DateTime:=as.integer(paste0(strftime(Date, "%Y%m%d", tz='GMT'), stri_pad_left(HE, min_length=2, pad="0")))]
  setkey(pwr.melt.dt, DateTime)

  # Adjust Dates/Times For DST
  pwr.adj.dt <- DST.Adj.dt[pwr.melt.dt, roll=T]
  pwr.adj.dt[,HE.Adj.1:=HE+Adj.Value]
  pwr.adj.dt[,Date.Adj.1:=as.IDate(Date + ifelse(HE.Adj.1==25,1,0))]
  pwr.adj.dt[,HE.Adj.2:=((HE.Adj.1-1) %% 24)+1 ]
  pwr.adj.dt[,":="(DateTime=NULL, Date=NULL, HE=NULL, HE.Adj.1=NULL, Adj.Value=NULL)]
  setnames(pwr.adj.dt, c("Date.Adj.1", "HE.Adj.2"), c("Date", "HE"))

  # Cast
  pwr.adj.grid.dt <- dcast(pwr.adj.dt, Date + RiskFactor ~ HE, fun.aggregate = mean, value.var = "Price", fill=NA)

  # Calculate Missing Spring Value
  setkey(pwr.adj.grid.dt, Date)
  pwr.adj.grid.dt[DST.Fill.dt, `3`:=mean(c(`2`,`4`)), nomatch=0, by=.EACHI]
  pwr.adj.grid.dt[,":="(as.character(1:24), lapply(.SD, function(x) ifelse(is.na(x), -999999,x))), .SDcols=as.character(1:24)]
  
  write.csv(pwr.adj.grid.dt, 
            stri_replace_all_fixed(file.i, "/Input/", "/Output/"), 
            row.names=F,
            quote=F)
}


# Part 2
library(data.table)
library(magrittr)
library(stringi)

setwd("C:/")
base.dir <- "C:/Temp/ZEMA/20150618b/"
sub.dirs <- c("GasFWD", "PowerFWD 2013 Batch 1", "PowerFWD 2014 Batch 1", "PwerFWD 2015 Batch 1", "PowerFWD NY Rerun")
sub.dirs <- c("20150521", "CurrentForwardsCoalMonthly")
sub.dirs <- c("CurrentPowerMonthlyExtra - 06.01.2015")
sub.dirs <- c("Tenn New")
sub.dirs <- c("CurrentForwardsOilMonthly - 05.27.2015")
sub.dirs <- c("CurrentSpotCoalDaily - 06.05.2015 - Re-run", "CurrentSpotOilDaily - 05.27.2015")
sub.dirs <- c("NE Mass Monthly", "NY Zone A Monthly", "NY Zone G & J Monthly", "PJM West Monthly")
out.dir <- stri_paste(base.dir, "Upload/")

full.dirs <- sapply(sub.dirs, . %>% stri_paste(base.dir, ., sep=""))
files.dt <- lapply(full.dirs, function(x) data.table(File.Name=list.files(path=x, full.names=T, recursive = T))) %>% 
              rbindlist(use.names=T)
files.dt[,File.Name.2:=stri_replace_all_regex(File.Name, "(CFGMT0\\d)_(\\d{2})", "$1$2")]

# Forwards
files.dt[, ":="(c("Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), 
                stri_match_all_regex(File.Name.2, 
                                     "ForwardQuotes_([^_]*_){5}([^_]*)_([^_]*)_([^_]*_[^_]*_[^_]*)_([^_]*)_(.*?)_(\\d{2}-\\d{2}-\\d{4})")[[1]][3:8] %>% as.list),
         by=File.Name.2]
# Spot 
files.dt[, ":="(c("Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), 
                stri_match_all_regex(File.Name.2, 
                                     "History_Spot_([^_]*_){4}([^_]*)_([^_]*)_([^_]*_[^_]*_[^_]*)_([^_]*)_(.*?)_(\\d{2}-\\d{2}-\\d{4})")[[1]][3:8] %>% as.list),
         by=File.Name.2]

files.dt[,RF:=stri_paste(Cmmdty, Spot, Model, Basis, Loc, sep="_")]
setkey(files.dt, RF)

# Forwards
read.prc.f <- function(file.x) {
  x.dt <- fread(file.x, header=T)
  setkey(x.dt, QuoteValue)
  y.dt <- x.dt[!J(-999999)]
  y.dt
}

proc.file.grp.f <- function(file.grp, by.l) {
  out.dt <- lapply(file.grp, read.prc.f) %>% rbindlist(use.names=T)
  setorder(out.dt, QuoteDate, RiskFactor, StartDate, QuoteType)
  write.csv(out.dt, stri_paste(out.dir,by.l$RF, ".csv",sep=""), quote = F, row.names = F)
  1
}

tmp.dt <- files.dt[,proc.file.grp.f(File.Name, .BY), by=RF]

# Spot
read.spot.f <- function(file.x) {
  print(file.x)
  x.dt <- fread(file.x, header=T)
  setkey(x.dt, `1`)
  y.dt <- x.dt[!J(-999999)]
  y.dt
}

proc.spot.grp.f <- function(file.grp, by.l) {
  in.dt <- lapply(file.grp, read.spot.f) %>% rbindlist(use.names=T)
  out.dt <- in.dt[,list(RiskFactor=unique(RiskFactor), `1`=mean(`1`)), by=Date]
  setorder(out.dt, Date)
  setcolorder(out.dt, c("Date", "RiskFactor", "1"))
  write.csv(out.dt, stri_paste(out.dir, by.l$RF, ".csv", sep=""), quote=F, row.names=F)
  1
}

tmp.dt <- files.dt[,proc.spot.grp.f(File.Name, .BY), by=RF]


# Part 3 - Review
to.IDate.f <- . %>% as.IDate(., "%m/%d/%Y", tz='GMT')

prc.dt <- fread("Pwr_DA_MF__L__Zn_J.csv", header=TRUE)

prc.dt[,QDate.Dt:=to.IDate.f(QuoteDate)]
prc.dt %>% setkey("QDate.Dt")

View(prc.dt[J(to.IDate.f("5/13/2015"))])
View(prc.dt[J(to.IDate.f("2/13/2013"))])
View(prc.dt[J(to.IDate.f("1/23/2013"))])


# Part 4
getwd()
setwd("C:/Temp/Nodal Exchange/FTP Donwload/exchange/archive/")

files <- list.files()

files[sapply(stri_extract_all_fixed(files, "NEX_EOD_PRICES_"), function(x) !is.na(x))]


Sys.timezone()
Sys.setenv(TZ='GMT')

getwd()
setwd("S:/Risk Management/MRP/05 - Portfolio Analytics/04 - ZE Power/04 - Prices/20150415 Test/Spot")
files <- list.files()

files.dt <- data.table(Files=files)
files.dt[,Cmdty:=NULL]
files.dt[,Cmdty:=stri_split_fixed(Files, "_")[[1]][5], by=Files]
setkey(files.dt, Cmdty)
files.Gas <- files.dt[J("Gas"), Files]
files.PJM <- files.dt[J("PJM"), Files]

read.Gas.f <- function(file.str) {
  require(data.table)
  require(stringi)
  
  date.IDt <- as.IDate(stri_match_first_regex(file.str, "\\d{8}")[1,1], "%Y%m%d", tz='GMT')
  
  file.dt <- fread(file.str, header=T)
  
  file.dt[,Prc.IDt:=as.IDate(Date, "%m/%d/%Y", tz='GMT')]
  file.dt[,Date:=NULL]
  file.dt %>% setnames("1", "Prc")
  file.dt[,AsOf.IDt:=date.IDt]
  
  return(file.dt)
}

Gas.dt <- lapply(files.Gas, read.Gas.f) %>% rbindlist()
Gas.2.dt <- dcast(Gas.dt, AsOf.IDt ~ Prc.IDt + RiskFactor, value.var = "Prc")


