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
library(ggplot2)

as.IDate.f <- function(date.ch) {
  as.IDate(date.ch, "%m/%d/%Y", tz='GMT')
}

# DST
dst.dt <- fread("C:/Temp/DST Start End v2.csv", header=T)
dst.dt[,Date.Dt:=as.IDate.f(Date)]
dst.dt[,Date:=NULL]
setnames(dst.dt, "Date.Dt", "Date")
setcolorder(dst.dt, c("Date","DST Type"))
setkey(dst.dt, "Date")


setwd("C:/")
base.dir <- c("C:/Temp/ZEMA/20150714b/")
#sub.dirs <- c("Power 02 Spot NE", "Power 02 Spot NY", "Power 02 Spot PJM")
#sub.dirs <- c("20150618a/PJM West Monthly", "20150622a/PJM West Daily", "20150525a/20150521")
sub.dirs <- "Power 02 Spot PJM/PJM_AECO"
out.dir <- stri_paste(base.dir, "Upload/")

full.dirs <- sapply(sub.dirs, . %>% stri_paste(base.dir, ., sep=""))
files.dt <- lapply(full.dirs, function(x) data.table(File.Name=list.files(path=x, full.names=T, recursive = T))) %>% 
              rbindlist(use.names=T)

# Spot or Forward?
files.dt[,Type:=stri_match_first_regex(File.Name, "([^/_]*)[^/]*$")[,2]]
setkey(files.dt, Type)

files.dt[,':='(c("Profile SC", "Provider SC", "TariffType SC", "RunType SC", "Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), "")]

# File Name Decomposition
# Forward
files.dt[J("ForwardQuotes"),
         ':='(c("Profile SC", "Provider SC", "TariffType SC", "RunType SC", "Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), 
                stri_match_all_regex(File.Name, 
                                     "ForwardQuotes_([^_]*)_([^_]*)_([^_]*)_([^_]*)__([^_]*)_([^_]*)_([^_]*_[^_]*_[^_]*)_([^_]*)_(.*?)_(\\d{2}-\\d{2}-\\d{4})")[[1]][2:11] %>% as.list),
         by=File.Name]
# Spot 
files.dt[J("History"),
         ':='(c("Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), 
                stri_match_all_regex(File.Name, 
                                     "History_Spot_([^_]*_){4}([^_]*)_([^_]*)_([^_]*_[^_]*_[^_]*)_([^_]*)_(.*?)_(\\d{2}-\\d{2}-\\d{4})")[[1]][3:8] %>% as.list),
         by=File.Name]

files.dt[,RF:=stri_paste(Cmmdty, Spot, Model, Basis, Loc, sep="_")]
setkey(files.dt, RF)

# Process Files
# Forwards
read.frwd.prc.f <- function(file.x) {
  x.dt <- fread(file.x, header=T)
  setkey(x.dt, QuoteValue)
  x.dt <- x.dt[!J(-999999)]
  x.dt
}

proc.frwd.file.grp.f <- function(file.grp, by.l) {
  out.1.dt <- lapply(file.grp, read.frwd.prc.f) %>% rbindlist(use.names=T)
  
  out.1.dt %>% setkey(NULL)
  out.2.dt <- unique(out.1.dt)
  
  out.2.dt[,':='(c("QuoteDate.Dt", "StartDate.Dt", "EndDate.Dt"), lapply(.SD, as.IDate.f)), .SDcols=c("QuoteDate", "StartDate", "EndDate")]
  
  out.2.dt[,Daily:=ifelse(StartDate.Dt==EndDate.Dt,T,F)]
  out.2.dt[,Keep:=ifelse(EndDate.Dt>=QuoteDate.Dt,T,F) ]
  
  setkey(out.2.dt, Keep)
  out.3.dt <- out.2.dt[J(T)]
  setorder(out.3.dt, RiskFactor, QuoteDate.Dt, StartDate.Dt, QuoteType)
  out.3.dt[,':='(QuoteDate.Dt=NULL, StartDate.Dt=NULL, EndDate.Dt=NULL, Daily=NULL, Keep=NULL)]
  write.csv(out.3.dt, stri_paste(out.dir,"ForwardQuotes_", by.l$RF, ".csv",sep=""), quote = F, row.names = F)
  
  1
}

# Spot
read.spot.prc.f <- function(file.x) {
  x.dt <- fread(file.x, header=T)
  setkey(x.dt, `1`)
  y.dt <- x.dt[!J(-999999)]
  y.dt
}

proc.spot.file.grp.f <- function(file.grp, by.l) {
  in.dt <- lapply(file.grp, read.spot.prc.f) %>% rbindlist(use.names=T)
  
  in.dt %>% setkey(NULL)
  out.1.dt <- unique(in.dt)
  
  setkey(out.1.dt, Date)
  max.cnt <- out.1.dt[,.N, by=Date][,max(N)]
  
  out.2.dt <- out.1.dt[,list(RiskFactor=unique(RiskFactor), `1`=mean(`1`)), by=Date]
  out.2.dt[,Date.Dt:=as.IDate.f(Date)]
  setorder(out.2.dt, Date.Dt)
  out.2.dt[,Date.Dt:=NULL]
  setcolorder(out.2.dt, c("Date", "RiskFactor", "1"))
  write.csv(out.2.dt, stri_paste(out.dir, "History_Spot_", by.l$RF, ".csv", sep=""), quote=F, row.names=F)
  
  max.cnt
}

read.spot.pwr.2.prc.f <- function(file.x) {
  require(data.table)
  require(magrittr)
  
  # Import
  a.dt <- fread(file.x, header=F, skip=1)
  
  # Assign Column Names
  setnames(a.dt, c("Date", "RiskFactor", 1:(length(a.dt)-2)))
  a.dt[,':='(seq(4, length(a.dt), 2), NULL)]
  setnames(a.dt, c("Date", "RiskFactor", 1:(length(a.dt)-2)))
  
  a.dt[,Date.Dt:=as.IDate.f(Date)]
  a.dt[,Date:=NULL]
  setnames(a.dt, "Date.Dt", "Date")
  setcolorder(a.dt, c(length(a.dt), 1:(length(a.dt)-1)))
  
  setkey(a.dt, Date)
  
  b.dt <- dst.dt[a.dt, nomatch=NA]
  setkey(b.dt, "DST Type")
  b.dt[J("Spring"), `3`:= mean(`2`, `4`), nomatch=0]
  b.dt[J("Fall"), `3`:= mean(`3`, `4`), nomatch=0]
  x <- b.dt[J("Fall"), 8:28, with=F] %>% as.numeric
  b.dt[J("Fall"), ':='(7:27, as.list(x))]
  b.dt[,`25`:=NULL]
  
  
  
  b.dt <- melt.data.table(a.dt, id.vars=c("Date", "RiskFactor"), variable.name="HE", value.name="Price", variable.factor=F)
  b.dt[,':='(HE.num=(as.numeric(HE)+1)/2, Date.Dt=as.IDate.f(Date))]
  b.dt[,HE:=NULL]
  setnames(b.dt, "HE.num", "HE")
  
  b.dt %>% setkey(HE)
  c.dt <- b.dt[J(1:24)]
  c.dt %>% setorder(RiskFactor, Date.Dt, HE)
  
  c.dt %>% setkey(Price)
  d.dt <- c.dt[!J(-999)]
  
  
  setkey(x.dt, `1`)
  y.dt <- x.dt[!J(-999999)]
  y.dt
}

proc.spot.pwr.2.fle.grp.f <- function(file.grp, by.l) {
  in.dt <- lapply(file.grp, read)
  
}


files.dt %>% setkey("Type")
frwd.dt <- files.dt[J("ForwardQuotes"),proc.frwd.file.grp.f(File.Name, .BY), by=RF]
spot.dt <- files.dt[J("History"),proc.spot.file.grp.f(File.Name, .BY), by=RF]


spot.dt <- files.dt[J("History"), proc.spot.file.grp.2.f(File.Name, QDate, .BY), by=RF]



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

# Part 5
setwd("C:/Temp/ZEMA/20150629a/Upload/")
a.dt <- fread("Gas_GD_MF__L_LD1_Hub_HH.csv", header=T)
a.dt[,':='(QuoteDate.Dt=as.IDate.f(QuoteDate), StartDate.Dt=as.IDate.f(StartDate), EndDate.Dt=as.IDate.f(EndDate))]
a.dt[,Daily:=ifelse(StartDate.Dt==EndDate.Dt, T, F)]
a.dt[,range(QuoteDate.Dt), by=Daily]

a <- ggplot(a.dt[,.N, by=c("QuoteDate.Dt", "Daily")], aes(x=QuoteDate.Dt, y=N, color=Daily)) + geom_point()
a
setkey(a.dt, Daily)
b <- ggplot(a.dt[J(FALSE),.N, by="QuoteDate.Dt"], aes(x=QuoteDate.Dt, y=N)) + geom_line()
b

setkey(a.dt, QuoteDate.Dt, Daily, StartDate.Dt)
a.dt[J(as.IDate.f("1/25/2010"),F)]
a.dt[J(as.IDate.f("8/1/2011"),F)]
a.dt[J(as.IDate.f("8/1/2012"),F)]
a.dt[J(as.IDate.f("6/10/2015"),F)]

a.dt[J(as.IDate.f("1/25/2010"),T)]
a.dt[J(as.IDate.f("8/1/2011"),T)]
a.dt[J(as.IDate.f("8/1/2012"),T)]
a.dt[J(as.IDate.f("6/10/2015"),T)]

