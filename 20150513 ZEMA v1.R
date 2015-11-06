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

as.IDate.1.f <- . %>% as.IDate(., "%m/%d/%Y", tz='GMT')
as.IDate.2.f <- . %>% as.IDate(., "%m-%d-%Y", tz='GMT')
as.IDate.3.f <- . %>% as.IDate(., "%Y-%m-%d", tz='GMT')

# DST
dst.dt <- fread("C:/Temp/DST Start End v2.csv", header=T)
dst.dt[,Date.Dt:=as.IDate.f(Date)]
dst.dt[,Date:=NULL]
setnames(dst.dt, "Date.Dt", "Date")
setcolorder(dst.dt, c("Date","DST Type"))
setkey(dst.dt, "Date")


setwd("C:/")
base.dir <- c("C:/Temp/ZEMA/20150810a/")
#sub.dirs <- c("Power 02 Spot NE", "Power 02 Spot NY", "Power 02 Spot PJM")
#sub.dirs <- c("20150618a/PJM West Monthly", "20150622a/PJM West Daily", "20150525a/20150521")
sub.dirs <- c("Upload")
out.dir <- stri_paste(base.dir, "Upload2/")

full.dirs <- sapply(sub.dirs, . %>% stri_paste(base.dir, ., sep=""))
files.dt <- lapply(full.dirs, function(x) data.table(File.Name=list.files(path=x, full.names=T, recursive = F, include.dirs=F))) %>% 
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
  
  # Run Date
  RunDate.IDt <- stri_match_all_regex(file.x, "_(\\d{2}-\\d{2}-\\d{4}).csv")[[1]][2] %>% 
                 as.IDate.2.f
  
  # Import
  a.dt <- read.csv(file.x, sep=",", header=F, skip=1L, colClasses=c(rep("character", 2), rep("numeric", 50))) %>%
          as.data.table
  
  # Remove Duplicate Column
  a.dt[,':='(seq(4, 52, 2), NULL)]
  setnames(a.dt, c("Date", "RiskFactor", 1:25))
  
  # Add Date Column & Key By
  a.dt[,Date.Dt:=as.IDate.f(Date)]
  a.dt[,Date:=NULL]
  setnames(a.dt, "Date.Dt", "Date")
  setcolorder(a.dt, c(27, 1:26))
  setkey(a.dt, Date)
 
  # Adjust DST Dates
  b.dt <- dst.dt[a.dt, nomatch=NA]
  setkey(b.dt, "DST Type")
  b.dt[J("Spring"), `3`:= mean(c(`2`, `4`)), nomatch=0]
  b.dt[J("Fall"), `2`:= mean(c(`2`, `3`)), nomatch=0]
  x <- b.dt[J("Fall"), 7:28, with=F] %>% as.numeric     # \ Shift HE4-HE25 left 1 hour to HE3-HE24
  b.dt[J("Fall"), ':='(6:27, as.list(x))]               # /
  b.dt[,':='(`DST Type`=NULL, `25`=NULL)]

  # Add Run Date
  b.dt[,"Run Date":=RunDate.IDt]
  setcolorder(b.dt, c(27, 1:26))
  
  # Return
  b.dt
}

proc.spot.pwr.2.file.grp.f <- function(file.grp, by.l) {
  require(data.table)
  require(magrittr)
  
  # Import Files
  in.dt <- lapply(file.grp, read.spot.pwr.2.prc.f) %>% rbindlist(use.names=T)
  
  # Calculate Filter
  setkey(in.dt, "Run Date", Date)
  a.dt <- in.dt[, list('Run Date'=max(`Run Date`)), by=Date]
  setkey(a.dt, "Run Date", Date)
  
  # Execute Filter
  b.dt <- in.dt[a.dt]
  b.dt[,"Run Date":=NULL]
  setkey(b.dt, "Date")
  
  # Remove All Dates with No Data (-999999)
  #b.dt[,Keep:=ifelse(`1`==`2` & `2`==`3` & `3`==`4` & `4`==`5` & `5`==`6` & `6`==`7` & 
  #                     `7`==`8` & `8`==`9` & `9`==`10` & `10`==`11` & `11`==`12` & `12`==`13` &
  #                     `13`==`14` & `14`==`15` & `15`==`16` & `16`==`17` & `17`==`18` & `18`==`19` &
  #                     `19`==`20` & `20`==`21` & `21`==`22` & `22`==`23` & `23`==`24` & `24`==-999999,
  #                   F,T)]
  c.dt <- melt(b.dt, id.vars = c("Date", "RiskFactor"), variable.name = "HE", variable.factor = F, value.name = "Price")
  d.dt <- c.dt[,Keep:=sum(Price==-999999)!=24, keyby="Date"]
  setkey(d.dt, Keep)
  e.dt <- d.dt[J(T)]
  f.dt <- dcast(e.dt, Date + RiskFactor ~ HE, value.var = "Price")
  setcolorder(f.dt, c("Date", "RiskFactor", 1:24))
  
  # Get Count Info for Debug
  max.cnt <- f.dt[,.N, by=Date][,max(N)]
  
  # Output
  setcolorder(f.dt, c("Date", "RiskFactor", 1:24))
  write.csv(f.dt, stri_paste(out.dir, "History_Spot_", by.l$RF, ".csv", sep=""), quote=F, row.names=F)
  
  # Return Debug Info
  max.cnt
}


files.dt %>% setkey("Type")
frwd.dt <- files.dt[J("ForwardQuotes"),proc.frwd.file.grp.f(File.Name, .BY), by=RF]
spot.dt <- files.dt[J("History"),proc.spot.file.grp.f(File.Name, .BY), by=RF]

spot.dt <- files.dt[J("History"), proc.spot.pwr.2.file.grp.f(File.Name, .BY), by=RF]



read.spot.gas.prc.f <- function(file.x) {
  
  # Run Date
  RunDate.Dt <- stri_match_all_regex(file.x, "_(\\d{2}-\\d{2}-\\d{4}).csv")[[1]][2] %>% as.IDate.2.f
  
  # Import
  prc.dt <- fread(file.x, header=T)
  prc.dt[,RunDate.Dt:=RunDate.Dt]
  
  prc.dt
}





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



t.2 <- readr::read_csv(file.x, col_types=list(Date=col_character(),RiskFactor=col_character(), `1`=col_character(),`2`=col_character(),`3`=col_character(),`4`=col_character(),`5`=col_character(),`6`=col_character(),`7`=col_character(),`8`=col_character(),`9`=col_character(),`10`=col_character(),`11`=col_character(),`12`=col_character(),`13`=col_character(),`14`=col_character(),`15`=col_character(),`16`=col_character(),`17`=col_character(),`18`=col_character(),`19`=col_character(),`20`=col_character(),`21`=col_character(),`22`=col_character(),`23`=col_character(),`24`=col_character(),`25`=col_character(),`26`=col_character(),`27`=col_character(),`28`=col_character(),`29`=col_character(),`30`=col_character(),`31`=col_character(),`32`=col_character(),`33`=col_character(),`34`=col_character(),`35`=col_character(),`36`=col_character(),`37`=col_character(),`38`=col_character(),`39`=col_character(),`40`=col_character(),`41`=col_character(),`42`=col_character(),`43`=col_character(),`44`=col_character(),`45`=col_character(),`46`=col_character(),`47`=col_character(),`48`=col_character(),`49`=col_character(),`50`=col_character()), skip=1)
t.2 <- readr::read_csv(file.x, col_types=list(col_character()=1:52), skip=1)
t.2 <- read.csv(file.x, header=F, sep=",", skip=1, colClasses=c("character"))
t.2 <- read.csv(file.x, header=F, sep=",", skip=1, colClasses=c("character", "character", rep("numeric", 50)))
t.2 <- read.csv(file.x, header=F, sep=",", skip=1, colClasses=c(Date="character", RiskFactor="character", rep("numeric", 50)))

t.3 <- read.csv(file.y, header=F, sep=",", skip=1, colClasses=c("character"))

t.3 <- fread(file.x, sep=",", header=F, skip=1L, colClasses = list(character=1:54))
t.4 <- readr::read_csv(file.x, col_names=F, skip = 1, na=c("0.00"))


# Data checks
setwd("C:/Temp/ZEMA/20150713a/Upload")
getwd()
a.dt <- fread("ForwardQuotes_Gas_GD_Sum_1_M_IF_Transco_Zn_6_NY.csv", header=T)
b.dt <- fread("ForwardQuotes_Gas_GD_Sum_1_S_IF_Transco_Zn_3.csv", header=T)
a.dt[,QuoteDt.Dt:=as.IDate.f(QuoteDate)]
b.dt[,QuoteDt.Dt:=as.IDate.f(QuoteDate)]
a.dt %>% setkey(QuoteDt.Dt)
b.dt %>% setkey(QuoteDt.Dt)

a.dt[,.N] - b.dt[,.N]

b.dt[,range(QuoteDt.Dt)]
b.dt[,length(unique(QuoteDt.Dt))]

a.QDt.dt <- a.dt[,unique(QuoteDt.Dt)] %>% as.data.table %>% setnames("QuoteDt.Dt") %>% setkey(QuoteDt.Dt)
b.QDt.dt <- b.dt[,unique(QuoteDt.Dt)] %>% as.data.table %>% setnames("QuoteDt.Dt") %>% setkey(QuoteDt.Dt)

a.QDt.dt[!b.QDt.dt]
b.QDt.dt[!a.QDt.dt]

QDt.inter <- intersect(a.dt[,unique(QuoteDate)], b.dt[,unique(QuoteDate)])
QDt.inter.Dt <- QDt.inter %>% as.IDate.f
QDt.diff.1 <- setdiff(a.dt[,unique(QuoteDate)], b.dt[,unique(QuoteDate)])
QDt.diff.1.Dt <- QDt.diff.1 %>% as.IDate.f %T>% print
QDt.diff.2 <- setdiff(b.dt[,unique(QuoteDate)], a.dt[,unique(QuoteDate)])
QDt.diff.2.Dt <- QDt.diff.2 %>% as.IDate.f %T>% print

QDt.int.diff.1.1 <- setdiff(QDt.inter, a.dt[,unique(QuoteDate)]) %>% as.IDate.f %T>% print
QDt.int.diff.1.2 <- setdiff(a.dt[,unique(QuoteDate)], QDt.inter) %>% as.IDate.f %T>% print

QDt.int.diff.2.1 <- setdiff(QDt.inter, b.dt[,unique(QuoteDate)]) %>% as.IDate.f %T>% print
QDt.int.diff.2.2 <- setdiff(b.dt[,unique(QuoteDate)], QDt.inter) %>% as.IDate.f %T>% print

QDt.inter.Dt.dt <- QDt.inter.Dt %>% as.data.table %>% setnames("QuoteDt.Dt") %>% setkey(QuoteDt.Dt)
a.2.dt <- a.dt[QDt.inter.Dt.dt, nomatch=0]
b.2.dt <- b.dt[QDt.inter.Dt.dt, nomatch=0]

a.2.dt[,.N] - b.2.dt[,.N]

a.2.dt[,RiskFactor:=NULL]
b.2.dt[,RiskFactor:=NULL]
identical(a.2.dt, b.2.dt)

# ------------------------------------------------------------------------------------------------------------------------
# File Archiving
# ------------------------------------------------------------------------------------------------------------------------
library(lubridate)
library(data.table)
library(stringi)
library(magrittr)
library(tools)

# ZEMA files have tz="US/Pacific", but we want to convert to EST
Sys.setenv(TZ='America/New_York')

as.IDate.mdY.1.f <- . %>% as.IDate(., "%m/%d/%Y", tz="America/New_York")
as.IDate.mdY.2.f <- . %>% as.IDate(., "%m-%d-%Y", tz="America/New_York")

as.IDate.Ymd.1.f <- . %>% as.IDate(., "%Y/%m/%d", tz="America/New_York")
as.IDate.Ymd.2.f <- . %>% as.IDate(., "%Y-%m-%d", tz="America/New_York")

proc.files.f <- function(files.sub.dt) {
  require(stringi)
  
  # Copy 1st File to Target
  copy.to.file.name <- files.sub.dt[1, file.copy(File.Name,
                                                 stri_paste(Target.Dir, New.File.Name, sep="/"),
                                                 overwrite=F,
                                                 copy.date=T)]
  
  # Delete All Files from Source
  # (Add code to do this ONLY if copy is successfull)
  files.sub.dt[,file.remove(File.Name)]
}

create.miss.dir.f <- . %>% {if(!file.exists(.)) dir.create(., recursive=T) else TRUE}

# ---------- Forwards ---------- 
# Set Source Directory
base.fwd.dir <- "W:/ZEMA/Forward/Archive"
setwd(base.fwd.dir)

# Get Dirs & Files
dirs.fwd.dt <- data.table(Dirs=list.dirs())

files.fwd.1.dt <- data.table(File.Name=list.files(pattern="^ForwardQuotes.*csv$", recursive=FALSE))
files.fwd.1.dt[, ':='(Mod=file.mtime(File.Name), MD5Sum=md5sum(File.Name))]

# File Name Decomposition
files.fwd.1.dt[,':='(c("Profile SC", "Provider SC", "TariffType SC", "RunType SC", "Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), "")]

files.fwd.1.dt[,':='(c("Profile SC", "Provider SC", "TariffType SC", "RunType SC", "Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), 
                     stri_match_all_regex(File.Name, 
                                          "ForwardQuotes_([^_]*)_([^_]*)_([^_]*)_([^_]*)__([^_]*)_([^_]*)_([^_]*_[^_]*_[^_]*)_([^_]*)_(.*?)_(\\d{2}-\\d{2}-\\d{4})")[[1]][2:11] %>% as.list),
                by=File.Name]

files.fwd.1.dt[,RF:=stri_paste(Cmmdty, Spot, Model, Basis, Loc, sep="_")]
setkey(files.fwd.1.dt, RF)

# Date Conversions
files.fwd.1.dt[,QDate.IDt:=as.IDate.mdY.2.f(QDate)]
files.fwd.1.dt[,Mod.IDt:=as.IDate.Ymd.2.f(Mod)]
files.fwd.1.dt[,Mod.ITime:=as.ITime(Mod)]

# What directories are required?
setkey(files.fwd.1.dt, QDate.IDt)
rqrd.fwd.dirs.dt <- files.fwd.1.dt[,list(QDate.IDt=unique(QDate.IDt))]
rqrd.fwd.dirs.dt[,Target.Dir:=stri_paste(rep(base.fwd.dir, .N),
                                         strftime(QDate.IDt, "%Y"),
                                         strftime(QDate.IDt, "%m"),
                                         strftime(QDate.IDt, "%Y_%m_%d"),
                                         sep="/")]

# Create any required directories
rqrd.fwd.dirs.dt[,create.miss.dir.f(Target.Dir), by=Target.Dir]

# Join Target Dir
setkey(files.fwd.1.dt, QDate.IDt)
setkey(rqrd.fwd.dirs.dt, QDate.IDt)
files.fwd.2.dt <- rqrd.fwd.dirs.dt[files.fwd.1.dt, nomatch=0]

# Calculate Target File
files.fwd.2.dt[,New.File.Name:=stri_paste(stri_paste("ForwardQuotes",
                                                     `Profile SC`, 
                                                     `Provider SC`, 
                                                     `TariffType SC`, 
                                                     `RunType SC`,
                                                     "", 
                                                     RF, 
                                                     strftime(QDate.IDt, "%m-%d-%Y"),
                                                     "AsOf",
                                                     strftime(Mod.IDt, "%m-%d-%Y"),
                                                     strftime(Mod.ITime, "%H%M%S"),
                                                     sep="_"),
                                          ".csv",
                                          sep="")]

# Current files in those required directories
curr.fwd.files.dt <- rqrd.fwd.dirs.dt[,list(File.Name=list.files(Target.Dir, pattern="*.csv", full.names=T)), by=Target.Dir]
curr.fwd.files.dt[,MD5Sum:=md5sum(File.Name)]

# Get set of new files as determined by MD5Sum, remove duplicates
setkey(files.fwd.2.dt, MD5Sum)
setkey(curr.fwd.files.dt, MD5Sum)
files.fwd.3.dt <- files.fwd.2.dt[!curr.fwd.files.dt]

dup.fwd.file.del.status <- files.fwd.2.dt[curr.fwd.files.dt, file.remove(File.Name)]

# Adhoc test of file duplication
setkey(files.fwd.3.dt, RF, `Profile SC`, `Provider SC`, `TariffType SC`, `RunType SC`, QDate.IDt, Mod.IDt, Mod.ITime)
files.fwd.3.dt[,uniqueN(MD5Sum)]
files.fwd.3.dt[,uniqueN(MD5Sum), by=list(RF, `Profile SC`, `Provider SC`, `TariffType SC`, `RunType SC`, QDate.IDt)][,list(.N, sum(V1))]
files.fwd.3.dt[,uniqueN(MD5Sum), by=list(RF, `Profile SC`, `Provider SC`, `TariffType SC`, `RunType SC`, QDate.IDt, Mod.IDt)][,list(.N, sum(V1))]
files.fwd.3.dt[,uniqueN(MD5Sum), by=list(RF, `Profile SC`, `Provider SC`, `TariffType SC`, `RunType SC`, QDate.IDt, Mod.IDt, Mod.ITime)][,list(.N, sum(V1))]

# Process files
setkey(files.fwd.3.dt, MD5Sum, RF, `Profile SC`, `Provider SC`, `TariffType SC`, `RunType SC`, QDate.IDt, Mod.IDt, Mod.ITime)
new.fwd.file.status <- files.fwd.3.dt[,proc.files.f(.SD), by=MD5Sum]

# ---------- Spot ----------
# Set Source Directory
base.sp.dir <- "W:/ZEMA/Spot/Archive"
setwd(base.sp.dir)

# Get Dirs & Files
dirs.sp.dt <- data.table(Dirs=list.dirs())

files.sp.1.dt <- data.table(File.Name=list.files(pattern="^History_Spot.*csv$", recursive=FALSE))
files.sp.1.dt[, ':='(Mod=file.mtime(File.Name), MD5Sum=md5sum(File.Name))]

# File Name Decomposition 
files.sp.1.dt[,':='(c("Profile SC", "Provider SC", "TariffType SC", "RunType SC", "Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), "")]

files.sp.1.dt[,
               ':='(c("Profile SC","TariffType SC", "RunType SC", "Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), 
                    stri_match_all_regex(File.Name, 
                                         "History_Spot_([^_]*)_([^_]*)_([^_]*)__([^_]*)_([^_]*)_([^_]*_[^_]*_[^_]*)_([^_]*)_(.*?)_(\\d{2}-\\d{2}-\\d{4})")[[1]][2:10] %>% as.list),
               by=File.Name]

files.sp.1.dt[,RF:=stri_paste(Cmmdty, Spot, Model, Basis, Loc, sep="_")]
setkey(files.sp.1.dt, RF)

# Date Conversions
files.sp.1.dt[,QDate.IDt:=as.IDate.mdY.2.f(QDate)]
files.sp.1.dt[,Mod.IDt:=as.IDate.Ymd.2.f(Mod)]
files.sp.1.dt[,Mod.ITime:=as.ITime(Mod)]

# What directories are required?
setkey(files.sp.1.dt, QDate.IDt)
rqrd.sp.dirs.dt <- files.sp.1.dt[,list(QDate.IDt=unique(QDate.IDt))]
rqrd.sp.dirs.dt[,Target.Dir:=stri_paste(rep(base.sp.dir, .N),
                                        strftime(QDate.IDt, "%Y"),
                                        strftime(QDate.IDt, "%m"),
                                        strftime(QDate.IDt, "%Y_%m_%d"),
                                        sep="/")]

# Create any required directories
rqrd.sp.dirs.dt[,create.miss.dir.f(Target.Dir), by=Target.Dir]

# Join Target Dir
setkey(files.sp.1.dt, QDate.IDt)
setkey(rqrd.sp.dirs.dt, QDate.IDt)
files.sp.2.dt <- rqrd.sp.dirs.dt[files.sp.1.dt, nomatch=0]

# Calculate Target File
files.sp.2.dt[,New.File.Name:=stri_paste(stri_paste("History_Spot",
                                                    `Profile SC`,
                                                    `TariffType SC`,
                                                    `RunType SC`,
                                                    "",
                                                    RF,
                                                    strftime(QDate.IDt, "%m-%d-%Y"),
                                                    "AsOf",
                                                    strftime(Mod.IDt, "%m-%d-%Y"),
                                                    strftime(Mod.ITime, "%H%M%S"),
                                                    sep="_"),
                                         ".csv",
                                         sep="")]

# Current files in those required directories
curr.sp.files.dt <- rqrd.sp.dirs.dt[,list(File.Name=list.files(Target.Dir, pattern="*.csv", full.names=T)), by=Target.Dir]
curr.sp.files.dt[,MD5Sum:=md5sum(File.Name)]

# Get set of new files as determined by MD5Sum, remove duplicates
setkey(files.sp.2.dt, MD5Sum)
setkey(curr.sp.files.dt, MD5Sum)
files.sp.3.dt <- files.sp.2.dt[!curr.sp.files.dt]

dup.sp.file.del.status <- files.sp.2.dt[curr.sp.files.dt, file.remove(File.Name)]

# Adhoc test of file duplication
setkey(files.sp.3.dt, RF, `Profile SC`, `TariffType SC`, `RunType SC`, QDate.IDt, Mod.IDt, Mod.ITime)
files.sp.3.dt[,uniqueN(MD5Sum)]

# Process files
setkey(files.sp.3.dt, MD5Sum, RF, `Profile SC`, `TariffType SC`, `RunType SC`, QDate.IDt, Mod.IDt, Mod.ITime)
new.sp.file.status <- files.sp.3.dt[,proc.files.f(.SD), by=MD5Sum]

# ------------------------------------------------------------------------------------------------------------------------
# Missing Files
# ------------------------------------------------------------------------------------------------------------------------
library(lubridate)
library(data.table)
library(stringi)
library(magrittr)
library(tools)

# ZEMA files have tz="US/Pacific", but we want to convert to EST
Sys.setenv(TZ='America/New_York')

as.IDate.mdY.1.f <- . %>% as.IDate(., "%m/%d/%Y", tz="America/New_York")
as.IDate.mdY.2.f <- . %>% as.IDate(., "%m-%d-%Y", tz="America/New_York")

as.IDate.Ymd.1.f <- . %>% as.IDate(., "%Y/%m/%d", tz="America/New_York")
as.IDate.Ymd.2.f <- . %>% as.IDate(., "%Y-%m-%d", tz="America/New_York")

# ---------- Forwards ---------- 
# Set Source Directory
ref.dir <- "W:/ZEMA/Forward/Archive/2015/09/2015_09_02"
comp.dir <- "W:/ZEMA/Forward/Archive/2015/10/2015_10_26"

# Get Files
get.files.f <- . %>% list.files(path=., pattern="^ForwardQuotes.*csv$", recursive=FALSE) %>% 
                     data.table() %>% 
                     setnames(old=".", new="File.Name")

files.ref.dt <- get.files.f(ref.dir)
files.comp.dt <- get.files.f(comp.dir)

files.dt <- rbindlist(list(files.ref.dt, files.comp.dt), use.names=T, idcol="Src")

files.dt <- data.table(File.Name=list.files(path=c(ref.dir, comp.dir), pattern="^ForwardQuotes.*csv$", recursive=FALSE, full.names=T))

# File Name Decomposition
files.dt[,':='(c("Profile SC", "Provider SC", "TariffType SC", "RunType SC", "Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate", "MDate", "MTime"), "")]

files.dt[,':='(c("Profile SC", "Provider SC", "TariffType SC", "RunType SC", "Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate", "MDate", "MTime"), 
               stri_match_all_regex(File.Name, 
                                    "ForwardQuotes_([^_]*)_([^_]*)_([^_]*)_([^_]*)__([^_]*)_([^_]*)_([^_]*_[^_]*_[^_]*)_([^_]*)_(.*?)_(\\d{2}-\\d{2}-\\d{4})_AsOf_(\\d{2}-\\d{2}-\\d{4})_(\\d{6})")[[1]][2:13] %>% as.list),
         by=File.Name]

files.dt[,RF:=stri_paste(Cmmdty, Spot, Model, Basis, Loc, sep="_")]
setkey(files.dt, RF)

# Date Conversions
files.dt[,QDate.IDt:=as.IDate.mdY.2.f(QDate)]
files.dt[,Mod.IDt:=as.IDate.Ymd.2.f(Mod)]
files.dt[,Mod.ITime:=as.ITime(Mod)]
