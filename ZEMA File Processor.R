


# ------------------------------------------------------------------------------------------------------------------------
# ---------- File Archiving ----------
# ------------------------------------------------------------------------------------------------------------------------
#.libPaths("C://R/Libraries//RRO_3.2.2")  # Default

library(lubridate)
library(data.table)
library(stringi)
library(magrittr)
library(tools)

options(data.table.nomatch=0)
options(stringsAsFactors=F)

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
  
  # Make Read-only
  files.sub.dt[1, Sys.chmod(stri_paste(Target.Dir, New.File.Name, sep="/"),
                            mode="0000",
                            use_umask=T)]
  
  # Delete All Files from Source
  # (Add code to do this ONLY if copy is successfull)
  files.sub.dt[,file.remove(File.Name)]
}

create.miss.dir.f <- . %>% {if(!file.exists(.)) dir.create(., recursive=T) else TRUE}

#proc.2.files.f <- function(files.sub.dt) {
#  my.sd.dt <- copy(files.sub.dt)
#  
#  if(my.sd.dt[,.N]>1) {
#    Copy.From <- my.sd.dt[1,stri_paste(Target.Dir, New.File.Name, sep="/")]
#    my.sd.dt[,Copy.To:=stri_paste(Target.Dir, New.File.Name, sep="/")]
#    my.sd.dt[2:.N, Sys.chmod(stri_paste(Target.Dir, New.File.Name, sep="/"),
#                                                       mode="0777"), by=New.File.Name]
#    my.sd.dt[2:.N,file.remove(Copy.To), by=New.File.Name]
#    my.sd.dt[2:.N, file.copy(Copy.From, Copy.To, overwrite=F, copy.date = T), by=New.File.Name]
#    my.sd.dt[2:.N, Sys.chmod(stri_paste(Target.Dir, New.File.Name, sep="/"),
#                             mode="0777"), by=New.File.Name]
#    
#    my.sd.dt[2:.N, Sys.setFileTime(Copy.To, Mod), by=New.File.Name]
#    my.sd.dt[2:.N, Sys.chmod(stri_paste(Target.Dir, New.File.Name, sep="/"),
#                              mode="0000",
#                              use_umask=T), by=New.File.Name]
#  }
#}


# ---------- Forwards ---------- 
# Set Source Directory
base.fwd.dir <- "D:/Lacima/ZEMA/Forward/Archive"
#base.fwd.dir <- "V:/ZEMA/Forward/Archive"
setwd(base.fwd.dir)

# Expected Forward Curve List
crvs.fwd.exp.dt <- fread("../Forward Curves.csv", header=T)

# Get Dirs & Files
dirs.fwd.dt <- data.table(Dirs=list.dirs())

files.fwd.1.dt <- data.table(File.Name=list.files(pattern="^ForwardQuotes.*csv$", recursive=FALSE))
files.fwd.1.dt[, ':='(Mod=file.mtime(File.Name), MD5Sum=md5sum(File.Name))]

# File Name Decomposition
files.fwd.1.dt[,':='(c("Curve","Profile SC", "Provider SC", "TariffType SC", "RunType SC", "RF", "Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), "")]

files.fwd.1.dt[,':='(c("Curve", "Profile SC", "Provider SC", "TariffType SC", "RunType SC", "RF", "Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), 
                     stri_match_all_regex(File.Name, 
                                          "ForwardQuotes_(([^_]*)_([^_]*)_([^_]*)_([^_]*)__(([^_]*)_([^_]*)_([^_]*_[^_]*_[^_]*)_([^_]*)_(.*?)))_(\\d{2}-\\d{2}-\\d{4})") %>%
                       transpose %>%
                       `[`(2:13))]

# Date Conversions
files.fwd.1.dt[,QDate.IDt:=as.IDate.mdY.2.f(QDate)]
files.fwd.1.dt[,Mod.IDt:=as.IDate.Ymd.2.f(Mod)]
files.fwd.1.dt[,Mod.ITime:=as.ITime(Mod)]

# Are there missing Curves?
setkey(files.fwd.1.dt, QDate.IDt)
crvs.fwd.miss.1.dt <- files.fwd.1.dt[J(files.fwd.1.dt[,unique(QDate.IDt)]),.SD[crvs.fwd.exp.dt, list(File.Name=File.Name, Curve=Curve), on="Curve"], by=.EACHI]
crvs.fwd.miss.2.dt <- crvs.fwd.miss.1.dt[is.na(File.Name), Curve, by=QDate.IDt]
crvs.fwd.miss.2.dt[, .N, by=QDate.IDt]

setkey(crvs.fwd.miss.2.dt, QDate.IDt)
crvs.fwd.miss.2.dt[J(files.fwd.1.dt[,max(QDate.IDt)]), Curve]

# Are there extra Curves?
setkey(files.fwd.1.dt, Curve)
setkey(crvs.fwd.exp.dt, Curve)
files.fwd.1.dt[!crvs.fwd.exp.dt, File.Name]

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

# Current files in those required directories (limited to most recent versions)
files.curr.fwd.1.dt <- rqrd.fwd.dirs.dt[,list(File.Name=list.files(Target.Dir, pattern="*.csv", full.names=T)), by=Target.Dir]
files.curr.fwd.1.dt[,MD5Sum:=md5sum(File.Name)]

if(files.curr.fwd.1.dt[,.N]!=0) {
  
  files.curr.fwd.1.dt[,':='(c("Curve", "QDate", "Mod.Date", "Mod.Time.ch"),
                            transpose(stri_match_all_regex(File.Name,
                                                           "(ForwardQuotes_.*?)_(\\d{2}-\\d{2}-\\d{4})_AsOf_(\\d{2}-\\d{2}-\\d{4})_(\\d{6})\\.csv"))[2:5])]
  
  files.curr.fwd.1.dt[,QDate.IDt:=as.IDate.mdY.2.f(QDate), by=QDate]
  files.curr.fwd.1.dt[,Mod.IDt:=as.IDate.mdY.2.f(Mod.Date), by=Mod.Date]
  files.curr.fwd.1.dt[,Mod.Time:=as.numeric(Mod.Time.ch)]
  files.curr.fwd.1.dt[,':='(QDate=NULL, Mod.Date=NULL, Mod.Time.ch=NULL)]
  
  setorderv(files.curr.fwd.1.dt, c("Curve", "QDate.IDt", "Mod.IDt", "Mod.Time"), c(1, 1, -1, -1))
  files.curr.fwd.2.dt <- files.curr.fwd.1.dt[,list(File.Name=File.Name[1], MD5Sum=MD5Sum[1]), by=list(Curve, QDate.IDt)]
  
  # Get set of new files as determined by MD5Sum, remove duplicates
  setkey(files.fwd.2.dt, MD5Sum)
  setkey(files.curr.fwd.2.dt, MD5Sum)
  files.fwd.3.dt <- files.fwd.2.dt[!files.curr.fwd.2.dt]
  
  dup.fwd.file.del.status <- files.fwd.2.dt[files.curr.fwd.2.dt, file.remove(File.Name)]
  
} else {
  #files.curr.fwd.2.dt <- copy(files.curr.fwd.1.dt)
  files.fwd.3.dt <- copy(files.fwd.2.dt)
}

# Adhoc test of file duplication
setkey(files.fwd.3.dt, RF, `Profile SC`, `Provider SC`, `TariffType SC`, `RunType SC`, QDate.IDt, Mod.IDt, Mod.ITime)
files.fwd.3.dt[,uniqueN(MD5Sum)]
files.fwd.3.dt[,uniqueN(MD5Sum), by=list(RF, `Profile SC`, `Provider SC`, `TariffType SC`, `RunType SC`, QDate.IDt)][,list(.N, sum(V1))]
files.fwd.3.dt[,uniqueN(MD5Sum), by=list(RF, `Profile SC`, `Provider SC`, `TariffType SC`, `RunType SC`, QDate.IDt, Mod.IDt)][,list(.N, sum(V1))]
files.fwd.3.dt[,uniqueN(MD5Sum), by=list(RF, `Profile SC`, `Provider SC`, `TariffType SC`, `RunType SC`, QDate.IDt, Mod.IDt, Mod.ITime)][,list(.N, sum(V1))]
files.fwd.3.dt[,.N, by=QDate.IDt]

# Process files
setkey(files.fwd.3.dt, MD5Sum, RF, `Profile SC`, `Provider SC`, `TariffType SC`, `RunType SC`, QDate.IDt, Mod.IDt, Mod.ITime)
new.fwd.file.status <- files.fwd.3.dt[,proc.files.f(.SD), by=MD5Sum]

# ---------- Spot ----------
# Set Source Directory
base.sp.dir <- "D:/Lacima/ZEMA/Spot/Archive"
#base.sp.dir <- "V:/ZEMA/Spot/Archive"
setwd(base.sp.dir)

# Expected Spot Curve List
crvs.sp.exp.dt <- fread("../Spot Curves.csv", header=T)

# Get Dirs & Files
dirs.sp.dt <- data.table(Dirs=list.dirs())

files.sp.1.dt <- data.table(File.Name=list.files(pattern="^History_Spot.*csv$", recursive=FALSE))
files.sp.1.dt[, ':='(Mod=file.mtime(File.Name), MD5Sum=md5sum(File.Name))]

# File Name Decomposition 
files.sp.1.dt[,':='(c("Curve", "Profile SC", "Provider SC", "TariffType SC", "RunType SC", "RF", "Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), "")]

files.sp.1.dt[,':='(c("Curve", "Profile SC","TariffType SC", "RunType SC", "RF", "Cmmdty", "Spot", "Model", "Basis", "Loc", "QDate"), 
                    stri_match_all_regex(File.Name, 
                                         "History_Spot_(([^_]*)_([^_]*)_([^_]*)__(([^_]*)_([^_]*)_([^_]*_[^_]*_[^_]*)_([^_]*)_(.*?)))_(\\d{2}-\\d{2}-\\d{4})") %>%
                      transpose %>%
                      `[`(2:12))]

# Date Conversions
files.sp.1.dt[,QDate.IDt:=as.IDate.mdY.2.f(QDate)]
files.sp.1.dt[,Mod.IDt:=as.IDate.Ymd.2.f(Mod)]
files.sp.1.dt[,Mod.ITime:=as.ITime(Mod)]

# Are we missing Curves?
setkey(files.sp.1.dt, QDate.IDt)
crvs.sp.miss.1.dt <- files.sp.1.dt[J(files.sp.1.dt[,unique(QDate.IDt)]),.SD[crvs.sp.exp.dt, list(File.Name=File.Name, Curve=Curve), on="Curve"], by=.EACHI]
crvs.sp.miss.2.dt <- crvs.sp.miss.1.dt[is.na(File.Name), Curve, by=QDate.IDt]
crvs.sp.miss.2.dt[, .N, by=QDate.IDt]

setkey(crvs.sp.miss.2.dt, QDate.IDt)
crvs.sp.miss.2.dt[J(files.sp.1.dt[,max(QDate.IDt)]), Curve]

# Are there any extra Curves?
files.sp.1.dt[!crvs.sp.exp.dt, File.Name, on="Curve"]

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
files.curr.sp.1.dt <- rqrd.sp.dirs.dt[,list(File.Name=list.files(Target.Dir, pattern="*.csv", full.names=T)), by=Target.Dir]
files.curr.sp.1.dt[,MD5Sum:=md5sum(File.Name)]

# Remove files from processing that are currently NOT unique
if(files.curr.sp.1.dt[,.N]!=0) {
  files.curr.sp.1.dt[,':='(c("Curve", "QDate", "Mod.Date", "Mod.Time.ch"),
                           transpose(stri_match_all_regex(File.Name,
                                                          "(History_Spot_.*?)_(\\d{2}-\\d{2}-\\d{4})_AsOf_(\\d{2}-\\d{2}-\\d{4})_(\\d{6})\\.csv"))[2:5])]
  
  files.curr.sp.1.dt[,QDate.IDt:=as.IDate.mdY.2.f(QDate), by=QDate]
  files.curr.sp.1.dt[,Mod.IDt:=as.IDate.mdY.2.f(Mod.Date), by=Mod.Date]
  files.curr.sp.1.dt[,Mod.Time:=as.numeric(Mod.Time.ch)]
  files.curr.sp.1.dt[,':='(QDate=NULL, Mod.Date=NULL, Mod.Time.ch=NULL)]
  
  # There may be multiple versions of any curve in the Archive as the file gets updated.  Since we want to have the latest files accurately reflect what's in
  # Lacima, only consider the most recent versions for purposes of determining uniqueness.
  setorderv(files.curr.sp.1.dt, c("Curve", "QDate.IDt", "Mod.IDt", "Mod.Time"), c(1, 1, -1, -1))
  files.curr.sp.2.dt <- files.curr.sp.1.dt[,list(File.Name=File.Name[1], MD5Sum=MD5Sum[1]), by=list(Curve, QDate.IDt)]
  
  # Get set of new files as determined by MD5Sum & QDate, remove duplicates
  # (In some rare cases, e.g. IF RFs, we can actually have identical files for different reference dates)
  setkey(files.sp.2.dt, QDate.IDt, MD5Sum)
  setkey(files.curr.sp.2.dt, QDate.IDt, MD5Sum)
  files.sp.3.dt <- files.sp.2.dt[!files.curr.sp.2.dt]
  
  dup.sp.file.del.status <- files.sp.2.dt[files.curr.sp.2.dt, file.remove(File.Name)]
  
} else {
  #files.curr.sp.2.dt <- copy(files.curr.sp.1.dt)
  #files.curr.sp.2.dt[,QDate.Idt:=as.IDate.mdY.1.f(character(0))]
  files.sp.3.dt <- copy(files.sp.2.dt)
}

# Adhoc test of file duplication
setkey(files.sp.3.dt, RF, `Profile SC`, `TariffType SC`, `RunType SC`, QDate.IDt, Mod.IDt, Mod.ITime)
files.sp.3.dt[,uniqueN(MD5Sum)]

# Process files
setkey(files.sp.3.dt, MD5Sum, RF, `Profile SC`, `TariffType SC`, `RunType SC`, QDate.IDt, Mod.IDt, Mod.ITime)
new.sp.file.status <- files.sp.3.dt[,proc.files.f(.SD), by=list(QDate.IDt, MD5Sum)]
