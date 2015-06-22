library(data.table)
library(magrittr)
library(RODBC)

source("C:/GitHub/Misc-R-Code/Secure v1.R")

lac.odbc <- odbcConnect("Lacima_Dev", uid = ps.login, pwd=ps.pwd)

lac.tbls <- sqlTables(lac.odbc)
lac.tbls.dt <- as.data.table(lac.tbls)
lac.tbls.dt %>% setkey("TABLE_SCHEM")
lac.tbls.dt[J("dbo"),.N]

lac.ear.out <- sqlQuery(lac.odbc, "select * from raEarningsAtRiskOutput") %>% as.data.table
lac.ear.out %>% setkey("EarValue")
lac.ear.out[!J(NA)] %>% head(10)
lac.ear.out %>% str
