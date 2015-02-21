# Matthew Pillmeier
# 2/19/2015
# Test Fitbit Scraper package

library(fitbitScraper)
library(data.table)
library(magrittr)
library(RCurl)
library(httr)

set_config( config( ssl.verifypeer = 0L ) )   # Ignores the SSL check
Sys.setenv(TZ='GMT')

fb.email <- ""
fb.pwd <- ""

cookie <- login(email=fb.email, password=fb.pwd)

# 15_min_data "what" options: "steps", "distance", "floors", "active-minutes", "calories-burned"   
fb.15.dt <- get_15_min_data(cookie, what="calories-burned", date="2015-02-20")
setDT(fb.15.dt)


# Daily Data
fb.dly.cat.ls <- list("caloriesBurnedVsIntake", "minutesVery", "steps", "distance", "floors")
fb.dly.st.date <- "2015-01-18"
fb.dly.end.date <- "2015-02-18"

fb.get.dly.f <- function(fb.cat) {
  # D/L data and put into a "melted" format so that multiple category data can be rbind'd
  
  dt <- get_daily_data(cookie, what=fb.cat, start_date="2015-01-18", end_date="2015-02-18")
  setDT(dt)
  
  dt[,Date:=as.IDate(time, "%Y-%m-%d", tz="GMT")]
  dt[,time:=NULL]
  dt[,":="(Variable, fb.cat)]   # Add a column "Variable" set to the name of the category, used 
  setkey(dt, Date)
  
  names(dt)[chmatch(fb.cat, names(dt))] <- "Value"    # Rename the category column (containing d/l'd data) to "Value"
  
  dt
}

fb.dly.dt <- lapply(fb.dly.cat.ls, fb.get.dly.f) %>% rbindlist
fb.dly.cast.dt <- dcast(fb.dly.dt, Date ~ Variable, value.var = "Value")

# ggplot2
p.1 <- ggplot(fb.dly.cast.dt, aes(x=Date, y=caloriesBurnedVsIntake)) 
p.1 <- p.1 + geom_line()
p.1 <- p.1 + geom_point()
p.1

# ggvis
fb.dly.cast.dt %>% ggvis(~Date, ~caloriesBurnedVsIntake) %>% layer_points() %>% layer_lines()

# GoogleVis
p.3 <- gvisScatterChart(fb.dly.cast.dt[,list(as.numeric(Date), caloriesBurnedVsIntake)], 
                        options=list(
                          legend="none"))
plot(p.3)


fb.get.dly.f <- function(cookie, )
