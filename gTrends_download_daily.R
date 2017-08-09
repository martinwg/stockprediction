#Initialization*****************************************************************
rm(list = ls()) # clear environment
cat("\014") # clear console

library(gtrendsR)
library(lubridate)

user <- "au.stock.research@gmail.com"
psw <- "Auburn University"
gconnect(user, psw)

#Initialize

start_date <- "2013-01-01"
end_date <- "2017-01-01"

search_term <- "Apple"

#Download the data
year.start <- year(start_date)
year.end <- year(end_date)-1

years <- seq(from=year.start, to=year.end)
months <- c(1,4,7,10)
#months <- formatC(months,width = 2,flag = "0")

# years <- as.character(years)
# months <- as.character(months)

date.generator <- function(year,month){
     month = formatC(month,width = 2,flag = "0")
     year = as.character(year)
     month = as.character(month)
     date = paste0(year,'-',month,'-','01')
     return(date)
}

res.daily <- list()
counter <- 1

for(year in years){
     for(month in months){
          start_date_daily = date.generator(year,month)
          month_end = as.numeric(month)+3
          if(month_end == 13){
               year = as.numeric(year)+1
               month_end = 1
          }
          end_date_daily = date.generator(year,month_end)
          end_date_daily = as.character(as.Date(end_date_daily)-1)
          res.daily[[counter]] = gtrends(search_term, geo = ("US"), 
                                         start_date = start_date_daily, 
                                         end_date = end_date_daily)$trend
          counter = counter +1
          print(paste(start_date_daily,end_date_daily, collapse = '>>>'))
          Sys.sleep(5)  #Don't know the minimum Google allowed, 5 works
     }
}


df.daily <- do.call("rbind", res.daily)

df.weekly <- gtrends(search_term, geo = ("US"), start_date = start_date, 
                     end_date = end_date)$trend

#An sample to test the download works

# library(gtrendsR)
# user <- "au.stock.research@gmail.com"
# psw <- "Auburn University"
# gconnect(user, psw)

# a <- gtrends("Aflac Insurance", geo = ("US"), start_date = "2016-01-01", 
#         end_date = "2017-01-01")$trend
# 
# b1 <- gtrends("Aflac Insurance", geo = ("US"), start_date = "2013-01-01", 
#              end_date = "2013-03-31")$trend
# b2 <- gtrends("Aflac Insurance", geo = ("US"), start_date = "2013-04-01", 
#               end_date = "2013-05-31")$trend
# b3 <- gtrends("Aflac Insurance", geo = ("US"), start_date = "2013-07-01", 
#               end_date = "2013-09-30")$trend
# b4 <- gtrends("Aflac Insurance", geo = ("US"), start_date = "2013-10-01", 
#               end_date = "2013-12-31")$trend
# 
# b <- rbind(b1,b2,b3,b4)

#Merge weekly and daily table and calculate the adjustment factor

df.merged <- merge(df.daily,df.weekly, by="start", all.x = T)
df.merged$adjustment_factor <- df.merged$hits.y/df.merged$hits.x

#Fill the adjustment factor to each day

for(i in 2:nrow(df.merged)){
     if(!is.finite(df.merged$adjustment_factor[i])){
          df.merged$adjustment_factor[i] = df.merged$adjustment_factor[i-1]
     }
}

#Remove rows without adjust factor
nonNa.factor <- which(!is.na(df.merged$adjustment_factor))
last.Na <- min(nonNa.factor) -1
df.merged <- tail(df.merged, -last.Na)

#Get the daily data
df.merged$Daily <- df.merged$adjustment_factor*df.merged$hits.x

df.gTrends <- data.frame(df.merged$start,df.merged$Daily)
colnames(df.gTrends)  <- c("Date", "gTrend_volumn")

#Plot the data
df.merged %>%
     ggplot(aes(start, Daily)) +geom_line()
