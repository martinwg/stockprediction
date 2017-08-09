library(pageviews)
library(lubridate)
library(wikipediatrend)

date.from = "2013-01-01"
date.to = "2017-01-01"

search.term = "Aflac"

#r_pageviews <- article_pageviews()
pageviewPKG = function(search.term, date.from, date.to){
     date.from_pv = format(as.Date(date.from),'%Y%m%d%H')
     date.to_pv = format(as.Date(date.to),'%Y%m%d%H')
     df.pageview = article_pageviews(project = "en.wikipedia",
                                     article = search.term,
                                     start = date.from_pv,end = date.to_pv,
                                     user_type = "user", platform = c("desktop"))
     df.wiki = data.frame(as.Date(df.pageview$date),df.pageview$views)
     colnames(df.wiki) = c("Date", "Wikitraffic")
     return(df.wiki)
}

# df.pageview <- article_pageviews(project = "en.wikipedia",article = search.term,
#                                  start = "2015070100",end = "2016010100",
#                                  user_type = "user", platform = c("desktop"))
# df.wiki = data.frame(as.Date(df.pageview$date),df.pageview$views)
# colnames(df.wiki) = c("Date", "Wikitraffic")

wikitrendPKG = function(search.term , date.from , date.to){
     wiki.traffic = wp_trend(search.term, from = date.from, to = date.to,
                              lang = "en")
     trendpkg = data.frame(as.Date(wiki.traffic$date),wiki.traffic$count)  #keep only the date and traffic
     colnames(trendpkg) = c("Date","Wikitraffic")
     return(trendpkg)
}

# wiki.traffic <- wp_trend(search.term, from = date.from, to = date.to,
#                          lang = "en")
# 
# trendpkg = data.frame(as.Date(wiki.traffic$date),wiki.traffic$count)  #keep only the date and traffic
# colnames(trendpkg) = c("Date","Wiki")


if(year(date.to)<2015){
     df.wiki = wikitrendPKG(search.term,date.from,date.to)
}else if(year(date.to) == 2015 & month(date.to) <= 7){
     df.wiki = wikitrendPKG(search.term,date.from,date.to)
}else if(year(date.from)>= 2016){
     df.wiki = pageviewPKG(search.term,date.from,date.to)
}else if(year(date.from)==2015 & month(date.to) >7){
     df.wiki = pageviewPKG(search.term,date.from,date.to)
}else{
     date.break_1 = "2015-06-30"
     date.break_2 = "2015-07-01"
     df.wiki_1 = wikitrendPKG(search.term,date.from,date.break_1)
     df.wiki_2 = pageviewPKG(search.term,date.break_2,date.to)
     df.wiki = rbind(df.wiki_1,df.wiki_2)
}


#final.wiki = merge(trendpkg,df.wiki, by = 'Date',all.x = T)
     
     
     
     
     
