rm(list=ls())
library(rvest)
library(lubridate)
library(stringr)
library(LSAfun)
## Feed in a website URL
my_wbpg <-read_html("https://www.stardem.com/opinion/editorials/no-virus-gets-to-kill-the-first-amendment/article_e2186fa9-1b99-5869-92a5-184d00f87663.html")

## Get the title
## my_wbpg %>%
## html_node("title") %>%
## html_text()

## my_wbpg %>%
##   html_nodes("p") %>%
##   html_text()

junk <- get.wbpg.date("https://www.marketwatch.com/search?q=bitcoin&Link=MW_SearchAC",100)

get.wbpg.date <- function(url = url, hoursAgo = 180)
{
  site_articles <- read_html(
      url)
    
    
    urls <- site_articles %>%
      html_nodes("div.searchresult a")  %>%
      html_attr("href")
    
    datatime.i <- site_articles %>%
      html_nodes("div.deemphasized span.invisible") %>%
      html_text()
    
    datatime.v <- site_articles %>%
      html_nodes("div.deemphasized span") %>%
      html_text()
    
    datatime <- cbind(datatime.i,datatime.v)
    datetime_clean <- gsub("\\.","",datatime)
    datetime_parse <- parse_date_time(
      datetime_clean, "%I:%M %p %m/%d/%Y")
    datetime_convert <- with_tz(datetime_parse, tz = "US/Eastern")
    site_webpgs_datetimes <- data.frame(
      WebPg = urls, DateTime=datetime_convert
    )
    dim(site_webpgs_datetimes)    
    diff_in_hours <- difftime(
      Sys.time(),site_webpgs_datetimes$DateTime, unit = "hours")
    diff_in_hours <- as.double(diff_in_hours)
    site_webpgs_datetimes$DiffHours <- diff_in_hours
    site_latest_data <- subset(site_webpgs_datetimes, DiffHours < hoursAgo)
    
    titles <- c()
    bodies <- c()
    for(i in site_latest_data$WebPg)
    {
      site_latest_wbpg <- read_html(i)
      title <- site_latest_wbpg %>%
        html_node("title") %>%
        html_text()
      titles <- append(titles, title)
      
      site_latest_wbpg <- read_html(i)
      body <- site_latest_wbpg %>%
        html_nodes("p") %>%
        html_text()
      one_body <- paste(body, collapse = "")
      bodies <- append(bodies, one_body)
      
    }
   site_latest_data$Title <- titles
   site_latest_data$Body <- bodies
   clean_text_bodies <- str_squish(site_latest_data$Body)
   
   summary <- c()
   for(i in clean_text_bodies)
   {
     top_info <- genericSummary(i, k=3) ## figure out why LSAfun package can't load
     one_summary <- paste(top_info, collapse = " ")
     summary <- append(summary, one_summary)
   }
   print(summary)
}







