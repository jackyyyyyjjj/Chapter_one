rm (list = ls(all=TRUE))
graphics.off()
library(stringr)
script_path <- rstudioapi::getActiveDocumentContext()$path#è¿æ¯è·åå½åèæ¬çæ¹???
hash = "(.*)/"#è®°ä½è¿éæ¯æç§éé¢çæ­£åè¡¨è¾¾å¼çè§åè¿è¡æååå®¹???
setwd(str_extract(script_path, hash))
library(tidyr)
library(mongolite)
if ('DBI' %in% rownames(installed.packages())==FALSE) {
  install.packages('DBI')
}else{
  library(DBI)
}
# è¿æ¥mongodbæ°æ®???
gc(T)



data1 <- price_sales_rating_quarter[!(is.na(price_sales_rating_quarter$sales)|is.na(price_sales_rating_quarter$prices)|is.na(price_sales_rating_quarter$ave_rating)),]
data2 <- data1[!(is.infinite(data1$sales)|is.infinite(data1$prices)),]
library(openxlsx)
write.xlsx(data2,'sales.xlsx')
# conn <- dbConnect(RSQLite::SQLite(), "C:/Users/hpe21bnu/OneDrive - University of East Anglia/Documents/UEA_PHD/First_charpter/data/Keepa_data/keepa2.sqlite")
# amazon_sales <- dbGetQuery(conn, "SELECT SALES_time,SALES,asin,title,listed_date,brand,variationCSV 
#                from KEEPA1")
# amazon_price <- dbGetQuery(conn, "SELECT AMAZON_NEW_time,AMAZON_NEW,asin,title
#                from KEEPA1")
# 
# amazon_rating <- dbGetQuery(conn, "SELECT COUNT_REVIEWS_time,COUNT_REVIEWS,asin
#                from KEEPA1")
# 
# amazon_ave_rating <- dbGetQuery(conn, "SELECT RATING_time,RATING,asin 
#                from KEEPA1")
# library(stringr)
# 
# dbDisconnect(conn)

# extract sales for AMAZCOUNT ---------------------------------------------
# library(tibble)
# library(openxlsx)
# 
# 
# 
# 
# 
# asin <- read.xlsx('full_data3.xlsx')
# needdata <- amazon_sales[amazon_sales$asin %in% asin$asin,]
# sales <- data.frame(date=c(),sales=c(),month=c(),asin=c())
# # vector_sales <- c(rep(NA,dim(amazon_sales)[1]))
# for (i in seq(1,dim(needdata)[1],1)) {
#   mid1 <- gsub('\\[','',needdata[i,1])
#   mid1 <- gsub('\\]','',mid1)
#   mid1 <- gsub("'",'',mid1)
#   mid1 <- strsplit(mid1,',')
#   mid1 <- unlist(mid1)
#   mid2 <- gsub('\\[','',needdata[i,2])
#   mid2 <- gsub('\\]','',mid2)
#   mid2 <- gsub("'",'',mid2)
#   mid2 <- strsplit(mid2,',')
#   mid2 <- unlist(mid2)
#   sales_dataframe <- data.frame(date=mid1,sales.rank=mid2)
#   names(sales_dataframe) <- c('date','sales')
#   sales_dataframe['month'] <- format(as.Date(sales_dataframe$date),format="%Y-%m")
#   sales_dataframe['asin'] <- needdata[i,3]
#   sales <- rbind(sales,sales_dataframe)
#   # subT <- sales_dataframe[sales_dataframe$date.month==quarters[1]|sales_dataframe$date.month==quarters[2]|sales_dataframe$date.month==quarters[3],2]
#   # subT <- as.numeric(subT)
#   # mean <- mean(subT,na.rm=T)
#   # vector_sales[i] <- mean
#   }
# 
# sales$sales <- as.numeric(sales$sales)
# saveRDS(sales,'sales.Rds')
# 
# 
# 
# salesama <- sales[sales$sales<120000&sales$sales>0,]
# 
# write.xlsx(salesama,'sales.xlsx')
# write.csv(sales,'sales.csv')
# # library(openxlsx)
# # k <- read.xlsx('./More_raw_data/asin.xlsx')
# # t <- con2$find(fields = '{"asin" : true}')
# # 
# # ff <- k[!k$asin %in% t$asin,]
# # write.csv(ff,'asin.csv')


amazon <- readRDS('FULL_ONLY3_1.Rds')
amazon_sales <- amazon[,c(1,3,4,12,13:15)]
amazon_price <- amazon[,c(1,5,6)]
amazon_rating <- amazon[,c(1,9,10)]

amazon_ave_rating <- amazon[,c(1,7,8)]


# collect price and sales -------------------------------------------------
# 
# for (i in seq(1,dim(amazon_sales)[1],1)) {
#   mid1 <- gsub('\\[','',amazon_sales[i,2])
#   mid1 <- gsub('\\]','',mid1)
#   mid1 <- gsub("'",'',mid1)
#   mid1 <- strsplit(mid1,',')
#   mid1 <- unlist(mid1)
#   mid2 <- gsub('\\[','',amazon_sales[i,3])
#   mid2 <- gsub('\\]','',mid2)
#   mid2 <- gsub("'",'',mid2)
#   mid2 <- strsplit(mid2,',')
#   mid2 <- unlist(mid2)
#   sales_dataframe <- data.frame(date=mid1,sales.rank=mid2)
#   names(sales_dataframe) <- c('date','sales')
#   sales_dataframe['month'] <- format(as.Date(sales_dataframe$date),format="%Y-%m")
#   sales_dataframe['asin'] <- amazon_sales[i,3]
#   sales <- rbind(sales,sales_dataframe)
#   # subT <- sales_dataframe[sales_dataframe$date.month==quarters[1]|sales_dataframe$date.month==quarters[2]|sales_dataframe$date.month==quarters[3],2]
#   # subT <- as.numeric(subT)
#   # mean <- mean(subT,na.rm=T)
#   # vector_sales[i] <- mean
# }


# library(openxlsx)
# k <- read.xlsx('./More_raw_data/asin.xlsx')
# t <- con2$find(fields = '{"asin" : true}')
# 
# ff <- k[!k$asin %in% t$asin,]
# write.csv(ff,'asin.csv')

# collect price and sales -------------------------------------------------

collect_sales <- function(quarters){
  quarters <- unlist(quarters)
  vector_sales <- c(rep(NA,dim(amazon_sales)[1]))
  for (i in seq(1,dim(amazon_sales)[1],1)) {
    mid1 <- gsub('\\[','',amazon_sales[i,2])
    mid1 <- gsub('\\]','',mid1)
    mid1 <- gsub("'",'',mid1)
    mid1 <- strsplit(mid1,',')
    mid1 <- unlist(mid1)
    mid2 <- gsub('\\[','',amazon_sales[i,3])
    mid2 <- gsub('\\]','',mid2)
    mid2 <- gsub("'",'',mid2)
    mid2 <- strsplit(mid2,',')
    mid2 <- unlist(mid2)
    sales_dataframe <- data.frame(date=mid1,sales.rank=mid2)
    names(sales_dataframe) <- c('date','sales')
    sales_dataframe$date.month <- format(as.Date(sales_dataframe$date),format="%Y-%m")
    subT <- sales_dataframe[sales_dataframe$date.month==quarters[1]|sales_dataframe$date.month==quarters[2]|sales_dataframe$date.month==quarters[3],2]
    subT <- as.numeric(subT)
    subT <- subT[subT>0]
    mean <- min(subT,na.rm=T)
    vector_sales[i] <- mean
  }
  return(vector_sales)
}


collect_prices <- function(quarters){
  quarters <- unlist(quarters)
  vector_price <- c(rep(NA,dim(amazon_price)[1]))
  for (i in seq(1,dim(amazon_price)[1],1)) {
    mid1 <- gsub('\\[','',amazon_price[i,2])
    mid1 <- gsub('\\]','',mid1)
    mid1 <- gsub("'",'',mid1)
    mid1 <- strsplit(mid1,',')
    mid1 <- unlist(mid1)
    mid2 <- gsub('\\[','',amazon_price[i,3])
    mid2 <- gsub('\\]','',mid2)
    mid2 <- gsub("'",'',mid2)
    mid2 <- strsplit(mid2,',')
    mid2 <- unlist(mid2)
    price_dataframe <- data.frame(date=mid1,price=mid2)
    names(price_dataframe) <- c('date','price')
    price_dataframe['date.month'] <- format(as.Date(price_dataframe$date),format="%Y-%m")
    subT <- price_dataframe[price_dataframe$date.month==quarters[1]|price_dataframe$date.month==quarters[2]|price_dataframe$date.month==quarters[3],2]
    subT <- as.numeric(subT)
    subT <- subT[subT>0]
    mean <- mean(subT,na.rm=T)
    vector_price[i] <- mean
  }
  return(vector_price)
} 


collect_rating <- function(quarters){
  quarters <- unlist(quarters)
  vector_rating <- c(rep(NA,dim(amazon_rating)[1]))
  for (i in seq(1,dim(amazon_rating)[1],1)) {
    i <- 1
    mid1 <- gsub('\\[','',amazon_rating[i,2])
    mid1 <- gsub('\\]','',mid1)
    mid1 <- gsub("'",'',mid1)
    mid1 <- strsplit(mid1,',')
    mid1 <- unlist(mid1)
    mid2 <- gsub('\\[','',amazon_rating[i,3])
    mid2 <- gsub('\\]','',mid2)
    mid2 <- gsub("'",'',mid2)
    mid2 <- strsplit(mid2,',')
    mid2 <- unlist(mid2)
    rating_dataframe <- data.frame(date=mid1,rating=mid2)
    names(rating_dataframe) <- c('date','rating')
    Negrat <- as.numeric(rating_dataframe$rating)<0
    rating_dataframe <- rating_dataframe[!Negrat,]
    rating_dataframe['date.month'] <- format(as.Date(rating_dataframe$date),format="%Y-%m")
    subT <- rating_dataframe[rating_dataframe$date.month==quarters[1]|rating_dataframe$date.month==quarters[2]|rating_dataframe$date.month==quarters[3],2]
    if(length(subT)!=0){
      subT <- as.numeric(subT)
      subT <- subT[subT>0]
      diff <- max(subT)  # later I can use this data to analysis these things, then I can use the biggest one minues the formal one
      vector_rating[i] <- diff
    }else{
      vector_rating[i]=NA
    }
    
  }
  return(vector_rating)
} 


collect_ave_rating <- function(quarters){
  quarters <- unlist(quarters)
  vector_ave_rating <- c(rep(NA,dim(amazon_ave_rating )[1]))
  for (i in seq(1,dim(amazon_ave_rating )[1],1)) {
    mid1 <- gsub('\\[','',amazon_ave_rating[i,2])
    mid1 <- gsub('\\]','',mid1)
    mid1 <- gsub("'",'',mid1)
    mid1 <- strsplit(mid1,',')
    mid1 <- unlist(mid1)
    mid2 <- gsub('\\[','',amazon_ave_rating[i,3])
    mid2 <- gsub('\\]','',mid2)
    mid2 <- gsub("'",'',mid2)
    mid2 <- strsplit(mid2,',')
    mid2 <- unlist(mid2)
    AVE_rating_dataframe <- data.frame(date=mid1,rating=mid2)
    names(AVE_rating_dataframe) <- c('date','AVE_rating')
    # Negrat <- as.numeric(AVE_rating_dataframe$AVE_rating)<0
    # rating_dataframe <- rating_dataframe[!Negrat,]
    AVE_rating_dataframe['date.month'] <- format(as.Date(AVE_rating_dataframe$date),format="%Y-%m")
    subT <- AVE_rating_dataframe[AVE_rating_dataframe$date.month==quarters[1]|AVE_rating_dataframe$date.month==quarters[2]|AVE_rating_dataframe$date.month==quarters[3],2]
    if(length(subT)!=0){
      subT <- as.numeric(subT)
      subT <- subT[subT>0]
      mean <- mean(subT,na.rm=T)
      vector_ave_rating[i] <- mean
    }else{
      vector_ave_rating[i]=NA
    }
    
  }
  return(vector_ave_rating)
} 



# market 9 2020 1qr -------------------------------------------------------
list_month <- list(c('2022-01','2022-02','2022-03'),
                   c('2021-01','2021-02','2021-03'),c('2021-04','2021-05','2021-06'),c('2021-07','2021-08','2021-09'),c('2021-10','2021-11','2021-12'),
                   c('2020-01','2020-02','2020-03'),c('2020-04','2020-05','2020-06'),c('2020-07','2020-08','2020-09'),c('2020-10','2020-11','2020-12'),
                   c('2019-01','2019-02','2019-03'),c('2019-04','2019-05','2019-06'),c('2019-07','2019-08','2019-09'),c('2019-10','2019-11','2019-12'),
                    c('2018-01','2018-02','2018-03'),c('2018-04','2018-05','2018-06'),c('2018-07','2018-08','2018-09'),c('2018-10','2018-11','2018-12'),
                c('2017-01','2017-02','2017-03'),c('2017-04','2017-05','2017-06'),c('2017-07','2017-08','2017-09'),c('2017-10','2017-11','2017-12'),
                c('2016-01','2016-02','2016-03'),c('2016-04','2016-05','2016-06'),c('2016-07','2016-08','2016-09'),c('2016-10','2016-11','2016-12'),
                c('2015-01','2015-02','2015-03'),c('2015-04','2015-05','2015-06'),c('2015-07','2015-08','2015-09'),c('2015-10','2015-11','2015-12'),
                c('2014-01','2014-02','2014-03'),c('2014-04','2014-05','2014-06'),c('2014-07','2014-08','2014-09'),c('2014-10','2014-11','2014-12'),
                c('2013-01','2013-02','2013-03'),c('2013-04','2013-05','2013-06'),c('2013-07','2013-08','2013-09'),c('2013-10','2013-11','2013-12'),
                c('2012-01','2012-02','2012-03'),c('2012-04','2012-05','2012-06'),c('2012-07','2012-08','2012-09'),c('2012-10','2012-11','2012-12'),
                c('2011-01','2011-02','2011-03'),c('2011-04','2011-05','2011-06'),c('2011-07','2011-08','2011-09'),c('2011-10','2011-11','2011-12'))

markit_ids <- c('R22Q1','R21Q1','R21Q2','R21Q3','R21Q4',
                'R20Q1','R20Q2','R20Q3','R20Q4',
                'R19Q1','R19Q2','R19Q3','R19Q4',
                'R18Q1','R18Q2','R18Q3','R18Q4',
                'R17Q1','R17Q2','R17Q3','R17Q4',
                'R16Q1','R16Q2','R16Q3','R16Q4',
                'R15Q1','R15Q2','R15Q3','R15Q4',
                'R14Q1','R14Q2','R14Q3','R14Q4',
                'R13Q1','R13Q2','R13Q3','R13Q4',
                'R12Q1','R12Q2','R12Q3','R12Q4',
                'R11Q1','R11Q2','R11Q3','R11Q4')
price_sales_rating <- data.frame(asin = c(),variationCSV.x =c(),listed_date=c(),
                                 market_ids = c(),sales =c(),rating=c(),
                                 asin = c(),ave_rating =c())

for (i in seq(1,length(list_month),1) ) {
  subT_20_1qr <- amazon_sales[,-c(2,3)]
  subT_20_1qr$market_ids <- markit_ids[i]
  subT_20_1qr$sales <- collect_sales(list_month[i])
  subT_20_1qr$prices <- collect_prices(list_month[i])
  subT_20_1qr$rating <- collect_rating(list_month[i])
  subT_20_1qr$ave_rating <- collect_ave_rating(list_month[i])
  price_sales_rating <-rbind(price_sales_rating,subT_20_1qr)
}



# collect <- function(markit_ids,list_month) {
#   subT_20_1qr <- amazon_sales[,-c(2,3)]
#   subT_20_1qr$market_ids <- markit_ids
#   subT_20_1qr$sales <- collect_sales(list_month)
#   subT_20_1qr$prices <- collect_prices(list_month)
#   subT_20_1qr$rating <- collect_rating(list_month)
#   subT_20_1qr$ave_rating <- collect_ave_rating(list_month)
#   price_sales_rating <-rbind(price_sales_rating,subT_20_1qr)
# }

# price_sales_rating <- mapply(collect, markit_ids,list_month)
saveRDS(price_sales_rating,'price_sales_rating_quarter.Rds')


# library("doParallel")      #??????doParallel???????????????????????????
# library("foreach")         #??????foreach???
# 
# system.time({
#   cl<- makeCluster(2)      
#   registerDoParallel(cl)       #??????????????????
#   mydata1 <- foreach(
#     markit_ids=markit_ids,          #???????????????????????????
#     list_month=list_month,
#     .combine=rbind,  #?????????????????????
#     #?????????????????????????????????
#   ) %dopar% collect(markit_ids,list_month)
#   stopCluster(cl)
# })



























subT_20_1qr <- amazon_sales[,-c(3,4)]
subT_20_1qr$market_ids <- 'R20M1'
subT_20_1qr$sales <- collect_sales(c('2020-01'))
subT_20_1qr$prices <- collect_prices(c('2020-01','2020-02','2020-03'))
subT_20_1qr$rating <- collect_rating(c('2020-01','2020-02','2020-03'))
subT_20_1qr$ave_rating <- collect_ave_rating(c('2020-01','2020-02','2020-03'))
# market 10 2019 4qr -------------------------------------------------------
subT_19_4qr <- amazon_sales[,-c(4,5)]
subT_19_4qr$market_ids <- 'R19Q4'
subT_19_4qr$sales <- collect_sales(c('2019-10','2019-11','2019-12'))
subT_19_4qr$prices <- collect_prices(c('2019-10','2019-11','2019-12'))
subT_19_4qr$rating <- collect_rating(c('2019-10','2019-11','2019-12'))
subT_19_4qr$ave_rating <- collect_ave_rating(c('2019-10','2019-11','2019-12'))
# market 11 2019 3qr -------------------------------------------------------
subT_19_3qr <- amazon_sales[,-c(1,2,4,5)]
subT_19_3qr$market_ids <- 'R19Q3'
subT_19_3qr$sales <- collect_sales(c('2019-07','2019-08','2019-09'))
subT_19_3qr$prices <- collect_prices(c('2019-07','2019-08','2019-09'))
subT_19_3qr$rating <- collect_rating(c('2019-07','2019-08','2019-09'))
subT_19_3qr$ave_rating <- collect_ave_rating(c('2019-07','2019-08','2019-09'))
# market 12 2019 2qr -------------------------------------------------------
subT_19_2qr <- amazon_sales[,-c(1,2,4,5)]
subT_19_2qr$market_ids <- 'R19Q2'
subT_19_2qr$sales <- collect_sales(c('2019-04','2019-05','2019-06'))
subT_19_2qr$prices <- collect_prices(c('2019-04','2019-05','2019-06'))
subT_19_2qr$rating <- collect_rating(c('2019-04','2019-05','2019-06'))
subT_19_2qr$ave_rating <- collect_ave_rating(c('2019-04','2019-05','2019-06'))
# market 13 2019 1qr -------------------------------------------------------
subT_19_1qr <- amazon_sales[,-c(1,2,4,5)]
subT_19_1qr$market_ids <- 'R19Q1'
subT_19_1qr$sales <- collect_sales(c('2019-01','2019-02','2019-03'))
subT_19_1qr$prices <- collect_prices(c('2019-01','2019-02','2019-03'))
subT_19_1qr$rating <- collect_rating(c('2019-01','2019-02','2019-03'))
subT_19_1qr$ave_rating <- collect_ave_rating(c('2019-01','2019-02','2019-03'))
# market 14 2018 4qr -------------------------------------------------------
subT_18_4qr <- amazon_sales[,-c(1,2,4,5)]
subT_18_4qr$market_ids <- 'R18Q4'
subT_18_4qr$sales <- collect_sales(c('2018-10','2018-11','2018-12'))
subT_18_4qr$prices <- collect_prices(c('2018-10','2018-11','2018-12'))
subT_18_4qr$rating <- collect_rating(c('2018-10','2018-11','2018-12'))
subT_18_4qr$ave_rating <- collect_ave_rating(c('2018-10','2018-11','2018-12'))
# market 15 2018 3qr -------------------------------------------------------
subT_18_3qr <- amazon_sales[,-c(1,2,4,5)]
subT_18_3qr$market_ids <- 'R18Q3'
subT_18_3qr$sales <- collect_sales(c('2018-07','2018-08','2018-09'))
subT_18_3qr$prices <- collect_prices(c('2018-07','2018-08','2018-09'))
subT_18_3qr$rating <- collect_rating(c('2018-07','2018-08','2018-09'))
subT_18_3qr$ave_rating <- collect_ave_rating(c('2018-07','2018-08','2018-09'))
# market 16 2018 2qr -------------------------------------------------------
subT_18_2qr <- amazon_sales[,-c(1,2,4,5)]
subT_18_2qr$market_ids <- 'R18Q2'
subT_18_2qr$sales <- collect_sales(c('2018-04','2018-05','2018-06'))
subT_18_2qr$prices <- collect_prices(c('2018-04','2018-05','2018-06'))
subT_18_2qr$rating <- collect_rating(c('2018-04','2018-05','2018-06'))
subT_18_2qr$ave_rating <- collect_ave_rating(c('2018-04','2018-05','2018-06'))
# market 17 2018 1qr -------------------------------------------------------
subT_18_1qr <- amazon_sales[,-c(1,2,4,5)]
subT_18_1qr$market_ids <- 'R18Q1'
subT_18_1qr$sales <- collect_sales(c('2018-01','2018-02','2018-03'))
subT_18_1qr$prices <- collect_prices(c('2018-01','2018-02','2018-03'))
subT_18_1qr$rating <- collect_rating(c('2018-01','2018-02','2018-03'))
subT_18_1qr$ave_rating <- collect_ave_rating(c('2018-01','2018-02','2018-03'))
# market 18 2017 4qr -------------------------------------------------------
subT_17_4qr <- amazon_sales[,-c(1,2,4,5)]
subT_17_4qr$market_ids <- 'R17Q4'
subT_17_4qr$sales <- collect_sales(c('2017-10','2017-11','2017-12'))
subT_17_4qr$prices <- collect_prices(c('2017-10','2017-11','2017-12'))
subT_17_4qr$rating <- collect_rating(c('2017-10','2017-11','2017-12'))
subT_17_4qr$ave_rating <- collect_ave_rating(c('2017-10','2017-11','2017-12'))
# market 19 2017 3qr -------------------------------------------------------
subT_17_3qr <- amazon_sales[,-c(1,2,4,5)]
subT_17_3qr$market_ids <- 'R17Q3'
subT_17_3qr$sales <- collect_sales(c('2017-07','2017-08','2017-09'))
subT_17_3qr$prices <- collect_prices(c('2017-07','2017-08','2017-09'))
subT_17_3qr$rating <- collect_rating(c('2017-07','2017-08','2017-09'))
subT_17_3qr$ave_rating <- collect_ave_rating(c('2017-07','2017-08','2017-09'))
# market 20 2017 2qr -------------------------------------------------------
subT_17_2qr <- amazon_sales[,-c(1,2,4,5)]
subT_17_2qr$market_ids <- 'R17Q2'
subT_17_2qr$sales <- collect_sales(c('2017-04','2017-05','2017-06'))
subT_17_2qr$prices <- collect_prices(c('2017-04','2017-05','2017-06'))
subT_17_2qr$rating <- collect_rating(c('2017-04','2017-05','2017-06'))
subT_17_2qr$ave_rating <- collect_ave_rating(c('2017-04','2017-05','2017-06'))
# market 21 2017 1qr -------------------------------------------------------
subT_17_1qr <- amazon_sales[,-c(1,2,4,5)]
subT_17_1qr$market_ids <- 'R17Q1'
subT_17_1qr$sales <- collect_sales(c('2017-01','2017-02','2017-03'))
subT_17_1qr$prices <- collect_prices(c('2017-01','2017-02','2017-03'))
subT_17_1qr$rating <- collect_rating(c('2017-01','2017-02','2017-03'))
subT_17_1qr$ave_rating <- collect_ave_rating(c('2017-01','2017-02','2017-03'))
# market 22 2016 4qr -------------------------------------------------------
subT_16_4qr <- amazon_sales[,-c(1,2,4,5)]
subT_16_4qr$market_ids <- 'R16Q4'
subT_16_4qr$sales <- collect_sales(c('2016-10','2016-11','2016-12'))
subT_16_4qr$prices <- collect_prices(c('2016-10','2016-11','2016-12'))
subT_16_4qr$rating <- collect_rating(c('2016-10','2016-11','2016-12'))
subT_16_4qr$ave_rating <- collect_ave_rating(c('2016-10','2016-11','2016-12'))
# market 23 2016 3qr -------------------------------------------------------
subT_16_3qr <- amazon_sales[,-c(1,2,4,5)]
subT_16_3qr$market_ids <- 'R16Q3'
subT_16_3qr$sales <- collect_sales(c('2016-07','2016-08','2016-09'))
subT_16_3qr$prices <- collect_prices(c('2016-07','2016-08','2016-09'))
subT_16_3qr$rating <- collect_rating(c('2016-07','2016-08','2016-09'))
subT_16_3qr$ave_rating <- collect_ave_rating(c('2016-07','2016-08','2016-09'))
# market 24 2016 2qr -------------------------------------------------------
subT_16_2qr <- amazon_sales[,-c(1,2,4,5)]
subT_16_2qr$market_ids <- 'R16Q2'
subT_16_2qr$sales <- collect_sales(c('2016-04','2016-05','2016-06'))
subT_16_2qr$prices <- collect_prices(c('2016-04','2016-05','2016-06'))
subT_16_2qr$rating <- collect_rating(c('2016-04','2016-05','2016-06'))
subT_16_2qr$ave_rating <- collect_ave_rating(c('2016-04','2016-05','2016-06'))
# market 25 2016 1qr -------------------------------------------------------
subT_16_1qr <- amazon_sales[,-c(1,2,4,5)]
subT_16_1qr$market_ids <- 'R16Q1'
subT_16_1qr$sales <- collect_sales(c('2016-01','2016-02','2016-03'))
subT_16_1qr$prices <- collect_prices(c('2016-01','2016-02','2016-03'))
subT_16_1qr$rating <- collect_rating(c('2016-01','2016-02','2016-03'))
subT_16_1qr$ave_rating <- collect_ave_rating(c('2016-01','2016-02','2016-03'))
# market 26 2015 4qr -------------------------------------------------------
subT_15_4qr <- amazon_sales[,-c(1,2,4,5)]
subT_15_4qr$market_ids <- 'R15Q4'
subT_15_4qr$sales <- collect_sales(c('2015-10','2015-11','2015-12'))
subT_15_4qr$prices <- collect_prices(c('2015-10','2015-11','2015-12'))
subT_15_4qr$rating <- collect_rating(c('2015-10','2015-11','2015-12'))
subT_15_4qr$ave_rating <- collect_ave_rating(c('2015-10','2015-11','2015-12'))
# market 27 2015 3qr -------------------------------------------------------
subT_15_3qr <- amazon_sales[,-c(1,2,4,5)]
subT_15_3qr$market_ids <- 'R15Q3'
subT_15_3qr$sales <- collect_sales(c('2015-07','2015-08','2015-09'))
subT_15_3qr$prices <- collect_prices(c('2015-07','2015-08','2015-09'))
subT_15_3qr$rating <- collect_rating(c('2015-07','2015-08','2015-09'))
subT_15_3qr$ave_rating <- collect_ave_rating(c('2015-07','2015-08','2015-09'))
# market 28 2015 2qr -------------------------------------------------------
subT_15_2qr <- amazon_sales[,-c(1,2,4,5)]
subT_15_2qr$market_ids <- 'R15Q2'
subT_15_2qr$sales <- collect_sales(c('2015-04','2015-05','2015-06'))
subT_15_2qr$prices <- collect_prices(c('2015-04','2015-05','2015-06'))
subT_15_2qr$rating <- collect_rating(c('2015-04','2015-05','2015-06'))
subT_15_2qr$ave_rating <- collect_ave_rating(c('2015-04','2015-05','2015-06'))
# market 29 2015 1qr -------------------------------------------------------
subT_15_1qr <- amazon_sales[,-c(1,2,4,5)]
subT_15_1qr$market_ids <- 'R15Q1'
subT_15_1qr$sales <- collect_sales(c('2015-01','2015-02','2015-03'))
subT_15_1qr$prices <- collect_prices(c('2015-01','2015-02','2015-03'))
subT_15_1qr$rating <- collect_rating(c('2015-01','2015-02','2015-03'))
subT_15_1qr$ave_rating <- collect_ave_rating(c('2015-01','2015-02','2015-03'))
# market 30 2014 4qr -------------------------------------------------------
subT_14_4qr <- amazon_sales[,-c(1,2,4,5)]
subT_14_4qr$market_ids <- 'R14Q4'
subT_14_4qr$sales <- collect_sales(c('2014-10','2014-11','2014-12'))
subT_14_4qr$prices <- collect_prices(c('2014-10','2014-11','2014-12'))
subT_14_4qr$rating <- collect_rating(c('2014-10','2014-11','2014-12'))
subT_14_4qr$ave_rating <- collect_ave_rating(c('2014-10','2014-11','2014-12'))
# market 31 2015 3qr -------------------------------------------------------
subT_14_3qr <- amazon_sales[,-c(1,2,4,5)]
subT_14_3qr$market_ids <- 'R14Q3'
subT_14_3qr$sales <- collect_sales(c('2014-07','2014-08','2014-09'))
subT_14_3qr$prices <- collect_prices(c('2014-07','2014-08','2014-09'))
subT_14_3qr$rating <- collect_rating(c('2014-07','2014-08','2014-09'))
subT_14_3qr$ave_rating <- collect_ave_rating(c('2014-07','2014-08','2014-09'))
# market 32 2015 2qr -------------------------------------------------------
subT_14_2qr <- amazon_sales[,-c(1,2,4,5)]
subT_14_2qr$market_ids <- 'R14Q2'
subT_14_2qr$sales <- collect_sales(c('2014-04','2014-05','2014-06'))
subT_14_2qr$prices <- collect_prices(c('2014-04','2014-05','2014-06'))
subT_14_2qr$rating <- collect_rating(c('2014-04','2014-05','2014-06'))
subT_14_2qr$ave_rating <- collect_ave_rating(c('2014-04','2014-05','2014-06'))
# market 33 2015 1qr -------------------------------------------------------
subT_14_1qr <- amazon_sales[,-c(1,2,4,5)]
subT_14_1qr$market_ids <- 'R14Q1'
subT_14_1qr$sales <- collect_sales(c('2014-01','2014-02','2014-03'))
subT_14_1qr$prices <- collect_prices(c('2014-01','2014-02','2014-03'))
subT_14_1qr$rating <- collect_rating(c('2014-01','2014-02','2014-03'))
subT_14_1qr$ave_rating <- collect_ave_rating(c('2014-01','2014-02','2014-03'))
# market 34 2013 4qr -------------------------------------------------------
subT_13_4qr <- amazon_sales[,-c(1,2,4,5)]
subT_13_4qr$market_ids <- 'R13Q4'
subT_13_4qr$sales <- collect_sales(c('2013-10','2013-11','2013-12'))
subT_13_4qr$prices <- collect_prices(c('2013-10','2013-11','2013-12'))
subT_13_4qr$rating <- collect_rating(c('2013-10','2013-11','2013-12'))
subT_13_4qr$ave_rating <- collect_ave_rating(c('2013-10','2013-11','2013-12'))
# market 35 2013 3qr -------------------------------------------------------
subT_13_3qr <- amazon_sales[,-c(1,2,4,5)]
subT_13_3qr$market_ids <- 'R13Q3'
subT_13_3qr$sales <- collect_sales(c('2013-07','2013-08','2013-09'))
subT_13_3qr$prices <- collect_prices(c('2013-07','2013-08','2013-09'))
subT_13_3qr$rating <- collect_rating(c('2013-07','2013-08','2013-09'))
subT_13_3qr$ave_rating <- collect_ave_rating(c('2013-07','2013-08','2013-09'))
# market 36 2013 2qr -------------------------------------------------------
subT_13_2qr <- amazon_sales[,-c(1,2,4,5)]
subT_13_2qr$market_ids <- 'R13Q2'
subT_13_2qr$sales <- collect_sales(c('2013-04','2013-05','2013-06'))
subT_13_2qr$prices <- collect_prices(c('2013-04','2013-05','2013-06'))
subT_13_2qr$rating <- collect_rating(c('2013-04','2013-05','2013-06'))
subT_13_2qr$ave_rating <- collect_ave_rating(c('2013-04','2013-05','2013-06'))
# market 37 2013 1qr -------------------------------------------------------
subT_13_1qr <- amazon_sales[,-c(1,2,4,5)]
subT_13_1qr$market_ids <- 'R13Q1'
subT_13_1qr$sales <- collect_sales(c('2013-01','2013-02','2013-03'))
subT_13_1qr$prices <- collect_prices(c('2013-01','2013-02','2013-03'))
subT_13_1qr$rating <- collect_rating(c('2013-01','2013-02','2013-03'))
subT_13_1qr$ave_rating <- collect_ave_rating(c('2013-01','2013-02','2013-03'))
# market 34 2012 4qr -------------------------------------------------------
subT_12_4qr <- amazon_sales[,-c(1,2,4,5)]
subT_12_4qr$market_ids <- 'R12Q4'
subT_12_4qr$sales <- collect_sales(c('2012-10','2012-11','2012-12'))
subT_12_4qr$prices <- collect_prices(c('2012-10','2012-11','2012-12'))
subT_12_4qr$rating <- collect_rating(c('2012-10','2012-11','2012-12'))
subT_12_4qr$ave_rating <- collect_ave_rating(c('2012-10','2012-11','2012-12'))
# market 35 2012 3qr -------------------------------------------------------
subT_12_3qr <- amazon_sales[,-c(1,2,4,5)]
subT_12_3qr$market_ids <- 'R12Q3'
subT_12_3qr$sales <- collect_sales(c('2012-07','2012-08','2012-09'))
subT_12_3qr$prices <- collect_prices(c('2012-07','2012-08','2012-09'))
subT_12_3qr$rating <- collect_rating(c('2012-07','2012-08','2012-09'))
subT_12_3qr$ave_rating <- collect_ave_rating(c('2012-07','2012-08','2012-09'))
# market 36 2012 2qr -------------------------------------------------------
subT_12_2qr <- amazon_sales[,-c(1,2,4,5)]
subT_12_2qr$market_ids <- 'R12Q2'
subT_12_2qr$sales <- collect_sales(c('2012-04','2012-05','2012-06'))
subT_12_2qr$prices <- collect_prices(c('2012-04','2012-05','2012-06'))
subT_12_2qr$rating <- collect_rating(c('2012-04','2012-05','2012-06'))
subT_12_2qr$ave_rating <- collect_ave_rating(c('2012-04','2012-05','2012-06'))
# market 37 2012 1qr -------------------------------------------------------
subT_12_1qr <- amazon_sales[,-c(1,2,4,5)]
subT_12_1qr$market_ids <- 'R12Q1'
subT_12_1qr$sales <- collect_sales(c('2012-01','2012-02','2012-03'))
subT_12_1qr$prices <- collect_prices(c('2012-01','2012-02','2012-03'))
subT_12_1qr$rating <- collect_rating(c('2012-01','2012-02','2012-03'))
subT_12_1qr$ave_rating <- collect_ave_rating(c('2012-01','2012-02','2012-03'))
# market 34 2011 4qr -------------------------------------------------------
subT_11_4qr <- amazon_sales[,-c(1,2,4,5)]
subT_11_4qr$market_ids <- 'R11Q4'
subT_11_4qr$sales <- collect_sales(c('2011-10','2011-11','2011-12'))
subT_11_4qr$prices <- collect_prices(c('2011-10','2011-11','2011-12'))
subT_11_4qr$rating <- collect_rating(c('2011-10','2011-11','2011-12'))
subT_11_4qr$ave_rating <- collect_ave_rating(c('2011-10','2011-11','2011-12'))
# market 35 2011 3qr -------------------------------------------------------
subT_11_3qr <- amazon_sales[,-c(1,2,4,5)]
subT_11_3qr$market_ids <- 'R11Q3'
subT_11_3qr$sales <- collect_sales(c('2011-07','2011-08','2011-09'))
subT_11_3qr$prices <- collect_prices(c('2011-07','2011-08','2011-09'))
subT_11_3qr$rating <- collect_rating(c('2011-07','2011-08','2011-09'))
subT_11_3qr$ave_rating <- collect_ave_rating(c('2011-07','2011-08','2011-09'))
# market 36 2011 2qr -------------------------------------------------------
subT_11_2qr <- amazon_sales[,-c(1,2,4,5)]
subT_11_2qr$market_ids <- 'R11Q2'
subT_11_2qr$sales <- collect_sales(c('2011-04','2011-05','2011-06'))
subT_11_2qr$prices <- collect_prices(c('2011-04','2011-05','2011-06'))
subT_11_2qr$rating <- collect_rating(c('2011-04','2011-05','2011-06'))
subT_11_2qr$ave_rating <- collect_ave_rating(c('2011-04','2011-05','2011-06'))
# market 37 2011 1qr -------------------------------------------------------
subT_11_1qr <- amazon_sales[,-c(1,2,4,5)]
subT_11_1qr$market_ids <- 'R11Q1'
subT_11_1qr$sales <- collect_sales(c('2011-01','2011-02','2011-03'))
subT_11_1qr$prices <- collect_prices(c('2011-01','2011-02','2011-03'))
subT_11_1qr$rating <- collect_rating(c('2011-01','2011-02','2011-03'))
subT_11_1qr$ave_rating <- collect_ave_rating(c('2011-01','2011-02','2011-03'))
# price_sales_rating <- rbind(subT_11_1qr,subT_11_2qr,subT_11_3qr,subT_11_4qr,
#                             subT_12_1qr,subT_12_2qr,subT_12_3qr,subT_12_4qr,
#                             subT_13_1qr,subT_13_2qr,subT_13_3qr,subT_13_4qr,
#                             subT_14_1qr,subT_14_2qr,subT_14_3qr,subT_14_4qr,
#                             subT_15_1qr,subT_15_2qr,subT_15_3qr,subT_15_4qr,
#                             subT_16_1qr,subT_16_2qr,subT_16_3qr,subT_16_4qr,
#                             subT_17_1qr,subT_17_2qr,subT_17_3qr,subT_17_4qr,
#                             subT_18_1qr,subT_18_2qr,subT_18_3qr,subT_18_4qr,
#                             subT_19_1qr,subT_19_2qr,subT_19_3qr,subT_19_4qr,
#                             subT_20_1qr,subT_20_2qr,subT_20_3qr,subT_20_4qr,
#                             subT_21_1qr,subT_21_2qr,subT_21_3qr,subT_21_4qr,subT_22_1qr,subT_22_2qr)

price_sales_rating <- rbind(subT_11_1qr,subT_11_2qr,subT_11_3qr,subT_11_4qr,
                            subT_12_1qr,subT_12_2qr,subT_12_3qr,subT_12_4qr,
                            subT_13_1qr,subT_13_2qr,subT_13_3qr,subT_13_4qr,
                            subT_14_1qr,subT_14_2qr,subT_14_3qr,subT_14_4qr,
                            subT_15_1qr,subT_15_2qr,subT_15_3qr,subT_15_4qr,
                            subT_16_1qr,subT_16_2qr,subT_16_3qr,subT_16_4qr,
                            subT_17_1qr,subT_17_2qr,subT_17_3qr,subT_17_4qr,
                            subT_18_1qr,subT_18_2qr,subT_18_3qr,subT_18_4qr,
                            subT_19_1qr,subT_19_2qr,subT_19_3qr,subT_19_4qr,
                            subT_20_1qr)
saveRDS(price_sales_rating,'price_sales_rating_ave_rating.Rds')



# con3 <- mongo(
#   collection = "amazon_vacuum_ratings_2",
#   url = "mongodb://Junjun:123456@localhost:27017/taobao",
#   verbose = FALSE,
#   options = ssl_options()
# )
# find the parent asin ----------------------------------------------------
# amazon_variation <- con2$find(fields = '{"variationCSV" : true,"variations" : true,"asin" : true,"parentAsin" : true,"brand" : true,"partNumber" : true}')
# # saveRDS(amazon_variation,'amazon_variation.Rds')
# amazon_variation_01 <- amazon_variation[complete.cases(amazon_variation$parentAsin),]
# parent_asin <- amazon_variation_01$parentAsin
# #first delete the duplicated asin by the parent asin
# amazon_variation_02 <- amazon_variation_01[amazon_variation_01$variationCSV!=amazon_variation_01$asin,]
# 
# 
# list_01 <- strsplit(amazon_variation_02$variationCSV,',')
# 
# emptyvector <- c()
# for (i in seq(1,length(list_01),1)) {
#   emptyvector <- append(emptyvector,c(list_01[[i]]))
# }
# ############# After deleating all the duplicated variation, then we can get all the unique data
# #?????????????????????????????
# amazon_variation_03 <- amazon_variation[!amazon_variation$asin %in% emptyvector,]
# amazon_variation_04 <- rbind(amazon_variation_03,amazon_variation_02)
# amazon_variation_04 <- amazon_variation_03[!duplicated(amazon_variation_04$asin),]
# saveRDS(amazon_variation_04,'amazon_variation_03.Rds')
# ######according to the variation data 3 , i will collect the unique sales and price data
# 
# 
# 
# amazon_price <- readRDS('amazon_price.Rds')
# amazon_sales <- readRDS('amazon_sales.Rds')
# amazon_brand <- readRDS('amazon_variation.Rds')
# raw_clean_data <- cbind(amazon_price,amazon_sales,amazon_brand)
# raw_clean_data <- raw_clean_data[,-c(1,3,7,10,14,17)]
# raw_clean_data['list_year'] <- format(raw_clean_data$listed_date,"%Y")
# saveRDS(raw_clean_data,'raw_clean_data.Rds')
# 
# #######ratings extracting###################
# con <- mongo(
#   collection = "amazon_vacuum_ratings_2",
#   url = "mongodb://Junjun:123456@localhost:27017/taobao",
#   verbose = FALSE,
#   options = ssl_options()
# )
# 
# amazon_detail <- con$find(fields = '{"asin" : true,"global_ratings" : true,"global_reviews" : true,
#                           "star5" : true,"star4" : true,"star3" : true,
#                           "star2" : true,"star1" : true}')
# 
# amazon_detail$global_reviews <- gsub('.*\\|',replacement = '',amazon_detail$global_reviews)
# saveRDS(amazon_detail,'amazon_rating_review_detail.Rds')
# 
# k <- readRDS('raw_clean_data.Rds')
# t <- readRDS('amazon_rating_review_detail.Rds')
# p <- readRDS('amazon_data_raw.Rds')
# k <- k[k$asin %in% t$asin,]
# t <- t[t$asin %in% k$asin,]
# p <- p[p$ASIN %in% t$asin,]
# FULL_DETAIL <- cbind(k,t)
# FULL_DETAIL <- cbind(FULL_DETAIL,p)
# FULL_DETAIL <- FULL_DETAIL[,-c(16,23,25,26)]
# 
# saveRDS(FULL_DETAIL,'FULL_AMAZON_DATA.Rds')
# 
# # amazon_detail -----------------------------------------------------------
# 
# amazon_data <- con1$find(fields = '{"ASIN" : true, "item_detail" : true,"detail_page_url" : true,"big_small_html" : true,"productOverview_feature" : true}')
# 
# amazon_data_no_duplicate <- amazon_data[!duplicated(amazon_data$ASIN),]
# 
# text1 <- amazon_data_no_duplicate$big_small_html
# 
# 
# # clean the amazon product detail technical detail data
# 
# text1 <- gsub('[{]','',text1)
# text2 <- gsub('[}]','',text1)
# text3 <- gsub('[(]','',text2)
# text4 <- gsub('[)]','',text3)
# text5 <- gsub('[;]','',text4)
# text6 <- gsub('[*]','',text5)
# text7 <- gsub('[||]','',text6)
# text8 <- gsub('https.*/','',text7)
# text9 <- gsub('[/]','',text8)
# text10 <- gsub('\\\"','',text9)
# text11 <- gsub('\'','',text10)
# text12 <- gsub(',','',text11)
# 
# list_feature <- strsplit(text12,'\\s{4,}')
# 
# detail_item <- data.frame(product_features=c("Product Dimensions","Item Weight","Batteries","Warranty Description","Batteries Required?",
#                                              "Included Components","Package Dimensions"))
# empty_array <- c(seq(1,7,1))
# for (j in seq(1,length(list_feature),1)) {
#   kkkk <- length(list_feature[[j]])
#   if(kkkk!=0){
#     for (i in seq(1,kkkk,1)) {
#       
#       if (list_feature[[j]][i] %in% detail_item$product_features){
#         location_1 <- which(detail_item$product_features==list_feature[[j]][i])
#         empty_array[location_1] <- list_feature[[j]][i+1]
#       }
#       
#     }
#   }
#   
#   name_arr <- sprintf('product_%d',j)
#   detail_item[name_arr] <-empty_array
#   empty_array <- c(seq(1,7,1))
# }
# kkk <- t(detail_item)
# ttt <- as.data.frame(kkk) 
# colnames(ttt) <- ttt[1,]
# lll <- ttt[-1,]
# # lll <- cbind(data3,ttt)
# # clean the amazon claim detail data
# text_item_detail <- amazon_data_no_duplicate$item_detail
# text_item_detail <- tolower(text_item_detail)
# text1 <- gsub('[{]','',text_item_detail)
# text2 <- gsub('[}]','',text1)
# text3 <- gsub('[(]','',text2)
# text4 <- gsub('[)]','',text3)
# text5 <- gsub('[;]','',text4)
# text6 <- gsub('[*]','',text5)
# text7 <- gsub('[||]','',text6)
# text8 <- gsub('https.*/','',text7)
# text9 <- gsub('[/]','',text8)
# text10 <- gsub('\\\"','',text9)
# text11 <- gsub('\'','',text10)
# text12 <- gsub(',','',text_item_detail)
# text13 <- gsub('\\s','',text12)
# suction <- str_extract(text13,'\\d{3,}pa')
# 
# jjjj <- data.frame(suction=suction)


#clean the product overview feature

# productOverview_feature <- amazon_data_no_duplicate$productOverview_feature
# text_item_detail <- tolower(productOverview_feature)
# list_feature <- strsplit(text_item_detail,'\\s{4,}')
# 
# detail_item <- data.frame(product_features=c("color","surface recommendation","controller type","battery cell composition"))
# empty_array <- c(seq(1,4,1))
# for (j in seq(1,length(list_feature),1)) {
#   kkkk <- length(list_feature[[j]])
#   if(kkkk!=0){
#     for (i in seq(1,kkkk,1)) {
#       
#       if (list_feature[[j]][i] %in% detail_item$product_features){
#         location_1 <- which(detail_item$product_features==list_feature[[j]][i])
#         empty_array[location_1] <- list_feature[[j]][i+1]
#       }
#       
#     }
#   }
#   
#   name_arr <- sprintf('product_%d',j)
#   detail_item[name_arr] <-empty_array
#   empty_array <- c(seq(1,4,1))
# }
# kkk <- t(detail_item)
# ttt <- as.data.frame(kkk) 
# colnames(ttt) <- ttt[1,]
# ttt <- ttt[-1,]
# 
# 
# 
# feature_vacuum_robot <- cbind(lll,jjjj,ttt)
# feature_vacuum_robot$`Package Dimensions`[which(feature_vacuum_robot$`Package Dimensions`==7)]=NA
# feature_vacuum_robot$`Included Components`[which(feature_vacuum_robot$`Included Components`==6)]=NA
# feature_vacuum_robot$`Batteries Required?`[which(feature_vacuum_robot$`Batteries Required?`==5)]=NA
# feature_vacuum_robot$`Warranty Description`[which(feature_vacuum_robot$`Warranty Description`==4)]=NA
# feature_vacuum_robot$Batteries[which(feature_vacuum_robot$Batteries==3)]=NA
# feature_vacuum_robot$`Item Weight`[which(feature_vacuum_robot$`Item Weight`==2)]=NA
# feature_vacuum_robot$`Product Dimensions`[which(feature_vacuum_robot$`Product Dimensions`==1)]=NA
# 
# feature_vacuum_robot$color[which(feature_vacuum_robot$color==1)]=NA
# feature_vacuum_robot$`surface recommendation`[which(feature_vacuum_robot$`surface recommendation`==2)]=NA
# feature_vacuum_robot$`controller type`[which(feature_vacuum_robot$`controller type`==3)]=NA
# feature_vacuum_robot$`battery cell composition`[which(feature_vacuum_robot$`battery cell composition`==4)]=NA
# 
# feature_vacuum_robot['product.dimension'] <- paste(feature_vacuum_robot$`Product Dimensions`,feature_vacuum_robot$`Package Dimensions`,sep = '')
# 
# 
# kk <- feature_vacuum_robot$product.dimension
# kk <- gsub('\\s','',kk)
# ttttt <- str_split(kk,'x')
# number_vector <- c()
# for (i in seq(1,length(ttttt),1)) {
#   FOR_LOOP <- as.numeric(gsub('[A-Za-z]','',ttttt[[i]]))
#   res <- 1
#   if(length(FOR_LOOP)!=0){
#     for (j in 1:length(FOR_LOOP)) {
#       res <- res * FOR_LOOP[j]
#     }
#   }
#   
#   number_vector <- append(number_vector,res)
#   
# }
# feature_vacuum_robot['numeric_product.dimension'] <- number_vector
# feature_vacuum_robot['Item_Weight'] <- gsub('[A-Za-z]','',feature_vacuum_robot$`Item Weight`)
# feature_vacuum_robot$asin <- amazon_data_no_duplicate$ASIN
# 
# saveRDS(feature_vacuum_robot,'amazon_data_detail.Rds')




