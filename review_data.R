
# Check the package -------------------------------------------------------
rm (list = ls(all=TRUE))
graphics.off()

if ('stringr' %in% rownames(installed.packages())==FALSE) {
  install.packages('stringr')
}else{
  library(stringr)
}
if ('dplyr' %in% rownames(installed.packages())==FALSE) {
  install.packages('dplyr')
}else{
  library(dplyr)
}

if ('RSQLite' %in% rownames(installed.packages())==FALSE) {
  install.packages('RSQLite')
}else{
  library(RSQLite)
}
if ('DBI' %in% rownames(installed.packages())==FALSE) {
  install.packages('DBI')
}else{
  library(DBI)
}

script_path <- rstudioapi::getActiveDocumentContext()$path
hash = "(.*)/"
setwd(str_extract(script_path, hash)) 
gc(T)


# con <- mongo(
#   collection = "amazon_vacuum_reviews1",
#   url = "mongodb://Junjun:123456@localhost:27017/taobao",
#   verbose = FALSE,
#   options = ssl_options()
# )
# find the parent asin ----------------------------------------------------
# kk <- con$find(fields = '{"asin" : true,"user_name" : true,"review_title" : true,"review_date" : true,"review_data" : true,"star_rating" : true,
#                               "review_country" : true}')
# saveRDS(amazon_variation,'amazon_variation.Rds')


# Only extract outside USA data -------------------------------------------
Outdata <- kk[kk$review_country!="the United States",]

### delete the completely duplicated row
Outdata <- Outdata %>% distinct()
Outdata$month_date <- format(as.Date(Outdata$review_date),format="%Y-%m")
Outdata$rate <- as.numeric(str_extract(Outdata$star_rating,"^\\d"))
kk <- Outdata



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

Ratings_data <- data.frame(asin=c(),Sumrevws=c(),Averate2=c(),market_ids=c())
for (i in seq(1,length(list_month),1)) {
  list <- unlist(list_month[i])
  rsubT_22_1qr <- kk[kk$month_date==list[1]|kk$month_date==list[2]|kk$month_date==list[3],]
  Market_1 <- rsubT_22_1qr %>% 
    group_by(asin,rate) %>%
    summarise(n=n()) %>%
    ungroup()
  
  
  Market_1 <- Market_1 %>%
    group_by(asin) %>%
    mutate(Sumrevws = sum(n)) %>%
    ungroup()
  
  
  Market_1 <- Market_1 %>%
    group_by(asin) %>%
    mutate(Averate2 = sum(rate*n)/Sumrevws) %>%
    ungroup()
  
  Market_1 <- Market_1 %>%
    select(asin,Sumrevws, Averate2) %>%
    distinct(asin, .keep_all =TRUE)
  
  Market_1$market_ids <- markit_ids[i]
  Ratings_data <- rbind(Ratings_data,Market_1)
}

saveRDS(Ratings_data,'outusa_data.Rds')








# merger the price and product characteritics data ------------------------
Ratings_data <- readRDS('./Rds_data/complete_data.Rds')
diff1 <- Ratings_data[complete.cases(Ratings_data$rating),]
diff1 <- diff1[order(diff1$asin,decreasing = F),]
diff1$N <- 1:length(diff1$asin)
diff2 <- diff1 %>%
  group_by(asin) %>%
  summarise(rating=c(diff(rating,lag = 1),NA),N=N) %>%
  ungroup()

diff3 <- merge(diff1,diff2,by='N')
diff3 <- diff3[,-c(11)] #delete the duplicate asin
diff3[which(is.na(diff3$rating.y),arr.ind = T),11] <- diff3[which(is.na(diff3$rating.y),arr.ind = T),9]
diff4 <- diff3[,-9]
diff4$rating.y <- abs(diff4$rating.y)
colnames(diff4)[10] <- 'rating'
colnames(diff4)[2] <- 'asin'
diff4 <- diff4[,-1]
# saveRDS(diff4,'./Rds_data/complete_data2.Rds')
# CLEAN THE DATA ----------------------------------------------------------
data1 <- diff4
data1$brand <- tolower(data1$brand)
data1$brand <- gsub('\\s','',data1$brand)
data1$brand <- gsub('[+]','',data1$brand)


data1$brand[which(data1$brand=='360www.360.cn',arr.ind = T)] <- '360'
data1$brand[which(data1$brand=='360smartnetwork',arr.ind = T)] <- '360'

saveRDS(data1,'./Expansion_version/data/data1.Rds')
# # blp IV ------------------------------------------------------------------
# 
# data1$suction <- gsub('pa','',data1$suction)
# 
# data1 <- data1 %>%
#   mutate(Nosurface = str_count(`surface recommendation`,','),
#          suction=as.numeric(suction),
#          Noconty = str_count(`controller type`,','))
# data1$Nosurface <- data1$Nosurface+1
# data1$Noconty <- data1$Noconty + 1
# 
# owner <- function(thisOwner){
#   owners <- as.numeric(factor(thisOwner, levels= unique(thisOwner)))
#   nprod <- length(thisOwner)
#   thisOwner <- matrix(0,ncol=nprod,nrow=nprod)
#   for( o in unique(owners)){
#     thisOwner [owners == o, owners == o] = 1
#   }
#   return(thisOwner)
# }
# 
# # data2 <- data1[!is.na(data1$suction),]
# data2 <- data1[!is.na(data1$numeric_product.dimension),]
# data2 <- data2[!is.na(data2$Item_Weight),]
# # data2 <- data2[!is.na(data2$Nosurface),]
# # data2 <- data2[!is.na(data2$Noconty),]
# data2 <- data2[!is.na(data2$prices),]
# data2 <- data2[!is.nan(data2$prices),]
# # data2$firm_ids <- .indexing.markets(data2$brand)
# # onwer_struc <- owner(data2$brand)\
# 
# 
# 
# ### deleate the small market data,there are only one firm data during these quarters
# table(data2$market_ids)#we can see the number of product in market is increasing thought the time.
# data3 <- data2[-which(data2$market_ids=='R15Q3',arr.ind = TRUE),]
# data3 <- data3[-which(data3$market_ids=='R15Q4',arr.ind = TRUE),]
# data3 <- data3[-which(data3$market_ids=='R16Q1',arr.ind = TRUE),]
# data3 <- data3[-which(data3$market_ids=='R16Q2',arr.ind = TRUE),]
# data3 <- data3[-which(data3$market_ids=='R16Q3',arr.ind = TRUE),]
# data3 <- data3[-which(data3$market_ids=='R16Q4',arr.ind = TRUE),]
# data3 <- data3[-which(data3$market_ids=='R17Q1',arr.ind = TRUE),]
# # data3 <- data3[-which(data3$market_ids=='R18Q1',arr.ind = TRUE),]
# # data3$firm_ids <- .indexing.markets(data3$brand)
# 
# 
# ### Put the same market data together
# # suction, product dimension, item weight,nonsurface,nonconty
# data4 <- data3[order(data3$market_ids),]
# data4$Item_Weight <- as.numeric(data4$Item_Weight)
# IVPARA <- data4[,c(23,24)]
# marid <- unique(data4$market_ids)
# toMarket <- matrix(0,nrow=dim(IVPARA)[1],ncol=dim(IVPARA)[2])
# for (i in unique(data4$market_ids)) {
#   sub <- IVPARA[data4$market_ids==i,]
#   same_firm <- owner(data4$brand[data4$market_ids==i])
#   z_1 = matrix(0,nrow=dim(sub)[1],ncol=dim(sub)[2])
#   for (j in 1:dim(IVPARA)[2]) {
#     z_1[, j] = t(matrix(sub[, j],nrow = 1) %*% same_firm)
#   }
#   toMarket[data4$market_ids==i, ] = z_1
# }
# 
# ### use the total of characteristics of other products produced by the same firm
# 
# iv_data1 <- toMarket-IVPARA
# 
# 
# ### the sum of characteristics of other goods produced by other firms.
# ### Here I need to rewrite the ownership structure function
# owner0 <- function(thisOwner){
#   owners <- as.numeric(factor(thisOwner, levels= unique(thisOwner)))
#   nprod <- length(thisOwner)
#   thisOwner <- matrix(1,ncol=nprod,nrow=nprod)
#   for( o in unique(owners)){
#     thisOwner [owners == o, owners == o] = 0
#   }
#   return(thisOwner)
# }
# toMarket0 <- matrix(0,nrow=dim(IVPARA)[1],ncol=dim(IVPARA)[2])
# for (i in unique(data4$market_ids)) {
#   sub <- IVPARA[data4$market_ids==i,]
#   same_firm <- owner0(data4$brand[data4$market_ids==i])
#   z_1 = matrix(0,nrow=dim(sub)[1],ncol=dim(sub)[2])
#   for (j in 1:dim(IVPARA)[2]) {
#     z_1[, j] = t(matrix(sub[, j],nrow = 1) %*% same_firm)
#   }
#   toMarket0[data4$market_ids==i, ] = z_1
# }
# iv_data2 <- as.data.frame(toMarket0)
# colnames(iv_data2) <- colnames(iv_data1)
# 
# 
# 
# ### Hausman and Nevo type instrument
# data5 <- data3[order(data3$asin),]
# 
# data6 <- data5 %>% 
#   group_by(asin) %>%
#   summarise(other_price=sum(prices),n=n()) %>%
#   ungroup()
# data7 <- merge(data5,data6,by='asin')
# 
# data7$demand_instruments4 <- (data7$other_price-data7$prices)/(data7$n-1)
# 
# 
# 
# # Combine blp and nevo iv together ----------------------------------------
# data8 <- data7[order(data7$market_ids),]
# colnames(iv_data1) <- paste('demand_instruments',0:1,sep = '')
# colnames(iv_data2) <- paste('demand_instruments',2:3,sep = '')
# Full_iv_data <- cbind(data8,iv_data1,iv_data2)
# Full_iv_data$firm_ids <- .indexing.markets(Full_iv_data$brand)#brand id needs to be solved
# 
# Full_iv_data$product_ids <- paste('F',Full_iv_data$firm_ids,'R',.indexing.markets(Full_iv_data$asin),sep = '')
# 
# 
# saveRDS(Full_iv_data,'Full_iv_data.Rds')
# 
# 
# 
# 
# 
# # 
# 
# data1 <- readRDS('numerical_data.Rds')
# Datafull <- subT[subT$asin %in% data1$asin,]
# 
# t <- table(Datafull$asin)
# tt <- as.data.frame.array(t)
# names(tt)[1] <- 'reviews'
# tt$asin <- row.names(tt)
# reviews <- tt
# 
# rates <- aggregate(Datafull$rate, by=list(type=Datafull$asin),mean)
# names(rates)[1] <- 'asin'
# 
# quater_data <- merge(reviews,rates,by="asin")
# 
# saveRDS(quater_data,'quater_data.Rds')

# ave.rating <- amazon_ave_rating[amazon_ave_rating$asin=='B09SKYW5XP'|amazon_ave_rating$asin=='B08T6JDDTY'|amazon_ave_rating$asin=='B08QCVWNSN',]
# amazon_price <- amazon_price[,-4]
# price <- amazon_price[amazon_price$asin=='B09SKYW5XP'|amazon_price$asin=='B08T6JDDTY'|amazon_price$asin=='B08QCVWNSN',]
# rating <- amazon_rating[amazon_rating$asin=='B09SKYW5XP'|amazon_rating$asin=='B08T6JDDTY'|amazon_rating$asin=='B08QCVWNSN',]
# amazon_sales <- amazon_sales[,-c(4,5,6)]
# amazon_sales <- amazon_sales[amazon_sales$asin=='B09SKYW5XP'|amazon_sales$asin=='B08T6JDDTY'|amazon_sales$asin=='B08QCVWNSN',]
# T <- cbind(price,rating,ave.rating,amazon_sales)
# library(openxlsx)
# write.xlsx(T,'sample_data.xlsx')
# 
# b <- kk[kk$asin=='B09SKYW5XP'|kk$asin=='B08T6JDDTY'|kk$asin=='B08QCVWNSN',]
# write.xlsx(b,'b.xlsx')
