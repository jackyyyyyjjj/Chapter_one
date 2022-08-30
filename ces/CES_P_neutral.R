#' ---
#' title: "Merger simulation"
#' author: "Junjun Zhang"
#' date: "2022/5/9"
#' output:
#'   html_document: default
#'   pdf_document: default
#' ---


#' 
#' 
#' 
## ----include=TRUE-------------------------------------------------------------------------
# Check  whether the packages called below require installation.
rm (list = ls(all=TRUE))#Clean objects from the workspace
graphics.off() #Close the graphics
#Simple, Consistent Wrappers for Common String Operations
if ('stringr' %in% rownames(installed.packages())==FALSE) {
  install.packages('stringr')
}else{
  library(stringr)
}
# library(xlsx)

if ('openxlsx' %in% rownames(installed.packages())==FALSE) {
  install.packages('openxlsx')
}else{
  library(openxlsx)
}
#Extract this Rmd file's working directory
if (!'rstudioapi' %in% rownames(installed.packages())) {
  install.packages('rstudioapi')
}else{
  library(rstudioapi)
}

#linearHypothesis
if (!'car' %in% rownames(installed.packages())) {
  install.packages('car')
}else{
  library(car)
}
#Export the regression result to latex format tables
if (!'stargazer' %in% rownames(installed.packages())) {
  install.packages('stargazer')
}else{
  library(stargazer)
}
#Connect to pyblp python package
if (!'reticulate' %in% rownames(installed.packages())) {
  install.packages('reticulate')
}else{
  library(reticulate)
}
#Visulaize the missing values in the dataframe
if (!'VIM' %in% rownames(installed.packages())) {
  install.packages('VIM')
}else{
  library(VIM)
}
#  A Grammar of Data Manipulation
if (!'dplyr' %in% rownames(installed.packages())) {
  install.packages('dplyr')
}else{
  library(dplyr)
}
#Fast and User-Friendly Fixed-Effects Estimations
if (!'fixest' %in% rownames(installed.packages())) {
  install.packages('fixest')
}else{
  library(fixest)
}

library("ivreg")

if ('DBI' %in% rownames(installed.packages())==FALSE) {
  install.packages('DBI')
}else{
  library(DBI)
}
gc(T)
#' 
#' # Set the working directory
## ----analysis,include=TRUE----------------------------------------------------------------
script_path <- rstudioapi::getActiveDocumentContext()$path
hash = "(.*)/"
setwd(str_extract(script_path, hash)) 

# START -------------------------------------------------------------------
# Outdata <- kk[kk$review_country!="the United States",]
# selec <- Outdata %>%
#   group_by(review_country) %>%
#   mutate(numPro = length(unique(asin))) %>%
#   ungroup()




# outusa_data <- readRDS('../data/outusa_data.Rds') other countries data
# price_sales_rating_quarter <- readRDS('../data/price_sales_rating_quarter.Rds')
# data1 <- read.csv('product.dea3_NO_EMSUCTION4.csv')
# data2 <- data1[,-c(1:2,10:18)]
# data3 <- data2[,-c(3:4)]
# Full_data <- merge(price_sales_rating_quarter,data3,by='asin',all.x = T)
# 
# 
# 
# 
# 
# # Missing value check -----------------------------------------------------
# # use aggr to visualize the missing value
# library('VIM')
# aggr(Full_data[,c(7:8,10,13,14)],prop=FALSE,numbers=TRUE)
# 
# 
# 
# # combine with the estimated sales data -----------------------------------
# Full_data2 <- Full_data[!(is.na(Full_data$sales)|is.na(Full_data$prices)|is.na(Full_data$ave_rating)),]
# Full_data2$COM <- paste(Full_data2$asin,Full_data2$market_ids,sep = '')
# 
# 
# conn <- dbConnect(RSQLite::SQLite(), "../SALES/amazon_sales.sqlite")
# amazon_sales <- dbGetQuery(conn, "SELECT asin,market_ids,sales from estsales")
# 
# library(stringr)
# 
# dbDisconnect(conn)

# amazon_sales$COM <- paste(amazon_sales$asin,amazon_sales$market_ids,sep = '')
# 
# 
# Full_data3 <- merge(Full_data2,amazon_sales,by='COM',all.x = T)
# 
# write.csv(Full_data3,'Full_data3.csv')

Full_data3 <- read.csv('Full_data3.csv')

#the more product characteristics
data.m <- readRDS('data2.Rds')
data.m1 <- data.m %>% distinct(asin,.keep_all =TRUE)



Full_data_m <- Full_data3[Full_data3$asin %in% data.m1$asin,]
Full_data3 <- Full_data3[Full_data3$Brand %in% Full_data_m$Brand,]

#pick only length or width here widtu
Full_data3 <- merge(Full_data3,data.m1[,c(1,11:13)],by='asin',all.x = T)
# write.csv(Full_data3,'Full_data3.csv')
Full_data3 <- read.csv('Full_PROC.csv')
Full_data3$year <- str_extract(Full_data3$listed_date,'\\d{4}')
Full_data3$year <- as.numeric(Full_data3$year) 
Full_data3$pro_age <- 2022-as.numeric(Full_data3$year)


# stargazer(Full_data3)

#Histogram

# hist(Full_data3$prices,  xlab = "prices",  main = "Histogram of prices",  breaks = sqrt(nrow(Full_data3))) # set number of bins
# hist(Full_data3$ave_rating,  xlab = "Average rating",  main = "Histogram of average rating",  breaks = sqrt(nrow(Full_data3))) # set number of bins
# hist(Full_data3$sales,  xlab = "Sales",  main = "Histogram of sales",  breaks = sqrt(nrow(Full_data3))) # set number of bins
# hist(Full_data3$suction,  xlab = "suction",  main = "Histogram of suction",  breaks = sqrt(nrow(Full_data3))) # set number of bins
# hist(Full_data3$Wok_1charge,  xlab = "Battery life",  main = "Histogram of battery life",  breaks = sqrt(nrow(Full_data3))) # set number of bins
# hist(Full_data3$pro_age,  xlab = "Product age",  main = "Histogram of product age",  breaks = sqrt(nrow(Full_data3))) # set number of bins


# library(ggplot2)
# ggplot(dat) +  aes(x = hwy) +  geom_histogram(bins = 30L, fill = "#0c4c8a") +  theme_minimal()



#clean the missing value
# outusa_data <- readRDS('../data/outusa_data.Rds')
# price_sales_rating_quarter <- readRDS('../data/price_sales_rating_quarter.Rds')
# data1 <- read.csv('product.dea3_NO_EMSUCTION4.csv')
# data2 <- data1[,-c(1:2,10:18)]
# nonmissing_price_sales <- price_sales_rating_quarter[price_sales_rating_quarter$sales,]
# nonmissing_price_sales <- price_sales_rating_quarter[!(is.na(price_sales_rating_quarter$sales)|is.na(price_sales_rating_quarter$prices)|is.na(price_sales_rating_quarter$ave_rating)),]
# nonmissing_price_sales <- nonmissing_price_sales[!(is.infinite(nonmissing_price_sales$sales)|is.infinite(nonmissing_price_sales$prices)),]
# 
# #put the estimated sales data with the nonmissing
# nonmissing_price_sales$combine <- paste(nonmissing_price_sales$asin,nonmissing_price_sales$market_ids,sep = '')
# data3 <- data2[,-c(3:4)]
# Full_data <- merge(nonmissing_price_sales,data3,by='asin',all.x = T)

# outusa_data$combine <- paste(outusa_data$asin,outusa_data$market_ids,sep = '')
# outusa_data <- outusa_data[,-c(1,4)]
# 
# Full_data2 <- merge(Full_data,outusa_data,by='combine',all.x = T)




# data2$prices <- log(data2$prices)
# write.csv(data2,'rawdata.csv')



# Outliers analysis -------------------------------------------------------
#In addition to histograms, boxplots are also useful to detect potential outliers.
boxplot(Full_data3$prices,  ylab = "prices",  
        main = "Boxplot of prices")
boxplot.stats(Full_data3$prices)$out #outliers
a<-which(Full_data3$prices %in% boxplot.stats(Full_data3$prices)$out) 
Full_data4 <- Full_data3[-c(a),]





boxplot(Full_data4$ave_rating,  ylab = "Average rating",  
        main = "Boxplot of Average rating")
boxplot.stats(Full_data4$ave_rating)$out #outliers
a<-which(Full_data4$ave_rating %in% boxplot.stats(Full_data4$ave_rating)$out) 
Full_data4 <- Full_data4[-c(a),]



boxplot(Full_data4$suction,  ylab = "suction",  
        main = "Boxplot of suction")
boxplot.stats(Full_data4$suction)$out #outliers
a<-which(Full_data4$suction %in% boxplot.stats(Full_data4$suction)$out) 
Full_data4 <- Full_data4[-c(a),]


boxplot(Full_data4$Wok_1charge,  ylab = "Battery life",  
        main = "Boxplot of battery life")
boxplot.stats(Full_data4$Wok_1charge)$out #outliers
a<-which(Full_data4$Wok_1charge %in% boxplot.stats(Full_data4$Wok_1charge)$out) 
Full_data4 <- Full_data4[-c(a),]



boxplot(Full_data4$sales,  ylab = "Sales",  
        main = "Boxplot of sales")
boxplot.stats(Full_data4$sales)$out #outliers
a<-which(Full_data4$sales %in% boxplot.stats(Full_data4$sales)$out) 
Full_data4 <- Full_data4[-c(a),]



boxplot(Full_data4$pro_age,  ylab = "PROAGE",  
        main = "Boxplot of product age")
boxplot.stats(Full_data4$pro_age)$out #outliers
a<-which(Full_data4$pro_age %in% boxplot.stats(Full_data4$pro_age)$out) 
Full_data4 <- Full_data4[-c(a),]



Full_data3 <- Full_data4
# stargazer(Full_data3[,c(7,8,11:13,15)])

# Correlation test --------------------------------------------------------
library(ggcorrplot)
library(ggplot2)
# corrdata <- Full_data3[,c(7,8,11,12,13,15)]
# cormtvacuum <- round(cor(corrdata), 3)
# ggcorrplot(cormtvacuum,method = "circle",lab=T)


#check how many products in each market
promar <- as.data.frame(table(Full_data3$market_ids))
stargazer(promar,summary = F,type = 'text')


usahousehold_number <- read.xlsx('../data/usahousehold_number.xlsx',sheet = 'quar')



# data1 <- data1[!(data1$market_ids=='R16Q3'|data1$market_ids=='R16Q4'|data1$market_ids=='R17Q1'|
#                   data1$market_ids=='R17Q2'|data1$market_ids=='R17Q3'|data1$market_ids=='R17Q4'),]
Full_data4 <- merge(Full_data3,usahousehold_number,by='market_ids',all.x = T)



# 
# 
# data2 <- data2[!(data2$market_ids=='2016-07'|data2$market_ids=='2016-08'|data2$market_ids=='2016-10'|
#                  data2$market_ids=='2016-11'|data2$market_ids=='2016-12'|data2$market_ids=='2017-01'|
#                  data2$market_ids=='2017-02'|data2$market_ids=='2017-03'|data2$market_ids=='2017-04'),]

PROCESS <- Full_data4 %>%
  group_by(market_ids,Firm,Brand) %>%
  summarise(Averate = mean(ave_rating),
            Prices = mean(prices),
            sales = sum(sales),
            itemWidth = mean(itemWidth),
            itemHeight = mean(itemHeight),
            itemWeight = mean(itemWeight),
            NO.brand = n(),
            Wok_1charge = mean(Wok_1charge),
            suction = mean(suction),
            pro_age = max(pro_age),
            # num.pro = ifelse(n()>1,1,0),
            Fra_income = mean(Fra_income)
  ) %>%
  ungroup()
# JUST DELETE THE ZERO SHARES WHICH i NEED TO IMPROVE IN THE FUTURE
# data2 <- data2[!data2$rating==0,]
Full_data5 <- as.data.frame(PROCESS)

#summary result
# stargazer(Full_data5)



# 
# stargazer(data2,summary = TRUE,digits=2)


owner <- function(thisOwner){
  owners <- as.numeric(factor(thisOwner, levels= unique(thisOwner)))
  nprod <- length(thisOwner)
  thisOwner <- matrix(0,ncol=nprod,nrow=nprod)
  for( o in unique(owners)){
    thisOwner [owners == o, owners == o] = 1
  }
  return(thisOwner)
}
# Form the id for charcter value function
.indexing.markets <- function( cdidOld ) {
  cdidOld <- as.character(cdidOld)
  unique.ids <-  unique(cdidOld)
  nmkt <- length(unique.ids)
  cdid <- numeric(length(cdidOld))
  
  for(i in 1:nmkt ){
    relevantMarkets <- cdidOld == unique.ids[i]
    cdid[ relevantMarkets ] <- i
    
  }
  return( cdid )
}





Full_data5$firm_ids <- .indexing.markets(Full_data5$Firm)
Full_data5$product_ids <- .indexing.markets(Full_data5$Brand)

#### Blp type iv
constructIV <- function(firmid, cdid, id, X) {
  n <- dim(X)[1]
  p <- dim(X)[2]
  names <- colnames(X)
  if (is.null(names)) names <- paste("V", 1:p, sep="")
  sum.other <-  matrix(NA, nrow = n, ncol = p)
  colnames(sum.other) <- paste("sum.o.", names, sep="")  # sum other
  sum.rival <-  matrix(NA, nrow = n, ncol = p)
  colnames(sum.rival) <- paste("sum.r.", names, sep="")  # sum rival
  
  for (i in 1:n) {
    for (j in 1:p) {
      other_ind=(firmid==firmid[i] & cdid==cdid[i] & id!=id[i])
      rival_ind=(firmid!=firmid[i] & cdid==cdid[i])
      
      sum.other[i,j]=sum(X[other_ind==1,j])
      sum.rival[i,j]=sum(X[rival_ind==1,j]) 
    }
  }
  return(cbind(sum.other, sum.rival))
}
##work1charge,suction,item-weight,product dimension IV
blp_iv <- constructIV(Full_data5[,15],Full_data5[,1],Full_data5[,16],Full_data5[,c(7:13)])
##work1charge,suction,PRO AGE,RATINGS IV
# blp_iv <- constructIV(data2[,16],data2[,1],data2[,17],data2[,c(3:4,9,15)])
Full_data6 <- cbind(Full_data5,blp_iv)


datatest <- Full_data6


# Now use usa data to represent the potential market data -----------------

sharesout <- datatest %>%
  group_by(market_ids) %>%
  summarise(s0=sum(sales,na.rm = T)) %>%
  ungroup()
# shares <- datatest %>% 
#   group_by(market_ids) %>%
#   summarise(shares=sum(rating,na.rm = T)) %>%
#   ungroup()
datatest2 <- merge(datatest,sharesout,by='market_ids',all.x = T)
datatest2$shares <- datatest2$sales/datatest2$Fra_income
datatest2$shares0 <- 1-datatest2$s0/datatest2$Fra_income
datatest2$lgs.s0 <- log(datatest2$shares)-log(datatest2$shares0) ## Do I need to line up the same form as the estimation form, what is the quatity  and income in my model.


# check whether the quality is endogenous ---------------------------------
#Durbin-Wu-Hausman test to check the endogenous variable










####  Firm id dummy and Market id dummy
dummycol <- data.frame(datatest2$product_ids)
colnames(dummycol) <- c("product")
library(fastDummies)
test.df <- dummy_cols(dummycol, select_columns = "product", remove_first_dummy = T)
test.df <- test.df[,-1]
datatest3 <- cbind(datatest2, test.df)

#quarter fixed effect
dummycol_time <- data.frame(datatest3$market_ids)
colnames(dummycol_time) <- c("quarYear")
library(fastDummies)
test.df_time <- dummy_cols(dummycol_time, select_columns = "quarYear", remove_first_dummy = T)
test.df_time1 <- test.df_time[,-1]
datatest4 <- cbind(datatest3, test.df_time1)
names <- tibble(test.df,test.df_time1)
name <- colnames(names)

datatest4$Prices <- log(datatest4$Prices)



# ols
# m_ols <- lm(shares ~ .,data =datatest4[,-c(1:2,5,8,9,11,12)])
##Only keep sum.o.suction, sum.o.Wok_1charge and sum.o.pro_age
# test <- datatest4[,-c(1:6,9,14:17,19,21,23,24,39:41)]
# test <- datatest4[,-c(1:3,6,11:13,20:22)] #with num.pro
test <- datatest4[,-c(1:3,6,14:16,31:33)] #with num.pro
# I will consider the rating as the IV ------------------------------------
# cormtvacuum2 <- round(cor(test[,-c(14:500)]), 3)
# ggcorrplot(cormtvacuum2,method = "circle",lab=T)


# test <- datatest4[,-c(1:2,5,6,10:13,14,16:17,26:28)]

#delete sum.o.Wok_1chargesum.r.Wok_1charge,sum.r.suction 12******     take this one*******
# test <- datatest4[,-c(1:2,5,8,9,10:11,13:14)]
# m_iv_put <- ivreg(shares ~ .-sum.o.suction   -sum.o.pro_age -sum.r.pro_age  |  .-prices -ave_rating, data = test[,-c(7,9:10)])
# stargazer(m_iv_put,omit =name,type = 'text')
# summary(m_iv_put,test = TRUE)
# 
# stage1_price <- lm(prices~ .-shares -ave_rating,data =test[,-c(7,9:10)])
# # summary(stage1_price)
# 
# stage1_quality <- lm(ave_rating~ .-prices -shares,data = test[,-c(7,9:10)])
# # summary(stage1_quality)
# # names <- tibble(test.df,test.df_time1)
# # name <- colnames(names)
# stargazer(stage1_price,stage1_quality,omit =name,type = 'text')

# k <- feols(lgs.s0 ~ Wok_1charge + suction + pro_age|product_ids + market_ids|Averate + Prices ~sum.o.pro_age+sum.r.Wok_1charge+sum.r.pro_age ,datatest4)
# etable(k)
# k <- feols(lgs.s0 ~  itemHeight + itemWeight+num.pro+ Wok_1charge +suction +pro_age|product_ids + market_ids|Averate + Prices ~sum.o.itemWeight+sum.o.suction+sum.o.pro_age,datatest4)
# etable(k)
# k <- feols(lgs.s0 ~ itemWidth  + itemWeight+ Wok_1charge +suction +pro_age|product_ids + market_ids|Averate + Prices ~sum.o.itemWeight+sum.o.suction+sum.o.pro_age,datatest4)
# etable(k)
# k <- feols(lgs.s0 ~ itemWidth + itemHeight + itemWeight+num.pro+ Wok_1charge +suction |product_ids + market_ids|Averate + Prices ~sum.o.itemWeight+sum.o.suction+sum.o.pro_age,datatest4)
# etable(k)
#here find pro_age is a problem
# three iv ----------------------------------------------------------------
set.seed(100)
x <- 10:23
# combn(x,3)
b <- t(combn(x,3))
testnames <- colnames(test)
right <- matrix(1:15,nrow = 1,ncol = 15)
wt <- c((.8/3),(.8/3),(.8/3), 0.2)
# colnames(test)[1] <- 'Prices'
# colnames(test)[2] <- 'Averate'
#1best 3,2best 2.2,

for (i in seq(1,length(b[,1]),1)) {
  i <- i
  skip_to_next <- FALSE
  #IV regression
  formula <- as.formula(sprintf('lgs.s0 ~.-%s -%s -%s | .-%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],'Prices','Averate'))
  # m_iv <- ivreg(formula, data = test[,c(1:7,b[i,1],b[i,2],b[i,3],13:500)])
  tryCatch({
    m_iv <- ivreg(formula, data = test[,c(1:9,b[i,1],b[i,2],b[i,3],24:122)])
    summ_m_iv <- summary(m_iv,diagnostics = TRUE)
  },
  error=function(e) {
    # message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(e)
    skip_to_next <<- TRUE
  })
  print(i)
  if(skip_to_next) { next }
  
  # #scores
  WeakIV_1 <- ifelse(summ_m_iv$diagnostics[1, "p-value"] < 0.01, 3,
                     ifelse(0.01< summ_m_iv$diagnostics[1, "p-value"] & summ_m_iv$diagnostics[1, "p-value"] < 0.05, 2,
                            ifelse(0.05 < summ_m_iv$diagnostics[1, "p-value"] & summ_m_iv$diagnostics[1, "p-value"] < 0.1, 1, 0)))
  WeakIV_2 <- ifelse(summ_m_iv$diagnostics[2, "p-value"] < 0.01, 3,
                     ifelse(0.01< summ_m_iv$diagnostics[2, "p-value"] & summ_m_iv$diagnostics[2, "p-value"] < 0.05, 2,
                            ifelse(0.05 < summ_m_iv$diagnostics[2, "p-value"] & summ_m_iv$diagnostics[2, "p-value"] < 0.1, 1, 0)))
  
  WuHausman <- ifelse(summ_m_iv$diagnostics[3, "p-value"] < 0.01, 3,
                      ifelse(0.01< summ_m_iv$diagnostics[3, "p-value"] & summ_m_iv$diagnostics[3, "p-value"] < 0.05, 2,
                             ifelse(0.05 < summ_m_iv$diagnostics[3, "p-value"] & summ_m_iv$diagnostics[3, "p-value"] < 0.1, 1, 0)))
  
  Sargan <- ifelse(summ_m_iv$diagnostics[4, "p-value"] < 0.01, 3,
                   ifelse(0.01< summ_m_iv$diagnostics[4, "p-value"] & summ_m_iv$diagnostics[4, "p-value"] < 0.05, 2,
                          ifelse(0.05 < summ_m_iv$diagnostics[4, "p-value"] & summ_m_iv$diagnostics[4, "p-value"] < 0.1, 1, 0)))
  
  
  score  <- sum(wt[1]*WeakIV_1,
                wt[2]*WeakIV_2,
                wt[3]*WuHausman,
                wt[4]*Sargan)
  # 
  # 
  # #First Stage regression
  formula_lh_endo_1 <- as.formula(sprintf('Prices ~.+%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],'lgs.s0','Averate'))
  formula_lh_endo_2 <- as.formula(sprintf('Averate ~.+%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],'Prices','lgs.s0'))
  fs_m_iv_endo1 <- lm(formula_lh_endo_1, data = test[,c(1:9,b[i,1],b[i,2],b[i,3],24:122)])
  fs_m_iv_endo2 <- lm(formula_lh_endo_2, data = test[,c(1:9,b[i,1],b[i,2],b[i,3],24:122)])
  # 
  lh_vec1 <- colnames(test[,c(b[i,1],b[i,2],b[i,3])])
  lh_vec2 <- paste(lh_vec1, "= 0", sep = " ")
  #later need to test the linearity in the test,, the linearHypothesis used to test where iv joint test is significat, the F score should be higher than 10
  lh_m_iv_endo1 <- linearHypothesis(fs_m_iv_endo1, c(lh_vec2[1], lh_vec2[2], lh_vec2[3]), test="F", type=HC1, singular.ok = T)
  lh_m_iv_endo2 <- linearHypothesis(fs_m_iv_endo2, c(lh_vec2[1], lh_vec2[2], lh_vec2[3]), test="F", type=HC1, singular.ok = T)
  
  
  # if (m_iv$coefficients[4]<0 & m_iv$coefficients[5]>0 &
  #     lh_m_iv_endo1$F[2] > 10 & lh_m_iv_endo2$F[2] > 10 ){
  if (m_iv$coefficients[3]<0 & m_iv$coefficients[2]>0){
    coeff <- summ_m_iv$coefficients
    diag <- summ_m_iv$diagnostics
    # Here I filter the result accoring to iv test and coefficient significant test
    
    right <- rbind(right,c(i,testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],coeff[2,1],coeff[3,1],coeff[2,4],coeff[3,4],diag[1,4],
                           diag[2,4],diag[3,4],diag[4,4],lh_m_iv_endo1$F[2], lh_m_iv_endo2$F[2], score))
    cat(c(i,testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],coeff[2,1],coeff[3,1],coeff[2,4],coeff[3,4],diag[1,4],
          diag[2,4],diag[3,4],diag[4,4],lh_m_iv_endo1$F[2], lh_m_iv_endo2$F[2], score))
    #stargazer(m_iv,omit =name,type = 'text')
    
  }
}
#5,7,9,12,13,14,16,18
Sum_dataframe <- as.data.frame(right)
colnames(Sum_dataframe) <- c('b','iv1','iv2','iv3','quality_coef','price_coef','quality_coef_Pvalue','price_coef_Pvalue',
                             'WeakIV_1','WeakIV_2','WuHausman','Sargan','iv_endo1_Ftest','iv_endo1_Ftes2','score')
Sum_dataframe <- Sum_dataframe[-1,]



# # 4 ivs -------------------------------------------------------------------
# 
# 
# x <-  7:12
# b <- t(combn(x,4))
# testnames <- colnames(test)
# right <- matrix(1:15,nrow = 1,ncol = 15)
# wt <- c((.8/3),(.8/3),(.8/3), 0.2)
# 
# #1best 3,2best 2.2,
# 
# for (i in seq(1,length(b[,1]),1)) {
#   skip_to_next <- FALSE
#   #IV regression
#   formula <- as.formula(sprintf('lgs.s0 ~.-%s -%s -%s -%s| .-%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],'Prices','Averate'))
#   tryCatch({
#     m_iv <- ivreg(formula, data = test[,c(1:7,b[i,1],b[i,2],b[i,3],b[i,4],13:500)])
#     summ_m_iv <- summary(m_iv,diagnostics = TRUE)
#   },
#   error=function(e) {
#     # message(paste("URL does not seem to exist:", url))
#     message("Here's the original error message:")
#     message(e)
#     skip_to_next <<- TRUE
#   })
#   print(i)
#   if(skip_to_next) { next }
#   
#   
#   
#   #scores
#   WeakIV_1 <- ifelse(summ_m_iv$diagnostics[1, "p-value"] < 0.01, 3, 
#                      ifelse(0.01< summ_m_iv$diagnostics[1, "p-value"] & summ_m_iv$diagnostics[1, "p-value"] < 0.05, 2, 
#                             ifelse(0.05 < summ_m_iv$diagnostics[1, "p-value"] & summ_m_iv$diagnostics[1, "p-value"] < 0.1, 1, 0)))
#   WeakIV_2 <- ifelse(summ_m_iv$diagnostics[2, "p-value"] < 0.01, 3, 
#                      ifelse(0.01< summ_m_iv$diagnostics[2, "p-value"] & summ_m_iv$diagnostics[2, "p-value"] < 0.05, 2, 
#                             ifelse(0.05 < summ_m_iv$diagnostics[2, "p-value"] & summ_m_iv$diagnostics[2, "p-value"] < 0.1, 1, 0)))
#   
#   WuHausman <- ifelse(summ_m_iv$diagnostics[3, "p-value"] < 0.01, 3, 
#                       ifelse(0.01< summ_m_iv$diagnostics[3, "p-value"] & summ_m_iv$diagnostics[3, "p-value"] < 0.05, 2, 
#                              ifelse(0.05 < summ_m_iv$diagnostics[3, "p-value"] & summ_m_iv$diagnostics[3, "p-value"] < 0.1, 1, 0)))  
#   
#   Sargan <- ifelse(summ_m_iv$diagnostics[4, "p-value"] < 0.01, 3, 
#                    ifelse(0.01< summ_m_iv$diagnostics[4, "p-value"] & summ_m_iv$diagnostics[4, "p-value"] < 0.05, 2, 
#                           ifelse(0.05 < summ_m_iv$diagnostics[4, "p-value"] & summ_m_iv$diagnostics[4, "p-value"] < 0.1, 1, 0)))
#   
#   
#   score  <- sum(wt[1]*WeakIV_1, 
#                 wt[2]*WeakIV_2,
#                 wt[3]*WuHausman,
#                 wt[4]*Sargan)
#   
#   
#   #First Stage regression
#   formula_lh_endo_1 <- as.formula(sprintf('Prices ~.+%s +%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],'lgs.s0','Averate'))
#   formula_lh_endo_2 <- as.formula(sprintf('Averate ~.+%s +%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],'Prices','lgs.s0'))
#   fs_m_iv_endo1 <- lm(formula_lh_endo_1, data = test[,c(1:7,b[i,1],b[i,2],b[i,3],b[i,4],13:500)])
#   fs_m_iv_endo2 <- lm(formula_lh_endo_2, data = test[,c(1:7,b[i,1],b[i,2],b[i,3],b[i,4],13:500)])
#   
#   lh_vec1 <- colnames(test[,c(b[i,1],b[i,2],b[i,3],b[i,4])]) 
#   lh_vec2 <- paste(lh_vec1, "= 0", sep = " ")
#   #later need to test the linearity in the test,, the linearHypothesis used to test where iv joint test is significat, the F score should be higher than 10
#   lh_m_iv_endo1 <- linearHypothesis(fs_m_iv_endo1, c(lh_vec2[1], lh_vec2[2], lh_vec2[3],lh_vec2[4]), test="F", type=HC1, singular.ok = T)
#   lh_m_iv_endo2 <- linearHypothesis(fs_m_iv_endo2, c(lh_vec2[1], lh_vec2[2], lh_vec2[3],lh_vec2[4]), test="F", type=HC1, singular.ok = T)
#   
#   
#   if (m_iv$coefficients[3]<0 & m_iv$coefficients[2]>0 ){
#     coeff <- summ_m_iv$coefficients
#     diag <- summ_m_iv$diagnostics
#     # Here I filter the result accoring to iv test and coefficient significant test
#     
#     right <- rbind(right,c(testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],coeff[2,1],coeff[3,1],coeff[2,4],coeff[3,4],diag[1,4], 
#                            diag[2,4],diag[3,4],diag[4,4],lh_m_iv_endo1$F[2], lh_m_iv_endo2$F[2], score)) 
#     cat(c(testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],coeff[2,1],coeff[3,1],coeff[2,4],coeff[3,4],diag[1,4], 
#           diag[2,4],diag[3,4],diag[4,4],lh_m_iv_endo1$F[2], lh_m_iv_endo2$F[2], score))
#     #stargazer(m_iv,omit =name,type = 'text')
#     
#   }
# }
# Sum_dataframe_4iv <- as.data.frame(right)
# colnames(Sum_dataframe_4iv) <- c('iv1','iv2','iv3','iv4','quality_coef','price_coef','quality_coef_Pvalue','price_coef_Pvalue',
#                              'WeakIV_1','WeakIV_2','WuHausman','Sargan','iv_endo1_Ftest','iv_endo1_Ftes2','score')
# Sum_dataframe_4iv <- Sum_dataframe_4iv[-1,]
# write.csv(Sum_dataframe_4iv,'Sum_dataframe_4iv.csv')
# saveRDS(Sum_dataframe_4iv,'Sum_dataframe_4iv.Rds')
# 
# # 5 ivs -------------------------------------------------------------------
# 
# 
# x <- 9:20
# b <- t(combn(x,5))
# testnames <- colnames(test)
# right <- matrix(1:16,nrow = 1,ncol = 16)
# wt <- c((.8/3),(.8/3),(.8/3), 0.2)
# 
# #1best 3,2best 2.2,
# 
# for (i in seq(1,length(b[,1]),1)) {
#   #IV regression
#   formula <- as.formula(sprintf('lgs.s0 ~.-%s -%s -%s -%s -%s| .-%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],testnames[b[i,5]],'Prices','Averate'))
#   m_iv <- ivreg(formula, data = test[,c(1:8,b[i,1],b[i,2],b[i,3],b[i,4],b[i,5],21:184)])
#   summ_m_iv <- summary(m_iv,diagnostics = TRUE)
#   #scores
#   WeakIV_1 <- ifelse(summ_m_iv$diagnostics[1, "p-value"] < 0.01, 3, 
#                      ifelse(0.01< summ_m_iv$diagnostics[1, "p-value"] & summ_m_iv$diagnostics[1, "p-value"] < 0.05, 2, 
#                             ifelse(0.05 < summ_m_iv$diagnostics[1, "p-value"] & summ_m_iv$diagnostics[1, "p-value"] < 0.1, 1, 0)))
#   WeakIV_2 <- ifelse(summ_m_iv$diagnostics[2, "p-value"] < 0.01, 3, 
#                      ifelse(0.01< summ_m_iv$diagnostics[2, "p-value"] & summ_m_iv$diagnostics[2, "p-value"] < 0.05, 2, 
#                             ifelse(0.05 < summ_m_iv$diagnostics[2, "p-value"] & summ_m_iv$diagnostics[2, "p-value"] < 0.1, 1, 0)))
#   
#   WuHausman <- ifelse(summ_m_iv$diagnostics[3, "p-value"] < 0.01, 3, 
#                       ifelse(0.01< summ_m_iv$diagnostics[3, "p-value"] & summ_m_iv$diagnostics[3, "p-value"] < 0.05, 2, 
#                              ifelse(0.05 < summ_m_iv$diagnostics[3, "p-value"] & summ_m_iv$diagnostics[3, "p-value"] < 0.1, 1, 0)))  
#   
#   Sargan <- ifelse(summ_m_iv$diagnostics[4, "p-value"] < 0.01, 3, 
#                    ifelse(0.01< summ_m_iv$diagnostics[4, "p-value"] & summ_m_iv$diagnostics[4, "p-value"] < 0.05, 2, 
#                           ifelse(0.05 < summ_m_iv$diagnostics[4, "p-value"] & summ_m_iv$diagnostics[4, "p-value"] < 0.1, 1, 0)))
#   
#   
#   score  <- sum(wt[1]*WeakIV_1, 
#                 wt[2]*WeakIV_2,
#                 wt[3]*WuHausman,
#                 wt[4]*Sargan)
#   
#   
#   #First Stage regression
#   formula_lh_endo_1 <- as.formula(sprintf('Prices ~.+%s +%s +%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],testnames[b[i,5]],'lgs.s0','Averate'))
#   formula_lh_endo_2 <- as.formula(sprintf('Averate ~.+%s +%s +%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],testnames[b[i,5]],'Prices','lgs.s0'))
#   fs_m_iv_endo1 <- lm(formula_lh_endo_1, data = test[,c(1:8,b[i,1],b[i,2],b[i,3],b[i,4],b[i,5],21:184)])
#   fs_m_iv_endo2 <- lm(formula_lh_endo_2, data = test[,c(1:8,b[i,1],b[i,2],b[i,3],b[i,4],b[i,5],21:184)])
#   
#   lh_vec1 <- colnames(test[,c(b[i,1],b[i,2],b[i,3],b[i,4],b[i,5])]) 
#   lh_vec2 <- paste(lh_vec1, "= 0", sep = " ")
#   #later need to test the linearity in the test,, the linearHypothesis used to test where iv joint test is significat, the F score should be higher than 10
#   lh_m_iv_endo1 <- linearHypothesis(fs_m_iv_endo1, c(lh_vec2[1], lh_vec2[2], lh_vec2[3],lh_vec2[4],lh_vec2[5]), test="F", type=HC1, singular.ok = T)
#   lh_m_iv_endo2 <- linearHypothesis(fs_m_iv_endo2, c(lh_vec2[1], lh_vec2[2], lh_vec2[3],lh_vec2[4],lh_vec2[5]), test="F", type=HC1, singular.ok = T)
#   
#   
#   if (m_iv$coefficients[4]<0 & m_iv$coefficients[5]>0 &
#       lh_m_iv_endo1$F[2] > 10 & lh_m_iv_endo2$F[2] > 10 ){
#     coeff <- summ_m_iv$coefficients
#     diag <- summ_m_iv$diagnostics
#     # Here I filter the result accoring to iv test and coefficient significant test
#     
#     right <- rbind(right,c(testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],testnames[b[i,5]],coeff[4,1],coeff[5,1],coeff[4,4],coeff[5,4],diag[1,4], 
#                            diag[2,4],diag[3,4],diag[4,4],lh_m_iv_endo1$F[2], lh_m_iv_endo2$F[2], score)) 
#     cat(c(testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],testnames[b[i,5]],coeff[4,1],coeff[5,1],coeff[4,4],coeff[5,4],diag[1,4], 
#           diag[2,4],diag[3,4],diag[4,4],lh_m_iv_endo1$F[2], lh_m_iv_endo2$F[2], score))
#     #stargazer(m_iv,omit =name,type = 'text')
#     
#   }
# }
# Sum_dataframe_5iv <- as.data.frame(right)
# colnames(Sum_dataframe_5iv) <- c('iv1','iv2','iv3','iv4','iv5','price_coef','quality_coef','price_coef_Pvalue','quality_coef_Pvalue',
#                                  'WeakIV_1','WeakIV_2','WuHausman','Sargan','iv_endo1_Ftest','iv_endo1_Ftes2','score')
# 
# Sum_dataframe_5iv <- Sum_dataframe_5iv[-1,]
# 
# saveRDS(Sum_dataframe_5iv,'Sum_dataframe_5iv.Rds')
# # Sum_dataframe_4iv <- Sum_dataframe_4iv[-1,]
# # saveRDS(Sum_dataframe_4iv,'Sum_dataframe_4iv.Rds')
# write.csv(Sum_dataframe_4iv,'Sum_dataframe_4iv.csv')
# 

# merger simulation -------------------------------------------------------
i <- 10 #2,3,5,7,8,9,14,18
formula <- as.formula(sprintf('lgs.s0 ~.-%s -%s -%s | .-%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],'Prices','Averate'))
m_iv_putR <- ivreg(formula, data = test[,c(1:9,b[i,1],b[i,2],b[i,3],24:122)])
summary(m_iv_putR,diagnostics = TRUE)

olsR <- lm('lgs.s0 ~.',data = test[,c(1:9,24:122)])

# #First Stage regression
formula_lh_endo_1 <- as.formula(sprintf('Prices ~.+%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],'lgs.s0','Averate'))
formula_lh_endo_2 <- as.formula(sprintf('Averate ~.+%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],'Prices','lgs.s0'))
fs_m_iv_endo1R <- lm(formula_lh_endo_1, data = test[,c(1:9,b[i,1],b[i,2],b[i,3],24:122)])
fs_m_iv_endo2R <- lm(formula_lh_endo_2, data = test[,c(1:9,b[i,1],b[i,2],b[i,3],24:122)])
# alpha <- -0.07065  # get from the two step gmm result. Coefficient of price
# tau <- 6.31321# Coefficient of quality g
alpha <- m_iv_putR$coefficients[3]   # get from the two step gmm result. Coefficient of price
tau <- m_iv_putR$coefficients[2]# Coefficient of quality g
# pickup one market data, then do the merge simulation
value <- m_iv_putR$fitted.values- alpha * datatest4$Prices - tau * datatest4$Averate
datatest4$meanval <- value 
# alpha <- 6.106  # From the above 2sls coefficent of price
# tau <- 2.528# Coefficient of quality 
owner <- function(thisOwner){
  owners <- as.numeric(factor(thisOwner, levels= unique(thisOwner)))
  nprod <- length(thisOwner)
  thisOwner <- matrix(0,ncol=nprod,nrow=nprod)
  for( o in unique(owners)){
    thisOwner [owners == o, owners == o] = 1
  }
  return(thisOwner)
}

elast_price <- function(shares,prices,alpha){
  nprods <-  length(shares)
  shares <- as.matrix(shares,nrow = nprods, ncol = 1)
  elast <- -matrix(alpha*shares,ncol=nprods,nrow=nprods,byrow=TRUE)
  diag(elast) <- alpha-1 + diag(elast)
  return(elast)
}

elast_quality <- function(shares,quality,tau){
  nprods <-  length(shares)
  shares <- as.matrix(shares,nrow = nprods, ncol = 1)
  elast <- -tau  * matrix(quality*shares,ncol=nprods,nrow=nprods,byrow=TRUE)
  diag(elast) <- tau*quality + diag(elast)
  return(elast)
}

calcShares <- function(prices,quality,meanval,alpha,tau){
  shares <- exp(meanval + alpha*log(prices)+tau*quality)
  shares <- shares/(1 + sum(shares,na.rm=TRUE))
  return(shares)
}

calcMc_price <- function(prices,quality,meanval,alpha,tau,owner,B){
  share_i <- calcShares(prices,quality,meanval,alpha,tau)
  price_over_share_i <- matrix(1 / (share_i*B/prices)) %*% matrix(prices, nrow = 1)
  elasticities_i <- elast_price(share_i,prices,alpha)
  derivatives_i <- elasticities_i / price_over_share_i # partial derivatives of shares wrt price
  own_prod_pre_i <- owner # if element (i,j) equals 1, that means that prod i and j are produced by same firm
  markups <- c(solve(t(derivatives_i) * own_prod_pre_i) %*% (share_i*B/prices))
  arg_cost <- prices + markups
  isNegMC <- arg_cost < 0
  if(any(isNegMC, na.rm = TRUE)){
    # warning(paste("Negative marginal costs were calibrated for the following brands:", paste(prodNames[isNegMC], collapse=",")))
    arg_cost[isNegMC] <- min(arg_cost[!isNegMC])
  }
  return(arg_cost)
}

calcMC_quality <- function(prices,quality,meanval,alpha,tau,owner,B){
  mc_price <- calcMc_price(prices,quality,meanval,alpha,tau,owner,B)
  share_i <- calcShares(prices,quality,meanval,alpha,tau)
  quality_pre_i <- quality
  quality_over_share_i <- matrix(1 / (share_i*B/prices)) %*% matrix(quality_pre_i, nrow = 1)
  elasticities_i <- elast_quality(share_i,quality_pre_i,tau)
  derivatives_i <- elasticities_i / quality_over_share_i # partial derivatives of shares wrt quality
  own_prod_pre_i <- owner # if element (i,j) equals 1, that means that prod i and j are produced by same firm
  mc_quality <- (t(derivatives_i) * own_prod_pre_i) %*% (prices-mc_price)
  isNegMC <- mc_quality < 0
  mc_quality[isNegMC] <- min(mc_quality[!isNegMC])
  # mc_quality <- log(mc_quality)
  if(any(isNegMC, na.rm = TRUE)){
    mc_quality[isNegMC] <- min(mc_quality[!isNegMC])
    # warning(paste("Negative marginal costs were calibrated for the following brands:", paste(prodNames[isNegMC], collapse=",")))
    
  }
  return(mc_quality)
}


calcShares.p <- function(prices,quality,meanval,alpha,tau){
  shares <- exp(meanval + alpha*log(prices)+tau*quality)
  shares <- shares/(1 + sum(shares,na.rm=TRUE))
  return(shares)
}


calcMc_price.p <- function(prices,meanval,quality,alpha,tau,owner,B){
  share_i <- calcShares.p(prices,quality,meanval,alpha,tau)
  price_over_share_i <- matrix(1 / (share_i*B/prices)) %*% matrix(prices, nrow = 1)
  elasticities_i <- elast_price(share_i,prices,alpha)
  derivatives_i <- elasticities_i / price_over_share_i # partial derivatives of shares wrt price
  own_prod_pre_i <- owner # if element (i,j) equals 1, that means that prod i and j are produced by same firm
  markups <- c(solve(t(derivatives_i) * own_prod_pre_i) %*% (share_i*B/prices))
  arg_cost <- prices + markups
  isNegMC <- arg_cost < 0
  arg_cost[isNegMC] <- min(arg_cost[!isNegMC])
  # arg_cost <- log(arg_cost)
  # if(any(isNegMC, na.rm = TRUE)){
  #   
  #   warning(paste("Negative marginal costs were calibrated for the following brands:", paste(prodNames[isNegMC], collapse=",")))
  # }
  return(arg_cost)
}

datatest5 <- datatest4
meanval <- datatest5$meanval
quality_pre <- datatest5$Averate # The average of ratings are expressed as the product quality
price_pre <- exp(datatest5$Prices)
brand_pre <- datatest5$Firm
ownerpre <- owner(brand_pre)
datatest5$Firm[which(datatest5$Firm=='eufy')] = 'iRobot'
brand_post <- datatest5$Firm
ownerpost <- owner(brand_post)
B <- datatest4$Fra_income
mc.qua <- calcMC_quality(price_pre,quality_pre,meanval,alpha,tau,ownerpre,B)

datatest5$mc_quality_pre <- mc.qua
linear.2 <- feols(mc_quality_pre ~ Averate | Brand+market_ids, datatest5)
coef_quality <- linear.2$coeftable[1,1]
etable(linear.2)

# tau <- 2.509# Coefficient of quality 
# pickup one market data, then do the merge simulation
wb1 <- createWorkbook(creator = 'junjunz',title = 'ces_d')
# wb2 <- createWorkbook(creator = 'junjunz',title = 'ces_s')
market_list <- c('R20Q1','R20Q2','R20Q3','R20Q4','R21Q1','R21Q2','R21Q3','R21Q4','R22Q1')
# datatest4$value <- inn_delta$Obprice_dif_pg
for (i in market_list) {
  market_ids <- i
  data.R22Q1 <- datatest4[datatest4$market_ids==market_ids,]
  inn_delta <- read.xlsx('ces_d.xlsx',sheet = market_ids)
  data.R22Q1$Mdelta <- abs(inn_delta$Obprice_dif_pg)
  data.R22Q1$Prices <- exp(data.R22Q1$Prices)
  B <- data.R22Q1$Fra_income
  Unobpart <- datatest5$mc_quality_pre[datatest4$market_ids==market_ids]-linear.2$fitted.values[datatest4$market_ids==market_ids] # get the unobserved part that will affect the marginal cost of quality
  data.R22Q1$Unobpart <- Unobpart[datatest4$market_ids==market_ids]
  meanval <- data.R22Q1$meanval
  quality_pre <- data.R22Q1$Averate # The average of ratings are expressed as the product quality
  price_pre <-data.R22Q1$Prices
  brand_pre <- data.R22Q1$Firm
  ownerpre <- owner(brand_pre)
  data.R22Q1$Firm[which(data.R22Q1$Firm=='eufy')] = 'iRobot'
  brand_post <- data.R22Q1$Firm
  ownerpost <- owner(brand_post)
  # elasticities_pre_mkt <- elast_price()
  FOC <- function(innitial_vetor){
    len <- length(innitial_vetor)
    delta <- innitial_vetor[1:(len/2)]
    prices <- price_pre
    quality <- innitial_vetor[(len/2+1):len]
    implied_shares <- calcShares(prices,quality,meanval,alpha,tau)
    price_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(prices, nrow = 1)
    elasticities_pre_price <- elast_price(implied_shares,prices,alpha)
    derivatives_price <- elasticities_pre_price / price_over_shares
    
    # markups_post <- c(-solve(t(derivatives_price) * ownerpre) %*% implied_shares)
    marg_cost <- calcMc_price(price_pre,quality_pre,meanval,alpha,tau,ownerpre,B)
    
    quality_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(quality, nrow = 1)
    elasticities_pre_quality <- elast_quality(implied_shares,quality,tau)
    derivatives_quality <- elasticities_pre_quality / quality_over_shares
    
    # markups_post <- c(-solve(t(derivatives_quality) * ownerpre) %*% implied_shares)
    vec1 <- as.matrix(coef_quality*quality+Unobpart)
    M1_PRICE <- t(derivatives_price) * ownerpre
    M2_QUALITY <- t(derivatives_quality) * ownerpre
    Merge_matrix <- rbind(cbind(M1_PRICE,matrix(0,nrow=nrow(M1_PRICE),ncol=ncol(M2_QUALITY))),cbind(matrix(0,nrow=nrow(M2_QUALITY),ncol=ncol(M1_PRICE)),M2_QUALITY))
    price_mc <- prices - marg_cost
    matrix_price_mc <- append(price_mc,price_mc)
    matrix_price_mc <- matrix(matrix_price_mc,ncol = 1)
    quantity <- implied_shares*B/prices
    matrix_shares_mC <- append(quantity,-vec1)
    matrix_shares_mC <- matrix(matrix_shares_mC,ncol = 1)
    thisFOC <- as.vector(matrix_shares_mC + Merge_matrix %*% matrix_price_mc)
    return(sum(thisFOC))
  }
  
  
  FOC_optim <- function(innitial_vetor){
    len <- length(innitial_vetor)
    delta <- innitial_vetor[1:(len/2)]
    prices <- price_pre
    quality <- innitial_vetor[(len/2+1):len]
    implied_shares <- calcShares(prices,quality,meanval,alpha,tau)
    price_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(prices, nrow = 1)
    elasticities_pre_price <- elast_price(implied_shares,prices,alpha)
    derivatives_price <- elasticities_pre_price / price_over_shares
    
    # markups_post <- c(-solve(t(derivatives_price) * ownerpre) %*% implied_shares)
    marg_cost <- calcMc_price(price_pre,quality_pre,meanval,alpha,tau,ownerpre,B)
    
    quality_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(quality, nrow = 1)
    elasticities_pre_quality <- elast_quality(implied_shares,quality,tau)
    derivatives_quality <- elasticities_pre_quality / quality_over_shares
    
    # markups_post <- c(-solve(t(derivatives_quality) * ownerpre) %*% implied_shares)
    vec1 <- as.matrix(coef_quality*quality+Unobpart)
    M1_PRICE <- t(derivatives_price) * ownerpre
    M2_QUALITY <- t(derivatives_quality) * ownerpre
    Merge_matrix <- rbind(cbind(M1_PRICE,matrix(0,nrow=nrow(M1_PRICE),ncol=ncol(M2_QUALITY))),cbind(matrix(0,nrow=nrow(M2_QUALITY),ncol=ncol(M1_PRICE)),M2_QUALITY))
    price_mc <- prices - marg_cost
    matrix_price_mc <- append(price_mc,price_mc)
    matrix_price_mc <- matrix(matrix_price_mc,ncol = 1)
    quantity <- implied_shares*B/prices
    matrix_shares_mC <- append(quantity,-vec1)
    matrix_shares_mC <- matrix(matrix_shares_mC,ncol = 1)
    thisFOC <- as.vector(matrix_shares_mC + Merge_matrix %*% matrix_price_mc)
    return(sum(thisFOC))
  }
  library(BB)
  innitial_vector <- append(data.R22Q1$Mdelta,data.R22Q1$Averate)
  len <- length(innitial_vector)
  minResult.pre <- BBsolve(innitial_vector,FOC)
  
  if (minResult.pre$convergence != 0){
    lo <- c(rep(-10000,len/2),rep(0,len/2))  
    hi <- c(rep(10000,len/2),rep(5,len/2))
    minResult.pre <- BBoptim(innitial_vector,FOC_optim,lower=lo, upper=hi)
  }
  
  
  
  
  FOC_post <- function(innitial_vetor){
    
    len <- length(innitial_vetor)
    delta <- innitial_vetor[1:(len/2)]
    prices <- price_pre
    quality <- innitial_vetor[(len/2+1):len]
    implied_shares <- calcShares(prices,quality,meanval,alpha,tau)
    price_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(prices, nrow = 1)
    elasticities_pre_price <- elast_price(implied_shares,prices,alpha)
    derivatives_price <- elasticities_pre_price / price_over_shares
    
    # markups_post <- c(-solve(t(derivatives_price) * ownerpre) %*% implied_shares)
    marg_cost <- calcMc_price(price_pre,quality_pre,meanval,alpha,tau,ownerpre,B)
    
    quality_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(quality, nrow = 1)
    elasticities_pre_quality <- elast_quality(implied_shares,quality,tau)
    derivatives_quality <- elasticities_pre_quality / quality_over_shares
    
    # markups_post <- c(-solve(t(derivatives_quality) * ownerpre) %*% implied_shares)
    vec1 <- as.matrix(coef_quality*quality+Unobpart)
    M1_PRICE <- t(derivatives_price) * ownerpost
    M2_QUALITY <- t(derivatives_quality) * ownerpost
    Merge_matrix <- rbind(cbind(M1_PRICE,matrix(0,nrow=nrow(M1_PRICE),ncol=ncol(M2_QUALITY))),cbind(matrix(0,nrow=nrow(M2_QUALITY),ncol=ncol(M1_PRICE)),M2_QUALITY))
    price_mc <- prices - marg_cost + delta
    matrix_price_mc <- append(price_mc,price_mc)
    matrix_price_mc <- matrix(matrix_price_mc,ncol = 1)
    quantity <- implied_shares*B/prices
    matrix_shares_mC <- append(quantity,-vec1)
    matrix_shares_mC <- matrix(matrix_shares_mC,ncol = 1)
    thisFOC <- as.vector(matrix_shares_mC + Merge_matrix %*% matrix_price_mc)
    return(sum(thisFOC))
  }
  # len <- length(innitial_vector)
  
  FOC_post_optim <- function(innitial_vetor){
    
    len <- length(innitial_vetor)
    delta <- innitial_vetor[1:(len/2)]
    prices <- price_pre
    quality <- innitial_vetor[(len/2+1):len]
    implied_shares <- calcShares(prices,quality,meanval,alpha,tau)
    price_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(prices, nrow = 1)
    elasticities_pre_price <- elast_price(implied_shares,prices,alpha)
    derivatives_price <- elasticities_pre_price / price_over_shares
    
    # markups_post <- c(-solve(t(derivatives_price) * ownerpre) %*% implied_shares)
    marg_cost <- calcMc_price(price_pre,quality_pre,meanval,alpha,tau,ownerpre,B)
    
    quality_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(quality, nrow = 1)
    elasticities_pre_quality <- elast_quality(implied_shares,quality,tau)
    derivatives_quality <- elasticities_pre_quality / quality_over_shares
    
    # markups_post <- c(-solve(t(derivatives_quality) * ownerpre) %*% implied_shares)
    vec1 <- as.matrix(coef_quality*quality+Unobpart)
    M1_PRICE <- t(derivatives_price) * ownerpost
    M2_QUALITY <- t(derivatives_quality) * ownerpost
    Merge_matrix <- rbind(cbind(M1_PRICE,matrix(0,nrow=nrow(M1_PRICE),ncol=ncol(M2_QUALITY))),cbind(matrix(0,nrow=nrow(M2_QUALITY),ncol=ncol(M1_PRICE)),M2_QUALITY))
    price_mc <- prices - marg_cost + delta
    matrix_price_mc <- append(price_mc,price_mc)
    matrix_price_mc <- matrix(matrix_price_mc,ncol = 1)
    quantity <- implied_shares*B/prices
    matrix_shares_mC <- append(quantity,-vec1)
    matrix_shares_mC <- matrix(matrix_shares_mC,ncol = 1)
    thisFOC <- as.vector(matrix_shares_mC + Merge_matrix %*% matrix_price_mc)
    return(sum(thisFOC))
  }
  # innitial_vector <- append(rep(1,length(data.R22Q1$ave_rating)),data.R22Q1$ave_rating)
  # len <- length(innitial_vector)
  
  minResult.post <- BBsolve(innitial_vector,FOC_post)
  
  if (minResult.post$convergence != 0){
    lo <- c(rep(-10000,len/2),rep(0,len/2))  
    hi <- c(rep(10000,len/2),rep(5,len/2))
    minResult.post <- BBoptim(innitial_vector,FOC_post_optim,lower=lo, upper=hi)
  }
  # inni_delta <- price_pre-minResult.pre$par[1:(len/2)]
  
  # minResult.post <- dfsane(innitial_vector,FOC_post,quiet=FALSE,control=list(maxit = 5000))

  
  
  EstdeltaPre.pg <- minResult.pre$par[1:(len/2)]
  EstqualityPre.pg <- minResult.pre$par[(len/2+1):len]
  # EstqualityPre.pg[which(EstqualityPre.pg<0,arr.ind = T)] <- 0.1
  # EstqualityPre.pg[which(EstqualityPre.pg>5,arr.ind = T)] <- 5
  EstsharesPre.pg <- calcShares(price_pre,EstqualityPre.pg,meanval,alpha,tau)
  EstdeltaPost.pg <- minResult.post$par[1:(len/2)]
  EstqualityPost.pg <- minResult.post$par[(len/2+1):len]
  # EstqualityPost.pg[which(EstqualityPost.pg<0,arr.ind = T)] <- 0.1
  # EstqualityPost.pg[which(EstqualityPost.pg>5,arr.ind = T)] <- 5
  EstsharesPost.pg <- calcShares(price_pre,EstqualityPost.pg,meanval,alpha,tau)
  
  #pROPOSITION1
  #The l*m
  
  prices <- price_pre
  quality <- minResult.post$par[(len/2+1):len]
  implied_shares <- calcShares(prices,quality,meanval,alpha,tau)
  price_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(prices, nrow = 1)
  elasticities_pre_price <- elast_price(implied_shares,prices,alpha)
  derivatives_price <- elasticities_pre_price / price_over_shares
  
  quality_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(quality, nrow = 1)
  elasticities_pre_quality <- elast_quality(implied_shares,quality,tau)
  derivatives_quality <- elasticities_pre_quality / quality_over_shares
  
  p_diversion_ration <- -derivatives_price/diag(derivatives_price)
  p_diversion_ration.same <- p_diversion_ration * ownerpost
  price_ratio <- apply(p_diversion_ration.same,1,sum)
  
  q_diversion_ration <- -derivatives_quality/diag(derivatives_quality)
  q_diversion_ration.same <- q_diversion_ration * ownerpost 
  quality_ratio <- apply(q_diversion_ration.same,1,sum)
  
  quality_price_lm <- -quality_ratio/price_ratio*implied_shares
  
  #the h*m
  prices <- price_pre
  quality <- minResult.pre$par[(len/2+1):len]
  implied_shares <- calcShares(prices,quality,meanval,alpha,tau)
  price_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(prices, nrow = 1)
  elasticities_pre_price <- elast_price(implied_shares,prices,alpha)
  derivatives_price <- elasticities_pre_price / price_over_shares
  
  quality_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(quality, nrow = 1)
  elasticities_pre_quality <- elast_quality(implied_shares,quality,tau)
  derivatives_quality <- elasticities_pre_quality / quality_over_shares
  
  quality_price_hm <- -diag(derivatives_quality)/diag(derivatives_price)*implied_shares
  
  lm_hm <- quality_price_lm - quality_price_hm
  
  g.m_g <- (EstqualityPost.pg- EstqualityPre.pg)/EstqualityPre.pg
  
  
  #Then I need to prove the difference of the diversion ratio in the core paper
  # delta <- innitial_vetor[1:(len/2)]
  prices <- price_pre
  quality <- minResult.post$par[(len/2+1):len]
  implied_shares <- calcShares(prices,quality,meanval,alpha,tau)
  price_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(prices, nrow = 1)
  elasticities_pre_price <- elast_price(implied_shares,prices,alpha)
  derivatives_price <- elasticities_pre_price / price_over_shares
  
  quality_over_shares <- matrix(1 / (implied_shares*B/prices)) %*% matrix(quality, nrow = 1)
  elasticities_pre_quality <- elast_quality(implied_shares,quality,tau)
  derivatives_quality <- elasticities_pre_quality / quality_over_shares
  
  p_diversion_ration <- -derivatives_price/diag(derivatives_price)
  diag(p_diversion_ration) <- 0
  p_diversion_ration.same <- p_diversion_ration * ownerpost
  p_diversion_ration.same <- apply(p_diversion_ration.same,1,sum)
  
  q_diversion_ration <- -derivatives_quality/diag(derivatives_quality)
  diag(q_diversion_ration) <- 0
  q_diversion_ration.same <- q_diversion_ration * ownerpost 
  q_diversion_ration.same <- apply(q_diversion_ration.same,1,sum)
  
  
  price_quality <- p_diversion_ration.same-q_diversion_ration.same
  corollary1 <- price_quality
  
  Est_result.pg <- data.frame(brand_pre,brand_post,EstqualityPre.pg,EstsharesPre.pg,EstsharesPost.pg,EstdeltaPre.pg,EstdeltaPost.pg,EstqualityPost.pg,p_diversion_ration.same,
                              q_diversion_ration.same,corollary1,
                              g.m_g,lm_hm)
  Est_result.pg$label <- ''
  Est_result.pg$label[Est_result.pg$brand_post=='iRobot'] <- "*"
  # Est_result.pg$priceDif.pg <- Est_result.pg$EstpricePost-Est_result.pg$EstpricePre
  Est_result.pg$qualityDif.pg <- (Est_result.pg$EstqualityPost.pg-Est_result.pg$EstqualityPre.pg)/Est_result.pg$EstqualityPre.pg
  Est_result.pg$EstsharDif.pg <- (Est_result.pg$EstsharesPost.pg-Est_result.pg$EstsharesPre.pg)/Est_result.pg$EstsharesPre.pg
  Est_result.pg$deltaDif.pg <- (Est_result.pg$EstdeltaPost.pg - Est_result.pg$EstdeltaPre.pg)/Est_result.pg$EstdeltaPre.pg
  Est_result.pg <- Est_result.pg %>%
    relocate(label,.after = brand_post)
  # stargazer(Est_result.pg,type = 'text')
  
  Pro1 <- Est_result.pg
  Pro1$market_ids <- market_ids
  Pro1$Ob.price <- price_pre
  Pro1$Ob.quality <- quality_pre
  # Pro1$Obprice_dif_pg <- price_pre-Pro1$EstpricePre.pg
  Pro1$Obquality_dif_pg <- quality_pre-Pro1$EstqualityPre.pg
  Pro1$Ob.shares <- data.R22Q1$shares
  Pro1$EST_ob_sharepg <- Pro1$EstsharesPre.pg- data.R22Q1$shares
  Pro1$converge_pre <- minResult.pre$convergence
  Pro1$converge_post <-  minResult.post$convergence
  
  Pro1 <- Pro1[order(Pro1$label,decreasing = T),]
  stargazer(Pro1[Pro1$label=='*',],type = 'text')
  addWorksheet(wb1,sheetName = market_ids)
  writeData(wb1,sheet = market_ids,x = Pro1)
}
saveWorkbook(wb1, "ces_p_neutral.xlsx", overwrite = TRUE)
# saveWorkbook(wb2, "ces_s_delta.xlsx", overwrite = TRUE)
# tt <- read.xlsx('./RESULT.xlsx',sheet = 'CES',detectDates = TRUE,fillMergedCells = TRUE)
# library(xtable)
# xtable(tt, display=rep('g', 10))
