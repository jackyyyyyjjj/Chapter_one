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
#' 
## -----------------------------------------------------------------------------------------
# data1 <- readRDS('../data/price_sales_rating.Rds')
# amazon <- readRDS('../data/FU2.Rds')
# char <- amazon[,c(1,9:11,21:25)]
# Com.data <- merge(data1,char,by='asin',all.x = T)
# usahousehold_number <- read.xlsx('../data/usahousehold_number.xlsx',sheet = 'usa')
# #check how many products in each market, delete the market before 2018
# # data1 <- data1[!(data1$market_ids=='R16Q3'|data1$market_ids=='R16Q4'|data1$market_ids=='R17Q1'|
# #                  data1$market_ids=='R17Q2'|data1$market_ids=='R17Q3'|data1$market_ids=='R17Q4'),]
# data2 <- merge(Com.data,usahousehold_number,by='market_ids',all.x = T)
# # using keepa data with ---------------------------
# #if i delete the missing value
# data3 <- data2[!is.infinite(data2$sales),]
# data3 <- data3[!is.infinite(data3$prices),]
# data4 <- data3[!is.na(data3$ave_rating),]
# 
# write.csv(data4,'data4.csv')




# FULL DATA IN DATA4.CSV --------------------------------------------------
# data1 <- read.csv('data4.csv')
# 
# conn <- dbConnect(RSQLite::SQLite(), "C:/Users/hpe21bnu/OneDrive - University of East Anglia/Documents/UEA_PHD/First_charpter/code/price_sales/amazon_sales.sqlite")
# amazon_sales <- dbGetQuery(conn, "SELECT asin,market_ids,sales
#                from estsales")
# dbDisconnect(conn)
# 
# # write.csv(amazon_sales,'amazon_sales.csv')
# 
# data1$com <- paste(data1$market_ids,data1$asin,sep = '')
# 
# amazon_sales$com <- paste(amazon_sales$market_ids,amazon_sales$asin,sep = '')
# 
# amazon_sales <- amazon_sales[,-c(1,2)]
# COMBINE1 <- merge(data1,amazon_sales,by='com',all.x = T)
# write.csv(COMBINE1,'COMBINE1.csv')


# START -------------------------------------------------------------------
amazon <- readRDS('../data/FU2.Rds')
raw_data1 <- read.csv('product.dea3_NO_EMSUCTION4.csv')
suction.work <- raw_data1[,c(3,9)]
data1 <- read.csv('COMBINE1.csv')
brand.firm <- amazon[,c(1,4,5,7,27)]
data2 <- merge(data1,brand.firm,by='asin',all.x = T)
data2 <- merge(data2,suction.work,by='asin',all.x = T)
data2$year <- str_extract(data2$market_ids,'\\d{4}')
data2$year <- as.numeric(data2$year) 
data2$pro_age <- 2022-as.numeric(data2$year)

data2 <- data2[!(data2$market_ids=='2016-07'|data2$market_ids=='2016-08'|data2$market_ids=='2016-10'|
                   data2$market_ids=='2016-11'|data2$market_ids=='2016-12'|data2$market_ids=='2017-01'|
                   data2$market_ids=='2017-02'|data2$market_ids=='2017-03'|data2$market_ids=='2017-04'),]

# PROCESS <- data2 %>%
#   group_by(market_ids,Firm,Brand) %>%
#   summarise(Averate = mean(ave_rating),
#             Prices = mean(prices),
#             itemLength = mean(itemLength),
#             itemWidth = mean(itemWidth),
#             itemHeight = mean(itemHeight),
#             itemWeight = mean(itemWeight),
#             sales = sum(sales),
#             Wok_1charge = mean(Wok_1charge),
#             suction = mean(suction),
#             pro_age = mean(pro_age),
#             num.pro = ifelse(n()>1,1,0),
#             Fra_income = mean(Fra_income)
#   ) %>%
#   ungroup()
# JUST DELETE THE ZERO SHARES WHICH i NEED TO IMPROVE IN THE FUTURE
# data2 <- data2[!data2$rating==0,]
# data2 <- as.data.frame(PROCESS)
# data2 <- data2[,-c(6)]
# 
stargazer(data2,summary = TRUE,digits=2)


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





data2$firm_ids <- .indexing.markets(data2$Firm)
data2$product_ids <- .indexing.markets(data2$Brand)

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
blp_iv <- constructIV(data2[,23],data2[,3],data2[,24],data2[,c(11:13,18,20,22)])
##work1charge,suction,PRO AGE,RATINGS IV
# blp_iv <- constructIV(data2[,16],data2[,1],data2[,17],data2[,c(3:4,9,15)])
data3 <- cbind(data2,blp_iv)


datatest <- data3


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



# ols
# m_ols <- lm(shares ~ .,data =datatest4[,-c(1:2,5,8,9,11,12)])
##Only keep sum.o.suction, sum.o.Wok_1charge and sum.o.pro_age
# test <- datatest4[,-c(1:6,9,14:17,19,21,23,24,39:41)]
test <- datatest4[,-c(1:6,9:10,14:17,19,21,23:24,37:39)]
# I will consider the rating as the IV ------------------------------------
# 
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

k <- feols(lgs.s0 ~ itemWidth + itemHeight + itemWeight+ Wok_1charge +suction |product_ids + market_ids|Averate + Prices ~sum.o.Wok_1charge+sum.o.suction+sum.r.suction,datatest4)
etable(k)
#here find pro_age is a problem
# three iv ----------------------------------------------------------------

x <- 9:20
# combn(x,3)
b <- t(combn(x,3))
testnames <- colnames(test)
right <- matrix(1:14,nrow = 1,ncol = 14)
wt <- c((.8/3),(.8/3),(.8/3), 0.2)
colnames(test)[1] <- 'Prices'
colnames(test)[2] <- 'Averate'
#1best 3,2best 2.2,

for (i in seq(1,length(b[,1]),1)) {
  #IV regression
  formula <- as.formula(sprintf('lgs.s0 ~.-%s -%s -%s | .-%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],'Prices','Averate'))
  m_iv <- ivreg(formula, data = test[,c(1:8,b[i,1],b[i,2],b[i,3],21:174)])
  summ_m_iv <- summary(m_iv,diagnostics = TRUE)
  #scores
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
  
  
  #First Stage regression
  formula_lh_endo_1 <- as.formula(sprintf('Prices ~.+%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],'lgs.s0','Averate'))
  formula_lh_endo_2 <- as.formula(sprintf('Averate ~.+%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],'Prices','lgs.s0'))
  fs_m_iv_endo1 <- lm(formula_lh_endo_1, data = test[,c(1:8,b[i,1],b[i,2],b[i,3],21:174)])
  fs_m_iv_endo2 <- lm(formula_lh_endo_2, data = test[,c(1:8,b[i,1],b[i,2],b[i,3],21:174)])
  
  lh_vec1 <- colnames(test[,c(b[i,1],b[i,2],b[i,3])])
  lh_vec2 <- paste(lh_vec1, "= 0", sep = " ")
  #later need to test the linearity in the test,, the linearHypothesis used to test where iv joint test is significat, the F score should be higher than 10
  lh_m_iv_endo1 <- linearHypothesis(fs_m_iv_endo1, c(lh_vec2[1], lh_vec2[2], lh_vec2[3]), test="F", type=HC1, singular.ok = T)
  lh_m_iv_endo2 <- linearHypothesis(fs_m_iv_endo2, c(lh_vec2[1], lh_vec2[2], lh_vec2[3]), test="F", type=HC1, singular.ok = T)
  
  
  # if (m_iv$coefficients[4]<0 & m_iv$coefficients[5]>0 &
  #     lh_m_iv_endo1$F[2] > 10 & lh_m_iv_endo2$F[2] > 10 ){
  if (m_iv$coefficients[4]<0 & m_iv$coefficients[5]>0){
    coeff <- summ_m_iv$coefficients
    diag <- summ_m_iv$diagnostics
    # Here I filter the result accoring to iv test and coefficient significant test
    
    right <- rbind(right,c(testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],coeff[4,1],coeff[5,1],coeff[4,4],coeff[5,4],diag[1,4],
                           diag[2,4],diag[3,4],diag[4,4],lh_m_iv_endo1$F[2], lh_m_iv_endo2$F[2], score))
    cat(c(testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],coeff[4,1],coeff[5,1],coeff[4,4],coeff[5,4],diag[1,4],
          diag[2,4],diag[3,4],diag[4,4],lh_m_iv_endo1$F[2], lh_m_iv_endo2$F[2], score))
    #stargazer(m_iv,omit =name,type = 'text')
    
  }
}
Sum_dataframe <- as.data.frame(right)
colnames(Sum_dataframe) <- c('iv1','iv2','iv3','price_coef','quality_coef','price_coef_Pvalue','quality_coef_Pvalue',
                             'WeakIV_1','WeakIV_2','WuHausman','Sargan','iv_endo1_Ftest','iv_endo1_Ftes2','score')
Sum_dataframe <- Sum_dataframe[-1,]

saveRDS(Sum_dataframe,'Sum_dataframe.Rds')

kk <- readRDS('Sum_dataframe.Rds')
write.csv(Sum_dataframe,'Sum_dataframe.csv')

# 4 ivs -------------------------------------------------------------------


x <- 9:20
b <- t(combn(x,4))
testnames <- colnames(test)
right <- matrix(1:15,nrow = 1,ncol = 15)
wt <- c((.8/3),(.8/3),(.8/3), 0.2)

#1best 3,2best 2.2,

for (i in seq(1,length(b[,1]),1)) {
  #IV regression
  formula <- as.formula(sprintf('lgs.s0 ~.-%s -%s -%s -%s| .-%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],'Prices','Averate'))
  m_iv <- ivreg(formula, data = test[,c(1:8,b[i,1],b[i,2],b[i,3],b[i,4],21:174)])
  summ_m_iv <- summary(m_iv,diagnostics = TRUE)
  #scores
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
  
  
  #First Stage regression
  formula_lh_endo_1 <- as.formula(sprintf('Prices ~.+%s +%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],'lgs.s0','Averate'))
  formula_lh_endo_2 <- as.formula(sprintf('Averate ~.+%s +%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],'Prices','lgs.s0'))
  fs_m_iv_endo1 <- lm(formula_lh_endo_1, data = test[,c(1:8,b[i,1],b[i,2],b[i,3],b[i,4],21:174)])
  fs_m_iv_endo2 <- lm(formula_lh_endo_2, data = test[,c(1:8,b[i,1],b[i,2],b[i,3],b[i,4],21:174)])
  
  lh_vec1 <- colnames(test[,c(b[i,1],b[i,2],b[i,3],b[i,4])]) 
  lh_vec2 <- paste(lh_vec1, "= 0", sep = " ")
  #later need to test the linearity in the test,, the linearHypothesis used to test where iv joint test is significat, the F score should be higher than 10
  lh_m_iv_endo1 <- linearHypothesis(fs_m_iv_endo1, c(lh_vec2[1], lh_vec2[2], lh_vec2[3],lh_vec2[4]), test="F", type=HC1, singular.ok = T)
  lh_m_iv_endo2 <- linearHypothesis(fs_m_iv_endo2, c(lh_vec2[1], lh_vec2[2], lh_vec2[3],lh_vec2[4]), test="F", type=HC1, singular.ok = T)
  
  
  if (m_iv$coefficients[4]<0 & m_iv$coefficients[5]>0 ){
    coeff <- summ_m_iv$coefficients
    diag <- summ_m_iv$diagnostics
    # Here I filter the result accoring to iv test and coefficient significant test
    
    right <- rbind(right,c(testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],coeff[4,1],coeff[5,1],coeff[4,4],coeff[5,4],diag[1,4], 
                           diag[2,4],diag[3,4],diag[4,4],lh_m_iv_endo1$F[2], lh_m_iv_endo2$F[2], score)) 
    cat(c(testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],coeff[4,1],coeff[5,1],coeff[4,4],coeff[5,4],diag[1,4], 
          diag[2,4],diag[3,4],diag[4,4],lh_m_iv_endo1$F[2], lh_m_iv_endo2$F[2], score))
    #stargazer(m_iv,omit =name,type = 'text')
    
  }
}
Sum_dataframe_4iv <- as.data.frame(right)
colnames(Sum_dataframe_4iv) <- c('iv1','iv2','iv3','iv4','price_coef','quality_coef','price_coef_Pvalue','quality_coef_Pvalue',
                                 'WeakIV_1','WeakIV_2','WuHausman','Sargan','iv_endo1_Ftest','iv_endo1_Ftes2','score')
Sum_dataframe_4iv <- Sum_dataframe_4iv[-1,]
write.csv(Sum_dataframe_4iv,'Sum_dataframe_4iv.csv')
saveRDS(Sum_dataframe_4iv,'Sum_dataframe_4iv.Rds')

# 5 ivs -------------------------------------------------------------------


x <- 9:20
b <- t(combn(x,5))
testnames <- colnames(test)
right <- matrix(1:16,nrow = 1,ncol = 16)
wt <- c((.8/3),(.8/3),(.8/3), 0.2)

#1best 3,2best 2.2,

for (i in seq(1,length(b[,1]),1)) {
  #IV regression
  formula <- as.formula(sprintf('lgs.s0 ~.-%s -%s -%s -%s -%s| .-%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],testnames[b[i,5]],'Prices','Averate'))
  m_iv <- ivreg(formula, data = test[,c(1:8,b[i,1],b[i,2],b[i,3],b[i,4],b[i,5],21:184)])
  summ_m_iv <- summary(m_iv,diagnostics = TRUE)
  #scores
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
  
  
  #First Stage regression
  formula_lh_endo_1 <- as.formula(sprintf('Prices ~.+%s +%s +%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],testnames[b[i,5]],'lgs.s0','Averate'))
  formula_lh_endo_2 <- as.formula(sprintf('Averate ~.+%s +%s +%s +%s +%s -%s -%s',testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],testnames[b[i,5]],'Prices','lgs.s0'))
  fs_m_iv_endo1 <- lm(formula_lh_endo_1, data = test[,c(1:8,b[i,1],b[i,2],b[i,3],b[i,4],b[i,5],21:184)])
  fs_m_iv_endo2 <- lm(formula_lh_endo_2, data = test[,c(1:8,b[i,1],b[i,2],b[i,3],b[i,4],b[i,5],21:184)])
  
  lh_vec1 <- colnames(test[,c(b[i,1],b[i,2],b[i,3],b[i,4],b[i,5])]) 
  lh_vec2 <- paste(lh_vec1, "= 0", sep = " ")
  #later need to test the linearity in the test,, the linearHypothesis used to test where iv joint test is significat, the F score should be higher than 10
  lh_m_iv_endo1 <- linearHypothesis(fs_m_iv_endo1, c(lh_vec2[1], lh_vec2[2], lh_vec2[3],lh_vec2[4],lh_vec2[5]), test="F", type=HC1, singular.ok = T)
  lh_m_iv_endo2 <- linearHypothesis(fs_m_iv_endo2, c(lh_vec2[1], lh_vec2[2], lh_vec2[3],lh_vec2[4],lh_vec2[5]), test="F", type=HC1, singular.ok = T)
  
  
  if (m_iv$coefficients[4]<0 & m_iv$coefficients[5]>0 &
      lh_m_iv_endo1$F[2] > 10 & lh_m_iv_endo2$F[2] > 10 ){
    coeff <- summ_m_iv$coefficients
    diag <- summ_m_iv$diagnostics
    # Here I filter the result accoring to iv test and coefficient significant test
    
    right <- rbind(right,c(testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],testnames[b[i,5]],coeff[4,1],coeff[5,1],coeff[4,4],coeff[5,4],diag[1,4], 
                           diag[2,4],diag[3,4],diag[4,4],lh_m_iv_endo1$F[2], lh_m_iv_endo2$F[2], score)) 
    cat(c(testnames[b[i,1]],testnames[b[i,2]],testnames[b[i,3]],testnames[b[i,4]],testnames[b[i,5]],coeff[4,1],coeff[5,1],coeff[4,4],coeff[5,4],diag[1,4], 
          diag[2,4],diag[3,4],diag[4,4],lh_m_iv_endo1$F[2], lh_m_iv_endo2$F[2], score))
    #stargazer(m_iv,omit =name,type = 'text')
    
  }
}
Sum_dataframe_5iv <- as.data.frame(right)
colnames(Sum_dataframe_5iv) <- c('iv1','iv2','iv3','iv4','iv5','price_coef','quality_coef','price_coef_Pvalue','quality_coef_Pvalue',
                                 'WeakIV_1','WeakIV_2','WuHausman','Sargan','iv_endo1_Ftest','iv_endo1_Ftes2','score')

Sum_dataframe_5iv <- Sum_dataframe_5iv[-1,]

saveRDS(Sum_dataframe_5iv,'Sum_dataframe_5iv.Rds')
# Sum_dataframe_4iv <- Sum_dataframe_4iv[-1,]
# saveRDS(Sum_dataframe_4iv,'Sum_dataframe_4iv.Rds')
write.csv(Sum_dataframe_4iv,'Sum_dataframe_4iv.csv')
