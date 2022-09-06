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
if ('DBI' %in% rownames(installed.packages())==FALSE) {
  install.packages('DBI')
}else{
  library(DBI)
}

script_path <- rstudioapi::getActiveDocumentContext()$path
hash = "(.*)/"
setwd(str_extract(script_path, hash)) 
gc(T)


#get the list data for the new data, which can help me check whether the data is available or something else.
conn <- dbConnect(RSQLite::SQLite(), "C:/Users/hpe21bnu/OneDrive - University of East Anglia/Documents/UEA_PHD/First_charpter/Top_firms/KEEPA/keepa.sqlite")
list_date <- dbGetQuery(conn, "SELECT * from KEEPA1")
dbDisconnect(conn)

# VARIATIONS --------------------------------------------------------------
#GET ALL THE VARIATIONS
# None <- list_date[list_date$variations=='None',]
# varia <- list_date[list_date$variations!='None',]
# varia <- varia %>%
#   distinct(variations,.keep_all = T)
# 
# list_01 <- strsplit(varia$variationCSV,',')
# 
# 
# emptyvector <- c()
# for (i in seq(1,length(list_01),1)) {
#   emptyvector <- append(emptyvector,c(list_01[[i]]))
# }
# 
# emptyvector1 <- emptyvector[emptyvector %in% varia$asin]
# emptyvector2 <- emptyvector[!emptyvector %in% varia$asin]
# write.csv(emptyvector2,'variation_asin.csv')
# 
# FULLDATA <- rbind(None,varia)
# 
# ASIN <- FULLDATA %>%
#   distinct(asin,.keep_all = T)
# 
# asin <- ASIN[,1]
# 
# ASIN2 <- old_data %>%
#   distinct(asin,.keep_all = T)
# asin2 <- ASIN2[,1]
# 
# fullasin <- append(asin,asin2)
# 
# fulldata <- data.frame(asin = fullasin)
# fulldata1 <- fulldata %>%
#   distinct(asin,.keep_all = T)
# 
# saveRDS(fulldata1,'asin.Rds')
# kkk <- readRDS('asin.Rds')
# write.csv(kkk,'asin.csv')




old_data1 <- old_data[,-4]#DELETE THE TITLE
#In old data, we do not have data for market 22 q2


new_full_data <- rbind(old_data1,newdata1)

# get the characteristic ----------------
write.csv(list_date[,c(6,8,9,11,17,18,21,23,24,25,26,27,28)],'list_date.csv')

list_data <- merge(list_date,feature_vacuum_robot,by = 'asin',all.x = T)


CHECK <- char_new %>%
  distinct(brand,pa,minutes,.keep_all = T)
## At this moment, we only extract the list year,suction,working minute three characteristics and see what is happening

sel_new <- char_new %>%
  select(asin,pa,minutes)
colnames(sel_new) <- c('asin','suction','Wok_1charge')#change the colname to sel_old
sel_old <- char_old %>%
  select(asin,suction,Wok_1charge)

char_full <- rbind(sel_new,sel_old)

#delete the missing char
char_full1 <- char_full[complete.cases(char_full$suction),]
char_full1 <- char_full1[complete.cases(char_full1$Wok_1charge),]


# Combine the char and the price data -------------------------------------
#delete the R22Q1 DATA
new_full_data1 <- new_full_data[!new_full_data$market_ids == 'R22Q2',]
full_data1 <- merge(char_full1,new_full_data1,by = 'asin',all.x = TRUE)
#ONLY MERGE THE DATA THAT HAS THE SAME ASIN
full_data2 <- merge(char_full1,new_full_data1,by = 'asin')
full_data2$ave_rating[which(full_data2$ave_rating=='NaN',arr.ind =T)] <- NA

#add the dummy value to the missing value

Add_dummy_miss_ave_rating <- function(data) {
  sub <- which(is.na(data$ave_rating))
  inputfile1 <- data[-sub,]
  inputfile1$dummy_averate <- 1
  inputfile2 <- data[sub,]
  inputfile2$dummy_averate <- 0
  result3 <- rbind(inputfile1, inputfile2)
  return(result3)
}

Add_dummy_miss_price <- function(data) {
  sub <- which(is.na(data$prices))
  inputfile1 <- data[-sub,]
  inputfile1$dummy_prices <- 1
  inputfile2 <- data[sub,]
  inputfile2$dummy_prices <- 0
  result3 <- rbind(inputfile1, inputfile2)
  return(result3)
}

full_data2 <-Add_dummy_miss_ave_rating(full_data2) 
full_data2 <-Add_dummy_miss_price(full_data2)
full_data3 <- full_data2 %>%distinct(asin,market_ids, .keep_all =TRUE)



library(openxlsx)
# write.xlsx(full_data3,'full_data3.xlsx')
# Use the linear regression to fullfilee the missing value inside  --------
clean2 <- read.xlsx('full_data3.xlsx')
clean2 <- clean2[clean2$sales!=0,]


# extract data from amazscout ---------------------------------------------


conn <- dbConnect(RSQLite::SQLite(), "../data/amazon_sales.sqlite")
list_data <- dbGetQuery(conn, "SELECT asin,market_ids,sales from estsales")

write.csv(list_data,'list_data.csv')
#impute the missing value for average rating data



impute_missing <- function(data) {
  
  sub <- which(is.na(data$ave_rating)) # ???????????????????????????
  if (length(sub) !=0){
    # ??????????????????????????????????????????????????????
    data$listed_date <- as.numeric(data$listed_date)
    inputfile1 <- data[-sub,] 
    inputfile2 <- data[sub,]
    # ??????????????????
    # ??????:?????????~?????????
    model <- lm(ave_rating ~ listed_date, data = inputfile1) 
    # ????????????
    inputfile2$ave_rating <- predict(model, inputfile2) 
    result3 <- rbind(inputfile1, inputfile2)
    return(result3)
  } else{
    return(data)
  }
  
}


impute_missing_price <- function(data) {
  
  sub <- which(is.na(data$prices)) # ???????????????????????????
  if (length(sub) !=0){
    # ??????????????????????????????????????????????????????
    data$listed_date <- as.numeric(data$listed_date)
    inputfile1 <- data[-sub,] 
    inputfile2 <- data[sub,]
    # ??????????????????
    # ??????:?????????~?????????
    model <- lm(prices ~ listed_date, data = inputfile1) 
    # ????????????
    inputfile2$prices <- predict(model, inputfile2) 
    result3 <- rbind(inputfile1, inputfile2)
    return(result3)
  } else{
    return(data)
  }
  
}



clean3 <- impute_missing(clean2)
clean4 <- impute_missing_price(clean3)

saveRDS(clean4,'linear_missing_data.Rds')
kk <- clean2 %>%
  group_by(asin) %>%
  group_modify(~ impute_missing(.x))






