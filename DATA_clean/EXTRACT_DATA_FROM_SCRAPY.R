rm (list = ls(all=TRUE))
graphics.off()
library(stringr)
script_path <- rstudioapi::getActiveDocumentContext()$path
hash = "(.*)/"
setwd(str_extract(script_path, hash)) 
gc(T)
# PRODUCT DETAIL FROM SCRPAING --------------------------------------------
library(mongolite)
con2 <- mongo(
  collection = "top_firms",
  url = "mongodb://Junjun:123456@localhost:27017/taobao",
  verbose = FALSE,
  options = ssl_options()
)



amazon_sales <- con2$find(fields = '{"big_small_html" : true,"item_detail" : true,"ASIN" : true,"detail_page_url" : true,"listed_date" : true,"productOverview_feature" : true}')


text1 <- amazon_sales$big_small_html


# clean the amazon product detail technical detail data

text1 <- gsub('[{]','',text1)
text2 <- gsub('[}]','',text1)
text3 <- gsub('[(]','',text2)
text4 <- gsub('[)]','',text3)
text5 <- gsub('[;]','',text4)
text6 <- gsub('[*]','',text5)
text7 <- gsub('[||]','',text6)
text8 <- gsub('https.*/','',text7)
text9 <- gsub('[/]','',text8)
text10 <- gsub('\\\"','',text9)
text11 <- gsub('\'','',text10)
text12 <- gsub(',','',text11)

list_feature <- strsplit(text12,'\\s{4,}')

detail_item <- data.frame(product_features=c("Product Dimensions","Item Weight","Batteries","Warranty Description","Batteries Required?",
                                             "Included Components","Package Dimensions"))
empty_array <- c(seq(1,7,1))
for (j in seq(1,length(list_feature),1)) {
  kkkk <- length(list_feature[[j]])
  if(kkkk!=0){
    for (i in seq(1,kkkk,1)) {
      
      if (list_feature[[j]][i] %in% detail_item$product_features){
        location_1 <- which(detail_item$product_features==list_feature[[j]][i])
        empty_array[location_1] <- list_feature[[j]][i+1]
      }
      
    }
  }
  
  name_arr <- sprintf('product_%d',j)
  detail_item[name_arr] <-empty_array
  empty_array <- c(seq(1,7,1))
}
kkk <- t(detail_item)
ttt <- as.data.frame(kkk)
colnames(ttt) <- ttt[1,]
lll <- ttt[-1,]
# lll <- cbind(data3,ttt)
# clean the amazon claim detail data
text_item_detail <- amazon_sales$item_detail
text_item_detail <- tolower(text_item_detail)
text1 <- gsub('[{]','',text_item_detail)
text2 <- gsub('[}]','',text1)
text3 <- gsub('[(]','',text2)
text4 <- gsub('[)]','',text3)
text5 <- gsub('[;]','',text4)
text6 <- gsub('[*]','',text5)
text7 <- gsub('[||]','',text6)
text8 <- gsub('https.*/','',text7)
text9 <- gsub('[/]','',text8)
text10 <- gsub('\\\"','',text9)
text11 <- gsub('\'','',text10)
text12 <- gsub(',','',text_item_detail)
text13 <- gsub('\\s','',text12)
suction <- str_extract(text13,'\\d{3,}pa')
# 
jjjj <- data.frame(suction=suction)


#clean the product overview feature

productOverview_feature <- amazon_sales$productOverview_feature
text_item_detail <- tolower(productOverview_feature)
list_feature <- strsplit(text_item_detail,'\\s{4,}')

detail_item <- data.frame(product_features=c("color","surface recommendation","controller type","battery cell composition"))
empty_array <- c(seq(1,4,1))
for (j in seq(1,length(list_feature),1)) {
  kkkk <- length(list_feature[[j]])
  if(kkkk!=0){
    for (i in seq(1,kkkk,1)) {
      
      if (list_feature[[j]][i] %in% detail_item$product_features){
        location_1 <- which(detail_item$product_features==list_feature[[j]][i])
        empty_array[location_1] <- list_feature[[j]][i+1]
      }
      
    }
  }
  
  name_arr <- sprintf('product_%d',j)
  detail_item[name_arr] <-empty_array
  empty_array <- c(seq(1,4,1))
}
kkk <- t(detail_item)
ttt <- as.data.frame(kkk)
colnames(ttt) <- ttt[1,]
ttt <- ttt[-1,]
# 
# 
# 
feature_vacuum_robot <- cbind(lll,jjjj,ttt)
feature_vacuum_robot$`Package Dimensions`[which(feature_vacuum_robot$`Package Dimensions`==7)]=NA
feature_vacuum_robot$`Included Components`[which(feature_vacuum_robot$`Included Components`==6)]=NA
feature_vacuum_robot$`Batteries Required?`[which(feature_vacuum_robot$`Batteries Required?`==5)]=NA
feature_vacuum_robot$`Warranty Description`[which(feature_vacuum_robot$`Warranty Description`==4)]=NA
feature_vacuum_robot$Batteries[which(feature_vacuum_robot$Batteries==3)]=NA
feature_vacuum_robot$`Item Weight`[which(feature_vacuum_robot$`Item Weight`==2)]=NA
feature_vacuum_robot$`Product Dimensions`[which(feature_vacuum_robot$`Product Dimensions`==1)]=NA

feature_vacuum_robot$color[which(feature_vacuum_robot$color==1)]=NA
feature_vacuum_robot$`surface recommendation`[which(feature_vacuum_robot$`surface recommendation`==2)]=NA
feature_vacuum_robot$`controller type`[which(feature_vacuum_robot$`controller type`==3)]=NA
feature_vacuum_robot$`battery cell composition`[which(feature_vacuum_robot$`battery cell composition`==4)]=NA

feature_vacuum_robot['product.dimension'] <- paste(feature_vacuum_robot$`Product Dimensions`,feature_vacuum_robot$`Package Dimensions`,sep = '')

kk <- feature_vacuum_robot$product.dimension
kk <- gsub('\\s','',kk)
ttttt <- str_split(kk,'x')
number_vector <- c()
for (i in seq(1,length(ttttt),1)) {
  FOR_LOOP <- as.numeric(gsub('[A-Za-z]','',ttttt[[i]]))
  res <- 1
  if(length(FOR_LOOP)!=0){
    for (j in 1:length(FOR_LOOP)) {
      res <- res * FOR_LOOP[j]
    }
  }
  
  number_vector <- append(number_vector,res)
  
}
feature_vacuum_robot['numeric_product.dimension'] <- number_vector
feature_vacuum_robot['Item_Weight'] <- gsub('[A-Za-z]','',feature_vacuum_robot$`Item Weight`)
feature_vacuum_robot$asin <- amazon_sales$ASIN
saveRDS(feature_vacuum_robot,'feature_vacuum_robot.Rds')


# the data from keepa -----------------------------------------------------

library('RSQLite')
library('DBI')
library('dplyr')
conn <- dbConnect(RSQLite::SQLite(), "C:/Users/hpe21bnu/OneDrive - University of East Anglia/Documents/UEA_PHD/First_charpter/Top_firms/KEEPA/keepa.sqlite")
kk <- dbGetQuery(conn, "SELECT asin,title from KEEPA1")
dbDisconnect(conn)
kk$title <- mapply(tolower,kk$title)
write.csv(kk,'kk.csv')


# #delete the paragraphs with replacement word
# kk1 <- kk[!grepl('replacement',kk$title),]
# #delete the paragraphs with filters word
# kk1 <- kk1[!grepl('filters',kk1$title),]
# #delete the paragraphs with filters word
# kk1 <- kk1[!grepl('filter',kk1$title),]
# #delete the paragraphs with filters word
# kk1 <- kk1[!grepl('brush for',kk1$title),]
# #delete the paragraphs with filters word
# kk1 <- kk1[!grepl('sticker for',kk1$title),]
# #delete the paragraphs with filters word
# kk1 <- kk1[!grepl('accessories',kk1$title),]
# #delete the paragraphs with filters word
# kk1 <- kk1[!grepl('tank for',kk1$title),]
# #delete the paragraphs with filters word
# kk1 <- kk1[!grepl('kit for',kk1$title),]
# #delete the paragraphs with filters word
# kk1 <- kk1[!grepl('mop for',kk1$title),]
# #delete the paragraphs with filters word
# kk1 <- kk1[!grepl('mop for',kk1$title),]
# kk1 <- kk1[!grepl('charger for',kk1$title),]
# kk1 <- kk1[!grepl('4pcs/pack for',kk1$title),]
# kk1 <- kk1[!grepl('dock for',kk1$title),]
# kk1 <- kk1[!grepl('parts fit',kk1$title),]
# kk1 <- kk1[!grepl('battery compatible',kk1$title),]
# 
# kk1 <- kk1[!grepl('wipes fit',kk1$title),]
# 
# kk1 <- kk1[!grepl('tank compatible',kk1$title),]
# kk1 <- kk1[!grepl('2pcs',kk1$title),]
# 
# kk1 <- kk1[!grepl('6 pcs mop ',kk1$title),]
# kk1 <- kk1[!grepl('CCP',kk1$title),]
# 
# kk1 <- kk1[!grepl('6 pcs disposable mop',kk1$title),]
# kk1 <- kk1[!grepl('dustbin for',kk1$title),]
# kk1 <- kk1[!grepl('skin compatible',kk1$title),]
library(dplyr)

kk2 <- kk %>%distinct(asin, .keep_all =TRUE)
kk2 <- kk2[!grepl('battery pack for',kk2$title),]
kk2 <- kk2[!grepl('sponge vacuum cup',kk2$title),]

# kk2$title <- gsub('\\(.+\\)','',kk2$title)
# kk2 <- kk2 %>%distinct(title, .keep_all =TRUE)


conn <- dbConnect(RSQLite::SQLite(), "C:/Users/hpe21bnu/OneDrive - University of East Anglia/Documents/UEA_PHD/First_charpter/Top_firms/KEEPA/keepa.sqlite")
kk3 <- dbGetQuery(conn, "SELECT asin,title,brand,productGroup,partNumber,features,description,itemHeight,itemLength,itemWidth,itemWeight from KEEPA1")
dbDisconnect(conn)

kk4 <- kk3[kk3$asin %in% kk2$asin,]


# library(openxlsx)
# procahar <- read.csv('raw_char.csv')
# kk5 <- kk4[kk4$asin %in% procahar$asin,]
# # write.csv(kk,'raw_detail.csv')
# feature_vacuum_robot <- readRDS('feature_vacuum_robot.Rds')


kk4$t_fe_des <- paste(kk4$title,kk4$features,kk4$description,sep = ' ')


kk4$t_fe_des <- tolower(kk4$t_fe_des)  




#
# # clean the amazon product detail technical detail data
#
text1 <- kk4$t_fe_des
text1 <- gsub('[{]','',text1)
text1 <- gsub('[\\\n]',' ',text1)
text2 <- gsub('[}]','',text1)
text3 <- gsub('[(]','',text2)
text4 <- gsub('[)]','',text3)
text5 <- gsub('[;]','',text4)
text6 <- gsub('[*]','',text5)
text7 <- gsub('[||]','',text6)
text8 <- gsub('https.*/',' ',text7)
text9 <- gsub('[/]',' ',text8)
text10 <- gsub('\\\"',' ',text9)
text12 <- gsub(',',' ',text10)

list_feature <- strsplit(text12,'\\s{1,}')

detail_item <- data.frame(product_features=c("db","pa","minutes",'mins',"hour","hours",'mah',
                                             "min","h"))

empty_array <- c(rep(NA,9))


for (j in seq(1,length(list_feature),1)) {
  kkkk <- length(detail_item[,1])
  if(kkkk!=0){
    for (i in seq(1,kkkk,1)) {
      
      # if (length(which(mapply(grepl,detail_item$product_features[1:7],list_feature[[j]][i])))==1){
      #   location_1 <- which(mapply(grepl,detail_item$product_features,list_feature[[j]][i]))
      #   if (i==1){
      #     empty_array[location_1] <- NA
      #   } else{
      #     empty_array[location_1] <- list_feature[[j]][i-1]
      #   }
      #   
      # }
      if (length(list_feature[[j]][mapply(grepl,detail_item$product_features[i],list_feature[[j]])])!=0){
        empty_array[i] <- paste(list_feature[[j]][mapply(grepl,detail_item$product_features[i],list_feature[[j]])],collapse = ';')
        pass <- paste(list_feature[[j]][which(mapply(grepl,detail_item$product_features[i],list_feature[[j]]))-1],collapse = ';')
        empty_array[i] <- paste(empty_array[i],pass,collapse = ';')
      }
      
    }
  }
  
  name_arr <- sprintf('product_%d',j)
  detail_item[name_arr] <-empty_array
  empty_array <- c(rep(NA,9))
}




# for (j in seq(1,length(list_feature),1)) {
#   kkkk <- length(list_feature[[j]])
#   if(kkkk!=0){
#     for (i in seq(1,kkkk,1)) {
#       
#       # if (length(which(mapply(grepl,detail_item$product_features[1:7],list_feature[[j]][i])))==1){
#       #   location_1 <- which(mapply(grepl,detail_item$product_features,list_feature[[j]][i]))
#       #   if (i==1){
#       #     empty_array[location_1] <- NA
#       #   } else{
#       #     empty_array[location_1] <- list_feature[[j]][i-1]
#       #   }
#       #   
#       # }
#       
#       paste(list_feature[[j]][mapply(grepl,'vacuum',list_feature[[j]])],collapse = ';')
#       tryCatch({paste(list_feature[[j]][which(mapply(grepl,'vacuum',list_feature[[j]]))-1],collapse = ';')  ## ?????????????????????
#       },error = function(e){
#         expr2..  
#       },warning = function(w){
#         expr3.. 
#       })
#       
#       paste(list_feature[[j]][which(mapply(grepl,'vacuum',list_feature[[j]]))-1],collapse = ';')
#       
#       if (grepl('.+?db',list_feature[[j]][i])){
#         empty_array[8] <- str_extract(list_feature[[j]][i],'(.+?)db')
#       }
#       if (grepl('.+?pa',list_feature[[j]][i])){
#         empty_array[9] <- str_extract(list_feature[[j]][i],'(.+?)pa')
#       }
#       if (grepl('.+?minutes',list_feature[[j]][i])){
#         empty_array[10] <- str_extract(list_feature[[j]][i],'(.+?)minutes')
#       }
#       if (grepl('.+?mins',list_feature[[j]][i])){
#         empty_array[11] <- str_extract(list_feature[[j]][i],'(.+?)mins')
#       }
#       if (grepl('.+?min',list_feature[[j]][i])){
#         empty_array[12] <- str_extract(list_feature[[j]][i],'(.+?)min')
#       }
# 
#       
#       if (grepl('.+?hour',list_feature[[j]][i])){
#         empty_array[13] <- str_extract(list_feature[[j]][i],'(.+?)hour')
#       }
#       if (grepl('.+?h',list_feature[[j]][i])){
#         empty_array[14] <- str_extract(list_feature[[j]][i],'(.+?)h')
#       }
#       if (grepl('.+?hours',list_feature[[j]][i])){
#         empty_array[15] <- str_extract(list_feature[[j]][i],'(.+?)hours')
#       }
#       if (grepl('.+?mah',list_feature[[j]][i])){
#         empty_array[16] <- str_extract(list_feature[[j]][i],'(.+?)mah')
#       }
# 
#     }
#   }
#   
#   name_arr <- sprintf('product_%d',j)
#   detail_item[name_arr] <-empty_array
#   empty_array <- c(seq(1,16,1))
# }

kkk <- t(detail_item)
ttt <- as.data.frame(kkk)
colnames(ttt) <- ttt[1,]
lll <- ttt[-1,]
lll <- cbind(kk4,lll)

fulldata1 <- lll[lll$asin %in% feature_vacuum_robot$asin,]

fulldata1 <- merge(fulldata1,feature_vacuum_robot,by='asin')


# fulldata1$db[which(fulldata1$db==1)]=NA
# fulldata1$pa[which(fulldata1$pa==2)]=NA
# fulldata1$minutes[which(fulldata1$minutes==3)]=NA
# fulldata1$mins[which(fulldata1$mins==4)]=NA
# fulldata1$hour[which(fulldata1$hour==5)]=NA
# fulldata1$hours[which(fulldata1$hours==6)]=NA
# fulldata1$mah[which(fulldata1$mah==7)]=NA
# 
# fulldata1$db2[which(fulldata1$db2==8)]=NA
# fulldata1$pa2[which(fulldata1$pa2==9)]=NA
# fulldata1$minutes2[which(fulldata1$minutes2==10)]=NA
# fulldata1$mins2[which(fulldata1$mins2==11)]=NA
# fulldata1$min2[which(fulldata1$min2==12)]=NA
# fulldata1$hour2[which(fulldata1$hour2==13)]=NA
# fulldata1$h[which(fulldata1$h==14)]=NA
# fulldata1$hours2[which(fulldata1$hours2==15)]=NA
# fulldata1$mah2[which(fulldata1$mah2==16)]=NA


only_char <- fulldata1
write.csv(only_char,'product.dea2.csv')






# extract_numerical data --------------------------------------------------

library(dplyr)
data_ext <- only_char %>%
  mutate(db = mapply(paste, str_extract_all(db,'\\d+'),collapse = ';'),
         pa = mapply(paste,str_extract_all(pa,'\\d+'),collapse = ';'),
         minutes = mapply(paste,str_extract_all(minutes,'\\d+'),collapse = ';'),
         mins = mapply(paste,str_extract_all(mins,'\\d+'),collapse = ';'),
         hour = mapply(paste,str_extract_all(hour,'\\d+'),collapse = ';'),
         hours = mapply(paste,str_extract_all(hours,'\\d+'),collapse = ';'),
         mah = mapply(paste,str_extract_all(mah,'\\d+'),collapse = ';'),
         min = mapply(paste,str_extract_all(min,'\\d+'),collapse = ';'),
         h = mapply(paste,str_extract_all(h,'\\d+'),collapse = ';'))



data_ext2 <- data_ext[data_ext$pa!='',]
data_ext3 <- data_ext2[data_ext2$pa!='NA',]
write.csv(data_ext,'product.dea3_NO_EMSUCTION.csv')




# # clean the amazon claim detail data
# text_item_detail <- kk$description
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


# clean the product overview feature

productOverview_feature <- kk$description
text_item_detail <- tolower(productOverview_feature)
list_feature <- strsplit(text_item_detail,'\\s{1,}')

detail_item <- data.frame(product_features=c("db","pa","minutes","hour","hours"))
empty_array <- c(seq(1,5,1))
for (j in seq(1,length(list_feature),1)) {
  kkkk <- length(list_feature[[j]])
  if(kkkk!=0){
    for (i in seq(1,kkkk,1)) {
      
      if (list_feature[[j]][i] %in% detail_item$product_features){
        location_1 <- which(detail_item$product_features==list_feature[[j]][i])
        empty_array[location_1] <- list_feature[[j]][i-1]
      }
      
    }
  }
  
  name_arr <- sprintf('product_%d',j)
  detail_item[name_arr] <-empty_array
  empty_array <- c(seq(1,5,1))
}
kkk <- t(detail_item)
ttt <- as.data.frame(kkk)
colnames(ttt) <- ttt[1,]
ttt <- ttt[-1,]



feature_vacuum_robot <- cbind(lll,ttt)


readdata <- read.csv('raw_char2 - Copy.csv')


feature_vacuum_robot2 <- feature_vacuum_robot[feature_vacuum_robot$asin %in% readdata$asin,]

write.csv(feature_vacuum_robot2,'feature_vacuum_robot2.csv')



feature_vacuum_robot$`Package Dimensions`[which(feature_vacuum_robot$`Package Dimensions`==7)]=NA
feature_vacuum_robot$`Included Components`[which(feature_vacuum_robot$`Included Components`==6)]=NA
feature_vacuum_robot$`Batteries Required?`[which(feature_vacuum_robot$`Batteries Required?`==5)]=NA
feature_vacuum_robot$`Warranty Description`[which(feature_vacuum_robot$`Warranty Description`==4)]=NA
feature_vacuum_robot$Batteries[which(feature_vacuum_robot$Batteries==3)]=NA
feature_vacuum_robot$`Item Weight`[which(feature_vacuum_robot$`Item Weight`==2)]=NA
feature_vacuum_robot$`Product Dimensions`[which(feature_vacuum_robot$`Product Dimensions`==1)]=NA

feature_vacuum_robot$color[which(feature_vacuum_robot$color==1)]=NA
feature_vacuum_robot$`surface recommendation`[which(feature_vacuum_robot$`surface recommendation`==2)]=NA
feature_vacuum_robot$`controller type`[which(feature_vacuum_robot$`controller type`==3)]=NA
feature_vacuum_robot$`battery cell composition`[which(feature_vacuum_robot$`battery cell composition`==4)]=NA

feature_vacuum_robot['product.dimension'] <- paste(feature_vacuum_robot$`Product Dimensions`,feature_vacuum_robot$`Package Dimensions`,sep = '')


kk <- feature_vacuum_robot$product.dimension
kk <- gsub('\\s','',kk)
ttttt <- str_split(kk,'x')
number_vector <- c()
for (i in seq(1,length(ttttt),1)) {
  FOR_LOOP <- as.numeric(gsub('[A-Za-z]','',ttttt[[i]]))
  res <- 1
  if(length(FOR_LOOP)!=0){
    for (j in 1:length(FOR_LOOP)) {
      res <- res * FOR_LOOP[j]
    }
  }
  
  number_vector <- append(number_vector,res)
  
}
feature_vacuum_robot['numeric_product.dimension'] <- number_vector
feature_vacuum_robot['Item_Weight'] <- gsub('[A-Za-z]','',feature_vacuum_robot$`Item Weight`)
feature_vacuum_robot$asin <- amazon_data_no_duplicate$ASIN

