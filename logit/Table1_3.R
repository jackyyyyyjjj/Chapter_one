rm (list = ls(all=TRUE))#Clearn objects from the workspacegraphics.off() #Close the graphics
#Simple, Consistent Wrappers for Common String Operations
if ('stringr' %in% rownames(installed.packages())==FALSE) {
  install.packages('stringr')
}else{
  library(stringr)
}
#Extract this Rmd file's working directory
if (!'rstudioapi' %in% rownames(installed.packages())) {
  install.packages('rstudioapi')
}else{
  library(rstudioapi)
}

library(openxlsx)
#Export the regression result to latex format tables
if (!'stargazer' %in% rownames(installed.packages())) {
  install.packages('stargazer')
}else{
  library(stargazer)
}

#  A Grammar of Data Manipulation
if (!'dplyr' %in% rownames(installed.packages())) {
  install.packages('dplyr')
}else{
  library(dplyr)
}
gc(T)
#' 
#' # Set the working directory
## ----analysis,include=TRUE-----------------------------------------------------------------------------------------------------------
script_path <- rstudioapi::getActiveDocumentContext()$path
hash = "(.*)/"
setwd(str_extract(script_path, hash)) 



# p increasing ------------------------------------------------------------


table <- read.xlsx('logit_p_increasing3.xlsx',sheet = 'sum')
# table <- read.xlsx('logit_p_neutral.xlsx',sheet = 'sum')
#delete the non-convergy data
# table <- table[table$converge_pre==0,]
table_merge <- table[table$label=='*',]
table_merge <- table_merge[complete.cases(table$label),]
table_other <- table[!complete.cases(table$label),]
ConfiIter <- function(data){
  sample.mean <- mean(data)
  # print(sample.mean)
  sample.n <- length(data)
  sample.sd <- sd(data)
  sample.se <- sample.sd/sqrt(sample.n)
  # print(sample.se)
  
  alpha = 0.10
  degrees.freedom = sample.n - 1
  t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
  # print(t.score)
  
  margin.error <- t.score * sample.se
  lower.bound <- sample.mean - margin.error
  upper.bound <- sample.mean + margin.error
  return(c(lower.bound,upper.bound))
  
}

sum <- as.data.frame(summary(table_merge[,-c(1:3)]))
stargazer(table_merge[,-c(1:3)],digits = 3,type = 'text')


l.model <- lm(EstpricePre.pg ~ 1, table1)

# Calculate the confidence interval
confint(l.model, level=0.80)

table2 <- table_merge[,c(1,14:16)]
table2 <- table_other[,c(1,14:16)]
table2 <- table2[table2$priceDif.pg<1,]

# sumtab1 <- table2[-161,] %>% group_by(brand_pre) %>% 
#   summarise(price = mean(priceDif.pg)*100,
#             price10 = ConfiIter(priceDif.pg)[1]*100,
#             price90 = ConfiIter(priceDif.pg)[2]*100,
#             quality = mean(qualityDif.pg)*100,
#             quality10 = ConfiIter(qualityDif.pg)[1]*100,
#             quality90 = ConfiIter(qualityDif.pg)[2]*100,
#             shares = mean(EstsharDif.pg)*100,
#             shares10 = ConfiIter(EstsharDif.pg)[1]*100,
#             shares90 = ConfiIter(EstsharDif.pg)[2]*100)

sumtab1 <- table2 %>% 
  summarise(price = mean(priceDif.pg)*100,
            price10 = ConfiIter(priceDif.pg)[1]*100,
            price90 = ConfiIter(priceDif.pg)[2]*100,
            quality = mean(qualityDif.pg)*100,
            quality10 = ConfiIter(qualityDif.pg)[1]*100,
            quality90 = ConfiIter(qualityDif.pg)[2]*100,
            shares = mean(EstsharDif.pg)*100,
            shares10 = ConfiIter(EstsharDif.pg)[1]*100,
            shares90 = ConfiIter(EstsharDif.pg)[2]*100)

sumtab1 <- as.data.frame(sumtab1)
stargazer(sumtab1,summary = FALSE,digits = 3,type = 'text')



# table for p neutral -----------------------------------------------------

table <- read.xlsx('logit_p_neutral.xlsx',sheet = 'sum')
table_merge <- table[table$label=='*',]
table_merge <- table_merge[complete.cases(table$label),]
table_other <- table[!complete.cases(table$label),]
stargazer(table1,digits = 3)
ConfiIter <- function(data){
  sample.mean <- mean(data)
  # print(sample.mean)
  sample.n <- length(data)
  sample.sd <- sd(data)
  sample.se <- sample.sd/sqrt(sample.n)
  # print(sample.se)
  
  alpha = 0.10
  degrees.freedom = sample.n - 1
  t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
  # print(t.score)
  
  margin.error <- t.score * sample.se
  lower.bound <- sample.mean - margin.error
  upper.bound <- sample.mean + margin.error
  return(c(lower.bound,upper.bound))
  
}


sum <- as.data.frame(summary(table_merge[,-c(1:3)]))
stargazer(table_merge[,-c(1:3)],digits = 3)


l.model <- lm(Ob.price ~ 1, table1)

# Calculate the confidence interval
confint(l.model, level=0.80)

table2 <- table_merge[,c(1,15:17)]
table2 <- table_other[,c(1,15:17)]

sumtab1 <- table2 %>% group_by(brand_pre) %>% 
  summarise(Sigma = mean(deltaDif.pg)*100,
            Sigma10 = ConfiIter(deltaDif.pg)[1]*100,
            Sigma90 = ConfiIter(deltaDif.pg)[2]*100,
            quality = mean(qualityDif.pg)*100,
            quality10 = ConfiIter(qualityDif.pg)[1]*100,
            quality90 = ConfiIter(qualityDif.pg)[2]*100,
            shares = mean(EstsharDif.pg)*100,
            shares10 = ConfiIter(EstsharDif.pg)[1]*100,
            shares90 = ConfiIter(EstsharDif.pg)[2]*100)

sumtab1 <- table2 %>% 
  summarise(Sigma = mean(deltaDif.pg)*100,
            Sigma10 = ConfiIter(deltaDif.pg)[1]*100,
            Sigma90 = ConfiIter(deltaDif.pg)[2]*100,
            quality = mean(qualityDif.pg)*100,
            quality10 = ConfiIter(qualityDif.pg)[1]*100,
            quality90 = ConfiIter(qualityDif.pg)[2]*100,
            shares = mean(EstsharDif.pg)*100,
            shares10 = ConfiIter(EstsharDif.pg)[1]*100,
            shares90 = ConfiIter(EstsharDif.pg)[2]*100)

sumtab1 <- as.data.frame(sumtab1)
stargazer(sumtab1,summary = FALSE,digits = 2)




# Corollary 1 and proposition 1 -------------------------------------------

# table_incr <- read.xlsx('ces_d.xlsx',sheet = '')
data_pincr <-table_merge[,c(1,12:13)]
#Proposition 1



sumtab1 <- data_pincr %>% group_by(brand_pre) %>% 
  summarise(innodiff = mean(g.m_g)*100,
            g.m_g10 = ConfiIter(g.m_g)[1]*100,
            g.m_g90 = ConfiIter(g.m_g)[2]*100,
            margi_gain = mean(lm_hm)*100,
            lm_hm10 = ConfiIter(lm_hm)[1]*100,
            lm_hm90 = ConfiIter(lm_hm)[2]*100)

sumtab1 <- as.data.frame(sumtab1)
stargazer(sumtab1,summary = FALSE,digits = 3)

data_pneu <- table_merge[,c(1,12:14)]

sumtab2 <- data_pneu %>% group_by(brand_pre) %>% 
  summarise(innodiff = mean(g.m_g)*100,
            g.m_g10 = ConfiIter(g.m_g)[1]*100,
            g.m_g90 = ConfiIter(g.m_g)[2]*100,
            margi_gain = mean(lm_hm)*100,
            lm_hm10 = ConfiIter(lm_hm)[1]*100,
            lm_hm90 = ConfiIter(lm_hm)[2]*100,
            corr1 = mean(corollary1)*100,
            corr10 = ConfiIter(corollary1)[1]*100,
            corr90 = ConfiIter(corollary1)[2]*100)

sumtab2 <- as.data.frame(sumtab2)
stargazer(sumtab2,summary = FALSE,digits = 2)


