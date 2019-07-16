library(ggpubr)
library(ggplot2)
library(dplyr)
library(glmnet)
library(caret)
library(tidyverse)
library(reshape2)
library(knitr)
library(scatterplot3d)
theme_fair <- theme(panel.background = element_rect(fill='white'))
list_of_id <- unique(Full_data$CNTRYID)
l <- length(list_of_id)
female.results <- tibble(CNTID=list_of_id,me=40)
male.results <- tibble(CNTID=list_of_id,me=40)
prop_NA<- function(x){sum(is.na(x))/length(x)}
#for female
for (i in 1:l) {
  NEW <- Full_data %>%
    filter(CNTRYID == list_of_id[i])
  NEW[NEW == ''] <-  NA
  FM <- NEW%>%filter(ST004D01T==2)
  y <- FM$PV1SCIE
  w <- FM$W_FSTUWT
  x <- FM %>%select(one_of(names_selected))
  x$NA_prop <- apply(x, 1, prop_NA)
  
  x$UH <- as.numeric(FM$CBASCI == 5)
  x$UH[is.na(x$UH)] <- 0
  
  x$ST011D17TA[endsWith(x$ST011D17TA, '1')] <- 1
  x$ST011D17TA[endsWith(x$ST011D17TA, '2')] <- 2
  x$ST011D18TA[endsWith(x$ST011D18TA, '1')] <- 1
  x$ST011D18TA[endsWith(x$ST011D18TA, '2')] <- 2
  x$ST011D19TA[endsWith(x$ST011D19TA, '1')] <- 1
  x$ST011D19TA[endsWith(x$ST011D19TA, '2')] <- 2
  
  x$OCOD1 <- str_extract(x$OCOD1, '^.{1}')
  x$OCOD2 <- str_extract(x$OCOD2, '^.{1}')
  x$OCOD3 <- str_extract(x$OCOD3, '^.{1}')
  x_names_categorical <- names(x %>% 
                                 select(UH,
                                        one_of(names_categorical)))
  
  x[x_names_categorical][is.na(x[x_names_categorical])] <- -99 
  set.seed(111)
  train_index <- createDataPartition(y, p = 0.8, list = F)
  x_train <- x[train_index,]
  x_test <- x[-train_index,]
  y_train <- y[train_index]
  y_test <- y[-train_index]
  w_train <- w[train_index]
  w_test <- w[-train_index]
  
  x_train[x_names_categorical][is.na(x_train[x_names_categorical])] <- -99
  # Different from the setting where we only train one model on the US, 
  # the training set is also added a row to avoid problems
  x_train <- add_row(x_train)
  x_train[nrow(x_train), x_names_categorical] <- -100 
  x_train[x_names_categorical] <- 
    as.data.frame(lapply(x_train[x_names_categorical], function(x) factor(x)))
  
  nzv <- nearZeroVar(x_train)
  x_train <- x_train[-nzv]
  
  medianImpute <- preProcess(x_train, method = c('center', 'scale', 'medianImpute'))
  x_train <- predict(medianImpute, x_train)
  
  options(na.action = 'na.pass')
  x_train <- as.data.frame(model.matrix(~., x_train))
  x_train <- x_train[-nrow(x_train),]
  
  corr <- preProcess(x_train, method = c('zv', 'corr'), cutoff = 0.90)
  x_train <- predict(corr, x_train)
  x_test[x_names_categorical][is.na(x_test[x_names_categorical])] <- -99
  x_test <- add_row(x_test)
  x_test[nrow(x_test), x_names_categorical] <- -100 
  x_test[x_names_categorical] <- 
    as.data.frame(lapply(x_test[x_names_categorical], function(x) factor(x)))
  
  x_test <- x_test[-nzv]
  x_test <- predict(medianImpute, x_test)
  x_test <- as.data.frame(model.matrix(~., x_test))
  x_test <- x_test[-nrow(x_test),]
  
  train_minus_test <- setdiff(names(x_train), names(x_test))
  if (length(train_minus_test) > 0) x_test[train_minus_test] <- 0
  x_test <- select(x_test, names(x_train))
  
  x_test <- predict(corr, x_test)
  set.seed(111)
  ridge <- cv.glmnet(as.matrix(x_train), y_train,
                     weights = w_train, alpha = 0, nfolds = 5)
  
  predicted <- as.vector(predict(ridge, as.matrix(x_test), s = 'lambda.min'))
  #get gender 
  female.results[i,2]<- mean(predicted-y_test)
  cat('country',i,'female model get  ')
}
female.results <- merge(female.results,countrygdp,by.x='CNTID',
                        by.y = 'CNTRYID')

#for male
for (i in 1:l) {
  NEW <- Full_data %>%
    filter(CNTRYID == list_of_id[i])
  NEW[NEW == ''] <-  NA
  FM <- NEW%>%filter(ST004D01T==1)
  y <- FM$PV1SCIE
  w <- FM$W_FSTUWT
  x <- FM %>%select(one_of(names_selected),-ST004D01T)
  x$NA_prop <- apply(x, 1, prop_NA)
  
  x$UH <- as.numeric(FM$CBASCI == 5)
  x$UH[is.na(x$UH)] <- 0
  
  x$ST011D17TA[endsWith(x$ST011D17TA, '1')] <- 1
  x$ST011D17TA[endsWith(x$ST011D17TA, '2')] <- 2
  x$ST011D18TA[endsWith(x$ST011D18TA, '1')] <- 1
  x$ST011D18TA[endsWith(x$ST011D18TA, '2')] <- 2
  x$ST011D19TA[endsWith(x$ST011D19TA, '1')] <- 1
  x$ST011D19TA[endsWith(x$ST011D19TA, '2')] <- 2
  
  x$OCOD1 <- str_extract(x$OCOD1, '^.{1}')
  x$OCOD2 <- str_extract(x$OCOD2, '^.{1}')
  x$OCOD3 <- str_extract(x$OCOD3, '^.{1}')
  x_names_categorical <- names(x %>% 
                                 select(UH,
                                        one_of(names_categorical)))
  
  x[x_names_categorical][is.na(x[x_names_categorical])] <- -99 
  set.seed(111)
  train_index <- createDataPartition(y, p = 0.8, list = F)
  x_train <- x[train_index,]
  x_test <- x[-train_index,]
  y_train <- y[train_index]
  y_test <- y[-train_index]
  w_train <- w[train_index]
  w_test <- w[-train_index]
  
  x_train[x_names_categorical][is.na(x_train[x_names_categorical])] <- -99
  # Different from the setting where we only train one model on the US, 
  # the training set is also added a row to avoid problems
  x_train <- add_row(x_train)
  x_train[nrow(x_train), x_names_categorical] <- -100 
  x_train[x_names_categorical] <- 
    as.data.frame(lapply(x_train[x_names_categorical], function(x) factor(x)))
  
  nzv <- nearZeroVar(x_train)
  x_train <- x_train[-nzv]
  
  medianImpute <- preProcess(x_train, method = c('center', 'scale', 'medianImpute'))
  x_train <- predict(medianImpute, x_train)
  
  options(na.action = 'na.pass')
  x_train <- as.data.frame(model.matrix(~., x_train))
  x_train <- x_train[-nrow(x_train),]
  
  corr <- preProcess(x_train, method = c('zv', 'corr'), cutoff = 0.90)
  x_train <- predict(corr, x_train)
  x_test[x_names_categorical][is.na(x_test[x_names_categorical])] <- -99
  x_test <- add_row(x_test)
  x_test[nrow(x_test), x_names_categorical] <- -100 
  x_test[x_names_categorical] <- 
    as.data.frame(lapply(x_test[x_names_categorical], function(x) factor(x)))
  
  x_test <- x_test[-nzv]
  x_test <- predict(medianImpute, x_test)
  x_test <- as.data.frame(model.matrix(~., x_test))
  x_test <- x_test[-nrow(x_test),]
  
  train_minus_test <- setdiff(names(x_train), names(x_test))
  if (length(train_minus_test) > 0) x_test[train_minus_test] <- 0
  x_test <- select(x_test, names(x_train))
  
  x_test <- predict(corr, x_test)
  set.seed(111)
  ridge <- cv.glmnet(as.matrix(x_train), y_train,
                     weights = w_train, alpha = 0, nfolds = 5)
  
  predicted <- as.vector(predict(ridge, as.matrix(x_test), s = 'lambda.min'))
  #get gender 
  male.results[i,2]<- mean(predicted-y_test)
  cat('country',i,'female model get  ')
}
male.results <- merge(male.results,countrygdp,by.x='CNTID',
                        by.y = 'CNTRYID')
ggplot(male.results,aes(x=HDI,y=me))+geom_point(color='red')+
  geom_smooth(se=FALSE,color='red')+
  geom_point(data=female.results,aes(x=HDI,y=me),color='black')+
  geom_smooth(data=female.results,aes(x=HDI,y=me),color='black',
              se=FALSE)+
  labs(title='Mean error between female and male models',x='HDI',
                             y='Mean error')

ggplot(gender,aes(x=HDI,y=me,col=factor(female)))+geom_point()+
  geom_smooth(se=FALSE,method = 'lm')+
  geom_point(data=male.results, aes(x=HDI,y=me),color='red')+
  geom_smooth(data=male.results, aes(x=HDI,y=me), method = 'lm',se=FALSE,color='red')+
  geom_point(data=female.results,aes(x=HDI,y=me),color='black')+
  geom_smooth(data=female.results,aes(x=HDI,y=me), method = 'lm',color='black',
              se=FALSE)+geom_text(aes(x=0.7,y=7),color='black', label='black: female models')+
  geom_text(aes(x=0.7,y=9),label='red:male models',color='red')+
  labs(title='Mean error between separate and integrate models',x='HDI',
                              y='Mean error')
