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
results <- tibble(id = list_of_id, RMSE = numeric(l), 
                  MAE = numeric(l), corr = numeric(l))
exgenresults <- tibble(female=10,CNTID=20,me=40)
immigresults <- tibble(immigstatus=c(10),CNTID=c(20),me=c(30))
escsresults <- data.frame(ESCS=c(10),CNTID=c(20),me=c(30))
nogencor <- data.frame(rmse=1,mae=2,correlation=3)
prop_NA<- function(x){sum(is.na(x))/length(x)}
for (i in 1:l) {
  NEW <- Full_data %>%
    filter(CNTRYID == list_of_id[i])
  NEW[NEW == ''] <-  NA
  y <- NEW$PV1SCIE
  w <- NEW$W_FSTUWT
  x <- NEW %>%select(one_of(names_selected))
  
  x$NA_prop <- apply(x, 1, prop_NA)
  
  x$UH <- as.numeric(NEW$CBASCI == 5)
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
  x_train_nogen <- x_train%>%select(-starts_with('ST004'))
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
  x_test_nogen <- x_test%>%select(-starts_with('ST004'))
  set.seed(111)
  ridge <- cv.glmnet(as.matrix(x_train_nogen), y_train,
                     weights = w_train, alpha = 0, nfolds = 5)
  
  predicted <- as.vector(predict(ridge, as.matrix(x_test_nogen), s = 'lambda.min'))
  error=data.frame(me=predicted-y_test)
  #get gender 
  gender1<-x_test%>%select(ST=starts_with('ST004D'))
  if( ncol(gender1)>0){
    gender1$CNTID<-list_of_id[i]
    subvar2 <- cbind(gender1,error)
    if (names(subvar2)[1]=='ST004D01T1'){
      subvar2<- subvar2%>%mutate(female=ifelse(ST==1,0,1))
    }else{
      subvar2 <- subvar2%>%mutate(female=ST)
    }
    subvar2<- subvar2%>%
      select(female,CNTID,me)
    genfair <- aggregate(me~female+CNTID,data = subvar2,FUN = mean)
    exgenresults <- rbind(exgenresults,genfair)
  }
  cat('country',i,'no gender model get  ')
}
exgenresults <- exgenresults[-1,]
exgender <- merge(exgenresults,countrygdp,by.x='CNTID',by.y = 'CNTRYID')
exgender <- na.omit(exgender)
attach(exgender)
ggplot(exgender,aes(x=HDI,y=me,col=factor(female)))+
  geom_smooth(se=FALSE)+geom_point()+ggtitle('model excluding gender')
gender22 <- merge(exgender,genresults,by=c('CNTID','female'),suffixes = c('.nogen','withgen'))
exgender2 <- exgender%>%arrange(desc(HDI),desc(female))
ggplot(gender22,aes(y=CNT))+geom_path(aes(x=me.nogen),color='black',size=1)+
  geom_path(aes(x=mewithgen),color='yellow',size=0.5)+ggtitle('mean error between two models')+
  xlab('mean error between female and male')+ylab('Country')+theme_fair
  geom_text(aes(x=me.x,y=CNT),data= gender22,label=gender22$female)+
  geom_text(aes(x=me.y,y=CNT),data=gender22,label=gender22$female)
#test the relation between gender and other variables

rsquares <-data.frame(CNTRYID=list_of_id,rsq=0)
for (i in 1:l){
  NEW <- Full_data %>%
    filter(CNTRYID == list_of_id[i])
  NEW[NEW == ''] <-  NA
  w <- NEW$W_FSTUWT
  x <- NEW %>%select(one_of(names_selected))
  
  x$NA_prop <- apply(x, 1, prop_NA)
  
  x$UH <- as.numeric(NEW$CBASCI == 5)
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
                                 select(one_of(names_categorical)))
  
  x[x_names_categorical][is.na(x[x_names_categorical])] <- -99 
  x[x_names_categorical] <- 
    as.data.frame(lapply(x[x_names_categorical], function(x) factor(x)))
  
  nzv <- nearZeroVar(x)
  x <- x[-nzv]
  medianImpute <- preProcess(x, method = c('center','scale','medianImpute'))
  x<- predict(medianImpute, x)
  options(na.action = 'na.pass')
  x <- as.data.frame(model.matrix(~., x))
  corr <- preProcess(x, method = c('zv', 'corr'), cutoff = 0.90)
  xlm <- predict(corr,x)
  if( c('ST004D01T2')%in% names(xlm)){
    fit <- lm(ST004D01T2~.,xlm,weights = w,contrasts = FALSE)
  }
  rsq <- summary(fit)$r.squared
  rsquares[i,2] <- rsq
  cat('finish country ',i,' ')
}
rsqgdp <- merge(rsquares,countrygdp,by='CNTRYID')
ggplot(rsqgdp,aes(x=HDI,y=rsq))+geom_point(color='light blue')+geom_smooth(color='blue',method = 'lm',se=FALSE)+
              labs(x='HDI',y='R squared',title='R-squares distribution across countries')+
  theme(panel.background = element_rect(fill='white'))

t.test(rsqgdp$rsq)
