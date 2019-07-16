library(ggpubr)
library(ggplot2)
library(dplyr)
library(glmnet)
library(caret)
library(reshape2)
library(knitr)
library(scatterplot3d)
library(tidyverse)

l <- length(list_of_id)
results <- tibble(id = list_of_id, RMSE = numeric(l), 
                  MAE = numeric(l), corr = numeric(l))
genresults <- tibble(female=10,CNTID=20,me=30)
immigresults <- tibble(immigstatus=c(10),CNTID=c(20),me=c(30))
escsresults <- data.frame(ESCS=c(10),CNTID=c(20),me=c(30))
noimmig <-c()
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
  error=data.frame(me=predicted-y_test)
  #get gender 
  gender1<-x_test%>%select(female=starts_with('ST004D01T2'))
  if( ncol(gender1)>0){
    gender1$CNTID<-list_of_id[i]
    subvar2 <- cbind(gender1,error)
    genfair <- aggregate(me~female+CNTID,data = subvar2,FUN = mean)
    genresults <- rbind(genresults,genfair)
  }
  cat('country',i,'gender get  ')
  #get immigration status
  immig1 <- x_test%>%select(starts_with('IMMIG'))
  if (ncol(immig1 ==0)){
    noimmig <-append(noimmig,list_of_id[i])
  }
  if(ncol(immig1)>0){
    if (i %in% c(1,4,5,6,10,14,
                 20,23,27,31,34,36,37,40,41,
                 42,52,53)){
      immig1$CNTID <- list_of_id[i]
      immig2 <- immig1%>%mutate(immigstatus=case_when(`IMMIG-99`==1~'UNKNOWN',
                                                      IMMIG1==1~'NATIVE',
                                                      IMMIG2==1~'SECOND-GEN',
                                                      IMMIG3==1~'FIRST-GEN'))%>%
        cbind(.,error)%>%select(immigstatus,CNTID,me)
      immig3 <- aggregate(me~immigstatus+CNTID,data = immig2,FUN = mean)
      immigresults<- rbind(immigresults,immig3)
    }
    if (i %in% c(3,33,17,24,28,46)){
      immig1$CNTID <- list_of_id[i]
      immig2 <- immig1%>%mutate(immigstatus=case_when(IMMIG1==1~'NATIVE',
                                                      IMMIG2==1~'SECOND-GEN',
                                                      IMMIG3==1~'FIRST-GEN'))%>%
        cbind(.,error)%>%select(immigstatus,CNTID,me)
      immig3 <- aggregate(me~immigstatus+CNTID,data = immig2,FUN = mean)
      immigresults<- rbind(immigresults,immig3)
    }
    if (i %in% c(21,50)){
      immig1$CNTID <- list_of_id[i]
      immig2 <- immig1%>%mutate(immigstatus=case_when(`IMMIG-99`==1~'UNKNOWN',
                                                      IMMIG1==1~'NATIVE',
                                                      IMMIG2==1~'SECOND-GEN'))%>%
        cbind(.,error)%>%select(immigstatus,CNTID,me)
      immig3 <- aggregate(me~immigstatus+CNTID,data = immig2,FUN = mean)
      immigresults<- rbind(immigresults,immig3)
    }
    if (i %in% c(43,44)){
      immig1$CNTID <- list_of_id[i]
      immig2 <- immig1%>%mutate(immigstatus=case_when(`IMMIG-99`==1~'UNKNOWN',
                                                      IMMIG2==1~'SECOND-GEN'))%>%
        cbind(.,error)%>%select(immigstatus,CNTID,me)
      immig3 <- aggregate(me~immigstatus+CNTID,data = immig2,FUN = mean)
      immigresults<- rbind(immigresults,immig3)
    }
    if (i %in% c(38,45)){
      immig1$CNTID <- list_of_id[i]
      immig2 <- immig1%>%mutate(immigstatus=case_when(IMMIG1==1~'NATIVE',
                                                      IMMIG2==1~'SECOND-GEN'))%>%
        cbind(.,error)%>%select(immigstatus,CNTID,me)
      immig3 <- aggregate(me~immigstatus+CNTID,data = immig2,FUN = mean)
      immigresults<- rbind(immigresults,immig3)
    }
  }
  cat('immigration get  ')
  #get social economical status
  escs1 <- x_test%>%select(starts_with('ESCS'))
  if(ncol(escs1)==1){
    escs1$CNTID <- list_of_id[i]
    escs1 <- cbind(escs1,error)
    escsresults <- rbind(escsresults,escs1)
  }
  cat('escs get  end')
  results[i, 2] <- RMSE(predicted, y_test)
  results[i, 3] <- MAE(predicted, y_test)
  results[i, 4] <- cor(predicted, y_test, method = 'pearson')[1]


}


theme_fair <- theme(panel.background = element_rect(fill='white'))
genresults<- genresults[-1,]
gender <- merge(genresults,countrygdp,by.x = 'CNTID',by.y = 'CNTRYID')
immigresults <- immigresults[-1,]
immigration <- merge(immigresults,countrygdp,by.x='CNTID',by.y = 'CNTRYID')
escsresults <- escsresults[-1,]
ggplot(gender,aes(x=HDI,y=me,col=factor(female)))+geom_smooth(se=FALSE)+geom_point()+
  ggtitle('Mean error between male and female')
ggplot(immigration,aes(x=HDI,y=me,col=factor(immigstatus)))+geom_smooth(method='lm',se=FALSE)+
  geom_point()+
  ggtitle('Mean error among immigration status')
escsresults <- escsresults[-1,]
ESCS <- merge(escsresults,countrygdp,by.x='CNTID',by.y='CNTRYID')
ESCS <- ESCS%>%select(ESCS,CNTID,CNT,me,HDI)%>%
  group_by(CNTID)%>%
  mutate(escslevel=cut(ESCS,breaks = c(-Inf,quantile(ESCS,c(0.25,0.5,0.75)),+Inf),
         labels=c('very low','low','high','very high')))%>%
  group_by(CNTID,escslevel)%>%mutate(me2=mean(me))
ESCS2 <- ESCS%>%select(CNTID,escslevel,me2,HDI,CNT)
ggplot(ESCS2,aes(x=HDI,y=me2,col=escslevel))+geom_smooth(method='lm',se=FALSE)+
  geom_point()+ggtitle('Mean error among ESCS levels')
ggplot(ESCS2,aes(x=HDI,y=me2,col=escslevel))+geom_smooth(method='lm',se=FALSE)+
  geom_point()+ggtitle('Mean error among ESCS levels')+
  xlab('HDI')+ylab('Mean error')
ggplot(ESCS,aes(x=ESCS,y=me))+geom_point(color='light blue')+
  geom_smooth(method = 'lm',se=FALSE,color='blue')+theme_fair+
  theme(legend.position = 'none')+
  geom_text(aes(x=-7,y=-300),label=paste('correlation:',round(cor(ESCS$ESCS,ESCS$me),2)))
ESCS2 %>%
  group_by(escslevel)%>%
  summarise(sd= sd(me2))
