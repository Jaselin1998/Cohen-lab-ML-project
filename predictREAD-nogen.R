exgenresultsread <- tibble(female=10,CNTID=20,me_nogen=40)
nogencor_read <- data.frame(id=list_of_id, rmse=1,mae=2,correlation=3)
for (i in 1:l) {
  NEW <- Full_data %>%
    filter(CNTRYID == list_of_id[i])
  NEW[NEW == ''] <-  NA
  y <- NEW$PV1READ
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
                                 select(UH,NA_prop,
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
  x_train_nogen <- x_train%>%select(-starts_with('ST004'))
  x_test_nogen <- x_test%>%select(-starts_with('ST004'))
  set.seed(1134)
  ridge_nogen <- cv.glmnet(as.matrix(x_train_nogen), y_train,
                           weights = w_train, alpha = 0, nfolds = 5)
  
  predicted_nogen <- as.vector(predict(ridge_nogen, as.matrix(x_test_nogen), s = 'lambda.min'))
  error_nogen=data.frame(me_nogen=predicted_nogen-y_test)
  nogen_rmse = RMSE(predicted_nogen,y_test)
  nogen_mae = MAE(predicted_nogen,y_test)
  nogen_cor = cor(predicted_nogen,y_test,method = 'pearson')
  nogencor_read[i,2] =nogen_rmse
  nogencor_read[i,3]= nogen_mae
  nogencor_read[i,4] = nogen_cor
  cat('  ',i,'  no gender model built')
  gender1<-x_test%>%select(female=starts_with('ST004'))
  if( ncol(gender1)>0){
    gender1$CNTID<-list_of_id[i]
    subvar2 <- cbind(gender1,error_nogen)
    withoutgen <- aggregate(me_nogen~female+CNTID,data=subvar2,FUN = mean)
    exgenresultsread <- rbind(exgenresultsread,withoutgen)
  }
  cat('  country',i,'gender get ,end ')
  
}
exgenresultsread <- exgenresultsread[-1,]
exgenderread <- merge(exgenresultsread,hdi,by.x='CNTID',by.y='CNTRYID')
ggplot(exgenderread,aes(x=HDI,y=me_nogen,col=factor(female)))+geom_smooth(se=FALSE)+
  geom_point()+ggtitle('Mean error between gender for predicting Reading')+
  xlab('HDI')+ylab('Mean error(no gender)')
