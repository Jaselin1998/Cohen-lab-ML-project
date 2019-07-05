features <- read.csv('clustering.csv')
feature2 <- features%>%dplyr::select(-1,-426)
library(vegan)
library(factoextra)
library(ykmeans)
library(cluster)
library(permute)
library(NbClust)
library(pvclust)
library(bclust)

feature_mat <- as.matrix(feature2,ncol=424,byrow = TRUE)
maxc <- nrow(feature_mat)-1
cafit <- cascadeKM(feature_mat,1,maxc,iter=100,criterion = 'calinski')
cavalue <- cafit$results
capart <- cafit$partition

plot(cafit)
calinski <- data.frame(clusternum = c(1:(nrow(feature_mat)-1)),chindex=cavalue[2,],silindex=2,wss=3,sse=cavalue[1,])
sil <- fviz_nbclust(feature_mat,hcut, method = 'silhouette',k.max = 54)
calinski$silindex <- sil$data[,2]
casize <- cafit$size
casize
calinski$chindex[1]<- quantile(chindex,0.3,na.rm = TRUE)
attach(calinski)
ggplot(calinski,aes(x=clusternum,y=chindex))+geom_line()+geom_point()+
  xlab('Number of cluster')+ylab('Calinski Index')+
  theme(panel.background = element_rect(fill='white'))+
  geom_line(aes(x=which(chindex==max(chindex))),linetype=3)+ggtitle('Country clustering')

ggplot(calinski,aes(x=clusternum,y=silindex))+geom_line()+geom_point()+
  xlab('Number of cluster')+ylab('Silhouette Index')+xlim(c(1,54))+
  theme(panel.background = element_rect(fill='white'))+
  geom_line(aes(x=which(silindex==max(silindex))),linetype=3)+ggtitle('Country clustering')
countrycluster <- features%>%
  dplyr::select(1)%>%
  cbind(.,capart[,2])%>%
  dplyr::select(country=X,Cluster=capart[,2])
attach(countrycluster)
countrycluster%>%group_by(Cluster54)
cluster1 <- countrycluster%>%filter(Cluster54==1)%>%dplyr::select(country)
plot(hclust(dist(feature_mat),method = 'ward.D'),labels = countrycluster$Cluster55)
fviz_nbclust(feature_mat,hcut, method = 'wss',k.max = nrow(feature_mat)-1)
fviz_nbclust(feature_mat,hcut,method = 'gap_stat',k.max = nrow(feature_mat)-1)
nb_clust <- NbClust(feature_mat,  distance = "euclidean",
                    min.nc=1, max.nc=30, method = "Ward.D2",
                    index = "alllong", alphaBeale = 0.1)

library(mclust)
summary(Mclust(feature_mat,G=2:30))
mclust <- Mclust(feature_mat,G=1:30)
plot(mclust)
getk <- function(start_k=1,end_k,df){
  withinerror <-c()
  for  ( k in start_k:end_k){
    kmeansmodel <-kmeans(df,k, algorithm = 'MacQueen')
    withinerror[k] <- kmeansmodel$tot.withinss
  }
  plot(1:end_k,withinerror,type = 'b',xlim = c(1,end_k),ylim = c(1,max(withinerror)),
       xlab = 'number of cluster',ylab = 'total within sum of square',main = 'kmeans cluster analysis')
}
mk <- getk(1,30,feature_mat)

d.apclus <- apcluster(negDistMat(r=2), d)
cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")
# 4
heatmap(d.apclus)
plot(d.apclus, d)
