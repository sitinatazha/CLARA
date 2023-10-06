install.packages("cluster")
install.packages("factoextra")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("NbClust")
install.packages("fviz_nbclust")
install.packages("pastecs")
install.packages("corrplot")
install.packages("MVN")
install.packages("PerformanceAnalytics")

library(cluster)
library(factoextra)
library(tidyverse)
library(dplyr)
library(NbClust)
library(fviz_nbclust)
library(car)
library(readxl)
library(xlsx)
library(writexl)

#input data
df <- read.delim('clipboard')
head(df)

df1<- scale(df)
head(df1)
df1<-as.data.frame(df1)
head(df1)

write_xlsx(df1, path = D:\ab)

#Statistik Deskriptif
summary(df)

library(pastecs)
stat.desc(df)

#Asumsi Multikolinearitas
CekVif <- function(data){
  corr = as.matrix(cor(data))
  VIF = diag(solve(corr))
  return(VIF)
}
CekVif(df1[1:8])

#penentuan cluster optimal
fviz_nbclust(df,FUNcluster = clara,method = "silhouette")

#penghitungan CLARA
clara.result=clara(df, k=2, metric = "manhattan", stand = FALSE)
clara.result$clusinfo
print(clara.result)

#Silhouette width
ces=clara.result$silinfo
ces

clara.result2= clara(df, k=2, metric = "euclidean", stand = FALSE)
print(clara.result2)

#Silhouette width
cus=clara.result2$silinfo
cus

#tabel dan plotting hasil CLARA
cc=data.frame(df,clara.result$clustering)
head(cc)
view(cc)

fviz_cluster(clara.result, data = df)
   fviz_cluster(clara.result2, data = df)

plot(clara.result, main='Plot Indeks Silhouette CLARA')

#kmeans
km=kmeans(df,2,metric="manhattan")
km

#K-medoids
pm=pam(df,2,metric="manhattan")
pm

#silhouette
pms=pm$silinfo
pms


pmk=km$silinfo
pmk

#informasi cluster
cc[,1:2]%>%
  mutate(cluster=clara.result$clustering)%>%
  group_by(cluster)%>%
  summarise_all("mean")

#Profiling hasil cluster 
#rata-rata cluster hasil clara
profiling<- data.frame(aggregate(df, by=list(cluster=clara.result$cluster),
                                 mean))
profiling
profiling<- data.frame(aggregate(df, by=list(cluster=cc$cluster),
                                 mean))
profiling

write.csv(cc, "exportcsv.csv,row.names" = False)

