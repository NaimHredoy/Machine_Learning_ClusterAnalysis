# MD SHAH NAIM HREDOY
# hridhowlader@gmail.com

#setting up working directory and import data

setwd("F:/GISPrograming/waterquality")
data<-read.csv(file.choose(), header=T)

#Checking data structure and correlation among the variable

str(data)
pairs.panels(data,
             gap=1,
             pch = 21)

#another fancy way of showing correlation, (corrplot package).

corrplot (cor(data), 
          method = "ellips")

#scaling the data set, (mean-value)/std
#this will return a data set whose mean is 0 with a unit std

mean<-apply(data,2,mean)
sd<-apply(data,2,mean)
scale<-scale(data,mean,sd)

#____________k-means clustering____________________________________

kmeans<-kmeans(scale,3)

#attribute of the cluster.

kmeans$cluster
kmeans$size      #other attribute are also available like iter, betweenss etc.
kmeans$centers

#checking data correspond to their cluster

cbind<-cbind(kmeans$cluster )
clusplot(scale,
         kmeans$cluster,
         main="2D representation of cluster", 
         shade = T,
         labels = 2,
         lines = 0 )
#end