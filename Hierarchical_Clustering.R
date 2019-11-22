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

#calculating distance matrix

dist<-dist(scale, 
           method = "euclidean")

#________________hierarchical clustering- complete linkage___________________________

hclst.c<-hclust(dist,
                method = "complete")
plot(hclst.c, hang = -1)

#clustering membership and means

member.c<-cutree(hclst.c, 4)
rect.hclust(hclst.c, k=4, border = "red")
member.c
table(member.c)
aggregate(scale,
          list(member.c),
          mean)

#_________________hierarchical clustering- ward method___________________________

hclst.w<-hclust(dist,
                method = "ward.D2")
plot(hclst.w, hang = -1)

#clustering membership and means

member.w<-cutree(hclst.w, 4)
rect.hclust(hclst.w, k=4, border = "red")
member.w
table(member.w)
aggregate(scale,
          list(member.w),
          mean)
#end