library(ggplot2)
library(lubridate)
library(stringr)
library(tm)
library(plotly)
library(readr)
library(tidyverse)
library(ggplot2)
library(corrgram)
library(corrplot)
library(cluster) 
library(gridExtra)
library(grid)
library(NbClust)
library(factoextra)


data <- read_csv("C:/Users/Samiksha/Desktop/DATA SCIENCE INTERNSHIP/ecommerce-data/data.csv")
glimpse(data)


#Cleaning
data$Description[data$Description==""] <- NA
data$CustomerID[data$CustomerID==""] <- NA
colSums(is.na(data))
data <- na.omit(data)

data <- data %>% mutate(lineTotal = Quantity * UnitPrice)

data1 <- data[,c(4,6,7)]
head(data1)

k2<-kmeans(data1,3,iter.max=100,nstart=50,algorithm="Lloyd")
k2
k2$cluster
set.seed(1)
ggplot(data1, aes(x ='Quantity', y ='UnitPrice'))+ 
  geom_point(stat = "identity", aes(color = as.factor(k2$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3")) +
  ggtitle("Market Basket Analysis", subtitle = "Using K-means Clustering")














set.seed(123)
wss <- function(k) {
  kmeans(data[,6:7], k, nstart = 10 )$tot.withinss}


k.values <- 1:15


wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


k1<-kmeans(data[,6:7],3,iter.max=100,nstart=50,algorithm="Lloyd")
k1
k1$cluster


set.seed(1)
ggplot(data, aes(x ='UnitPrice', y ='CustomerID'))+ 
  geom_point(stat = "identity", aes(color = as.factor(k1$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3")) +
  ggtitle("Market Basket Analysis", subtitle = "Using K-means Clustering")







k3 <- kmeans(data1[,1:2],3,iter.max = 100,nstart = 50,algorithm = "Lloyd")
k3
set.seed(1)
ggplot(data, aes(x ='Quantity', y ='UnitPrice'))+ 
  geom_point(stat = "identity", aes(color = as.factor(k3$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3")) +
  ggtitle("Market Basket Analysis", subtitle = "Using K-means Clustering")


data2 <- data[,c(4,)]
k3
print(k1)




































k2<-kmeans(data[,6:7],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(data[,6:7],"euclidean")))

k3<-kmeans(data[,6:7],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(data[,6:7],"euclidean")))

k4<-kmeans(data[,6:7],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(data[,6:7],"euclidean")))

k5<-kmeans(data[,6:7],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(data[,6:7],"euclidean")))

k6<-kmeans(data[,6:7],6,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(data[,6:7],"euclidean")))

k7<-kmeans(data[,6:7],7,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(data[,6:7],"euclidean")))

k8<-kmeans(data[,6:7],8,iter.max=100,nstart=50,algorithm="Lloyd")
s8<-plot(silhouette(k8$cluster,dist(data[,6:7],"euclidean")))

k9<-kmeans(data[,6:7],9,iter.max=100,nstart=50,algorithm="Lloyd")
s9<-plot(silhouette(k9$cluster,dist(data[,6:7],"euclidean")))

k10<-kmeans(data[,6:7],10,iter.max=100,nstart=50,algorithm="Lloyd")
s10<-plot(silhouette(k10$cluster,dist(data[,6:7],"euclidean")))

#Finding the optimal number of clusters using the above method
fviz_nbclust(data[,6:7], kmeans, method = "silhouette")







length(unique(data$StockCode[data$Country=="United Kingdom"]))
length(unique(data$CustomerID[data$Country=="United Kingdom"]))
length(unique(data$InvoiceNo[data$Country=="United Kingdom"]))
data[data$Quantity > 80000 | data$Quantity< -80000,]
data[data$UnitPrice > 35000 | data$UnitPrice< 0,]
countrysplit <- as.data.frame(table(data$Country))
countrysplit


#Filtering The Data

#Standard Deviation
sd_quantity <- sqrt(var(data$Quantity))
mean_quantity <- mean(data$Quantity)
UL_quantity <- mean_quantity+3*sd_quantity
LL_quantity <- mean_quantity-3*sd_quantity

sd_UP <- sqrt(var(data$UnitPrice))
mean_UP <- mean(data$UnitPrice)
UL_UP <- mean_UP+3*sd_UP
LL_UP <- mean_UP-3*sd_UP


data_filt <- data[(data$Quantity>LL_quantity),]
data_filt <- data_filt[(data_filt$Quantity<UL_quantity),]
data_filt <- data_filt[(data_filt$UnitPrice<UL_UP),]
data_filt <- data_filt[(data_filt$UnitPrice>LL_UP),]
data_filt$Country1 <- ifelse(data_filt$Country=="United Kingdom", "UK", as.character(data_filt$Country))


dat_uk<- data_filt[data_filt$Country1=="UK",]
nrow(dat_uk)

datclus_uk <- aggregate(dat_uk[c(4,9,13,14)], by = list(dat_uk$CustomerID), sum)
up <- aggregate(dat_uk[c(6)], by = list(dat_uk$CustomerID), mean)
datclus_uk <- merge(datclus_uk, up, by = "Group.1")

