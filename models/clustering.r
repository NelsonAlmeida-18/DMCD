dataset = read.csv('Data/Datasets/all.csv', sep=",") 
library(tidyverse)  
library(factoextra)

dataset$romantic=factor(dataset$romantic, levels = c("no","yes"),labels = c(0,1))
dataset$internet=factor(dataset$internet, levels = c("no","yes"),labels = c(0,1))
dataset$higher=factor(dataset$higher, levels = c("no","yes"),labels = c(0,1))
dataset$nursery=factor(dataset$nursery, levels = c("no","yes"),labels = c(0,1))
dataset$activities=factor(dataset$activities, levels = c("no","yes"),labels = c(0,1))
dataset$paid=factor(dataset$paid, levels = c("no","yes"),labels = c(0,1))
dataset$famsup=factor(dataset$famsup, levels = c("no","yes"),labels = c(0,1))
dataset$schoolsup=factor(dataset$schoolsup, levels = c("no","yes"),labels = c(0,1))
dataset$guardian=factor(dataset$guardian, levels = c("mother","father", "other"),labels = c(1,2,3))
dataset$reason=factor(dataset$reason, levels = c("home","reputation", "course","other"),labels = c(1,2,3,4))
dataset$Fjob=factor(dataset$Fjob, levels = c("teacher","health", "services","at_home","other"),labels = c(1,2,3,4,5))
dataset$Mjob=factor(dataset$Mjob, levels = c("teacher","health", "services","at_home","other"),labels = c(1,2,3,4,5))
dataset$Pstatus=factor(dataset$Pstatus, levels = c("T","A"),labels = c(1,2))
dataset$famsize=factor(dataset$famsize, levels = c("LE3","GT3"),labels = c(1,2))
dataset$address=factor(dataset$address, levels = c("U","R"),labels = c(1,2))
dataset$sex=factor(dataset$sex, levels = c("M","F"),labels = c(1,2))

dataset=dataset[,c("age","failures")]

#Clustering

#Elbow Method
set.seed(1234)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('Ideal Number of Clusters'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = dataset, centers = 4) #Number os clusters
y_kmeans = kmeans$cluster

fviz_cluster(kmeans, data = dataset, main = paste('Clusters Chumbos e Idades'),)

