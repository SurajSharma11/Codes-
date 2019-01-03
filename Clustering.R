rm(list=ls(all=TRUE))

#Consider mtacrs data of R-datasets
data(mtcars)
str(mtcars)
mydata <- data.frame(mtcars[,1:7])
sum(is.na(mydata))
summary(mydata)
str(mydata)

mydata <- scale(mydata) # standardize variables 

# simple distance calculation between HOnda Civic and Camaro Z28
x <-mydata["Honda Civic",] 
y <- mydata["Camaro Z28",] 
dist(rbind(x, y)) 

# # distance between Camaroz28 and Firebird
# z <- mydata["Pontiac Firebird",] 
# dist(rbind(y, z))
# summary(mydata)

###-------------------------    Hierarchical Clustering     ------------------------###
# Ward's method 
# distance matrix euclidean
d <- dist(mydata,method = "euclidean") 

#View(data.matrix(d))
fit <- hclust(d, method="ward.D") #WARD is a min variance method to find compact clusters

plot(fit) # display dendogram
fit$merge
fit$dist.method

groups <- cutree(fit, k=5) # cut tree into 5 clusters
groups

#groups <- cutree(fit, k=2) # cut tree into 2 clusters
#groups

# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red") 
mydata_clusters=data.frame(mydata,groups)
par(mfrow=c(2,2))
fit1 <-hclust(d, method="complete")
fit2 <- hclust(d, method="single")
fit3 <- hclust(d, method='average')
fit4 <- hclust(d, method='ward.D')

############
dev.off()
par(mfrow = c(2, 2))
plot(fit3, leaflab = "textlike", main = "Average", xlab = "")
plot(fit4, leaflab = "textlike", main = "Ward", xlab = "")
plot(fit2, leaflab = "textlike", main = "Single", xlab = "")
plot(fit1, leaflab = "textlike", main = "Complete", xlab = "")

### Finding Optimum number of clusters
# install.packages("factoextra")
library(factoextra)
fviz_nbclust(mydata, hcut, method = "wss")

###-------------------------    K- means Clustering     ------------------------###
# K-Means Cluster Analysis with k = 5
set.seed(123)
fit <- kmeans(mydata, 5) # 5 cluster solution
fit$withinss
fit$betweenss
#study the mdoel
fit$cluster
fit$tot.withinss
fit
fit$centers
#or # get cluster means
#aggregate(mydata,by=list(fit$cluster),FUN=mean)
#dev.off()

#install.packages("factoextra")
library(factoextra)
fviz_cluster(fit, mydata)

# append cluster label to the actual data frame
mydata <- data.frame(mydata, fit$cluster)
#write.csv(mydata,"kmeans_2.csv")
head(mydata)

### Finding Optimum number of clusters
# K-means:  Determine number of clusters by considering the withinness measure
wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
}

#the scree plot
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 
#Choose that k, where the within groups sum of sqaures converges 

#Using factoextra library
factoextra::fviz_nbclust(mydata[,-c(8)], kmeans, method = "wss")
#k=6!?
set.seed(123)
final_fit_kmeans <- kmeans(mydata, 6) # 5 cluster solution

###-------------------------  on unseen data   ------------------------###
# For unseen data, we compute its distance from all the cluster centroids
# and assigns it to that cluster that is nearest to it

test_datapoint <- mtcars[sample(1:nrow(mtcars),1),]
closest.cluster <- function(x) {
  cluster.dist <- apply(fit$centers, 1, function(y) sqrt(sum((x-y)^2)))
  print(cluster.dist)
  return(which.min(cluster.dist)[1])
}

closest.cluster(test_datapoint)

###-------------------------  Quality check   ------------------------###
library(cluster)
#gower_dist = daisy(x = mydata, metric = "gower")
distance_matrix = daisy(x = mydata, metric = "euclidean")
clust_assignment = mydata$fit.cluster
sil_value_hc_mixed = silhouette(clust_assignment,
                                dist = distance_matrix)
plot(sil_value_hc_mixed)


###-------------------------  stability check   ------------------------###
#stabilitycheck
set.seed(123)
index <- (sample(nrow(mydata),.70*nrow(mydata)))
data <- mydata[index,]
fit2 <- kmeans(data,5)
data$clusters <- fit2$cluster

group1 <- mydata$fit.cluster[index]
group2 <- data$clusters

#loop dis for n imes. 
#install.packages("fossil")
library(fossil)
stabilitycheck <- adj.rand.index(group1, group2)
stabilitycheck
#across samples: avg_stabilitycheck
#Index value between 0 and 1, where 1 means the two clustering outcomes match identically.


# install.packages("clusteval")
library(clusteval)
Stabindex <- cluster_similarity(group1, group2, similarity = "jaccard", method="independence")
Stabindex


###-------------------------    Gower's distance     ------------------------###
# How to deal with mixture of attribute types
mydata2<-mtcars
names(mydata2)
#categorical data 
data1<-as.data.frame(apply(mydata2[,c(2,8,9,10)],2,as.factor))
#Numeric data - standardization
data2<-scale(mydata2[,-c(2,8,9,10)],scale=T,center = T)
#data_allnum<-scale(mydata2,scale=T,center=T)

# combine
dat_gower_numcat<-cbind(data2,data1)
# distance using gower measure
distMat <- daisy(dat_gower_numcat, metric = "gower")
# hierarchical clustering
fitGower<-hclust(distMat,method="single")
dev.off()
plot(fitGower)
# naming the clusters - refer to the activity sheet.


###-------------------------  Kernel clustering   ------------------------###
###Kernel clusering
rm(x)
group.one = cbind(runif(100,0,15),  runif(100,0,1))
group.two = cbind(rnorm(100,15,1),  rnorm(100,5,1))
uniform.pts = runif(100,4,15)
group.three = cbind(uniform.pts+
                      rnorm(100,0,.5),
                    uniform.pts+
                      rnorm(100,0,.5))
x = rbind(group.one,group.two,group.three)
plot(x, xlab="",ylab="")

km = kmeans(x, centers=3,
            iter.max=20)

plot(x,xlab="",ylab="",
     col=c("red","black","blue")[km$cluster],
     main="k-means")

require(kernlab)
specclu = specc(x, kernel = "laplacedot",centers=3)
plot(x, col=specclu)

library(kernlab)
specclu = specc(x, centers=2)
plot(x, col=specclu)

