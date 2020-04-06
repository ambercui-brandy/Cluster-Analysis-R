ph0 <- read.csv(file.choose(), header=T)
ph <- ph0[,-c(1,2,12,13,14)]
str(ph)
# finding mean of each column and store all values in "means"
means <- apply(ph,2,mean)
sds <- apply(ph,2,sd)
# normalize
nor <- scale(ph,center=means,scale=sds)
nor
# Calculate distance matrix  method:default
distance = dist(nor)
print(distance, digits = 3)
# 1. Hierarchical agglomerative clustering using default method
ph.hclust = hclust(distance)
plot(ph.hclust,labels=ph0$Symbol, hang=-1, main='Default linkage: 4 clusters')
# 2. Hierarchical agglomerative clustering using "average" linkage 
ph.hclust2<-hclust(distance,method="average")
plot(ph.hclust2, hang=-1,,labels=ph0$Symbol, main='Average')
# 3. Hierarchical agglomerative clustering using "median" linkage 
ph.hclust3<-hclust(distance,method="median")
plot(ph.hclust3, hang=-1,,labels=ph0$Symbol, main='median')
# 4. Hierarchical agglomerative clustering using "single" linkage 
ph.hclust4<-hclust(distance,method="single")
plot(ph.hclust4, hang=-1,,labels=ph0$Symbol,main='single')

# default method is the best
# place rectangular boxes, cluster=4
rect.hclust(ph.hclust,k=4)

# Cluster membership
# dividing into 3 clusters
member = cutree(ph.hclust,6)
table(member)

# Characterizing clusters 
# eg: PE_Ratio and Net_Profit_Margin contribute most in the forming of clusters
aggregate(nor,list(member),mean)
# cluster 1 is characterized by medium PE Ratio and high Net Profit Margin

par(mfrow=c(1,1))
# Silhouette Plot
library(cluster) 
# Interpretation, outliners within group
plot(silhouette(cutree(ph.hclust,6), distance)) 

# Scree Plot: within cluster varibility
# each line between lines: how much varibility will be reduced if increase one cluster
set.seed(1234)
wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
# 4 clusters are better in this case
help(hclust)
# K-means clustering
# clusering vector: each company -> which cluster
set.seed(123)

# set how many clusters we want
kc4<-kmeans(nor,4)
kc4
kc3<-kmeans(nor,3)
kc3
kc5<-kmeans(nor,5)
kc5
kc6<-kmeans(nor,6)
kc6
# compare betweenss_totalss, the higher the better - decide for optimal cluster number
# 4 clusters is optimal choice

# Tree
mydata$cluster <- as.factor(kc$cluster)
str(mydata)
library(party)
tree <- ctree(cluster ~ . -Company,
              mydata,
              controls = ctree_control(mincriterion = 0.8, minsplit = 1))
plot(tree)
# change mincriterion and minsplit
