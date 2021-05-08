# Cluster Analysis
library(readxl)
mydata <- read_xlsx("D_Mart.xlsx")
str(mydata)
head(mydata)
pairs(mydata)

# Scatter plot 
plot(mydata$Fuel_Cost~ mydata$Sales, data = mydata, xlab = "Sales", ylab = "Fuel Cost")
with(mydata,text(mydata$Fuel_Cost ~ mydata$Sales, labels=mydata$Outlets,pos=4))

# Normalize 
z = mydata[,-c(1,1)]
means = apply(z,2,mean)
sds = apply(z,2,sd)
nor = scale(z,center=means,scale=sds)

##calculate distance matrix (default is Euclidean distance)
distance = dist(nor)

# Hierarchical agglomerative clustering using default complete linkage 
mydata.hclust = hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust,labels=mydata$Outlets,main='Default from hclust')
plot(mydata.hclust,hang=-1)

# Hierarchical agglomerative clustering using "average" linkage 
mydata.hclust<-hclust(distance,method="average")
plot(mydata.hclust,hang=-1)

# Cluster membership
member = cutree(mydata.hclust,3)
table(member)

#Characterizing clusters 
aggregate(nor,list(member),mean)
aggregate(mydata[,-c(1,1)],list(member),mean)

# Scree Plot
wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-means clustering
kc<-kmeans(nor,4)
kc

plot(mydata$Fuel_Cost~ mydata$Sales, data = mydata, xlab = "Sales", ylab = "Fuel Cost", 
     col = kc$cluster, pch = 19)
with(mydata,text(mydata$Fuel_Cost ~ mydata$Sales, labels=mydata$Outlets,pos=4))
