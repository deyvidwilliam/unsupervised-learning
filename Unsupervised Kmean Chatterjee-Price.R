# Unsupervised machine learning using K-Means clustering
# The dataset Chatterjee-Price Attitude Dat is a survey of clerical employees of a large financial organization

# Load necessary libraries
library(datasets)

# Inspect data structure
str(attitude)

# Summarise data
# The dataset gives the percent of favourable responses to seven questions in each department 
summary(attitude)

# We re going to cluster the attitude dataset with the responses from all departments when
# it comes to complaints and privileges and we would like to understand whether there
# are shared characteristics among certain departments when it comes to these two variables.

# Subset the attitude data (complaints and privileges)
dat = attitude[,c(2,3)]

# Plot subset data
plot(dat, main = "Percent of favourable responses to \n Complaints x Privilege", pch =20, cex =2)

# Applying K-Means clustering to try to assign each department to a specific number of clusters that are similar

# Perform K-Means with 2 clusters
set.seed(7)
km1 = kmeans(dat, 3, nstart=100)

# Plot results
plot(dat, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

# Elbow method to examine the dissimilarity within the cluster and identifiy the optimal number of clusters 
mydata <- dat
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# We notice that after 5 clusters the observed difference in the within-cluster dissimilarity is not substantial. 
# Run K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
km2 = kmeans(dat, 5, nstart=100)

# Examine the result of the clustering algorithm
km2

# Plot results
plot(dat, col =(km2$cluster +1) , main="K-Means result with 5 clusters", pch=20, cex=2)
