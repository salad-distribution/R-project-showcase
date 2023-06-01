### Preparing Data For Unsupervised Learning ###
################################################
# Data Source: Bennet and Mangasarian "Robust Linear Programming Discrimination of Two Linearly Inseparable Sets" THROUGH Data Camp
url <- "https://assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"

# Download the data
wisc.df <- read.csv(url)

# Convert features (columns 3-32) of the data to matrix
wisc.data <- as.matrix(wisc.df[3:32])

# Set row names of wisc.data
row.names(wisc.data) <- wisc.df$id

# Diagnosis vector, "M" = 1, otherwise = 0
diagnosis <- as.numeric(wisc.df$diagnosis == 'M')

#################################
### Exploratory Data Analysis ###
#################################
#number of observations
nrow(wisc.data) 
    ##569 observations

#number of features suffixed with '_mean'
colmnames <- colnames(wisc.data)
col.suffix <- endsWith(colmnames, "_mean")
sum(col.suffix, na.rm=TRUE) 
    ##10 variables

#number of observations w/ malignant diagnosis
sum(diagnosis) 
    ##212 observations w/ malignant diagnosis

## mean of features
colMeans(wisc.data) 
    ##means present suggest variance in data 
    ##means range from -.00379 to 881

## standard deviation (SD) of features
apply(wisc.data, 2, sd) 
    ##SDs present suggest variance in data 
    ##SDs range from -.00265 to 569

###########################################################
### Principal Component Analysis (PCA) of prepared data ###
###########################################################
wisc.pr <- prcomp(wisc.data,
                  scale = TRUE,
                  center = TRUE
                  )

#view SD, proportion of variance, and cumulative proportion of variance explained for each of the 30 principal components
summary(wisc.pr)

## PCA Visualizations
# biplot visualization of first 2 principal components
biplot(wisc.pr) ## not super helpful ¯\_(ツ)_/¯ but we have it if we wanna look at it

# scatterplot of first and second principal components
plot(wisc.pr$x[, c(1,2)], 
     col = (diagnosis + 1), 
     xlab = "PC1", 
     ylab = "PC2"
     )
    ## great separation between the 2 subgroups

# scatterplot of first and third principal components
plot(wisc.pr$x[, c(1,3)],
     col = (diagnosis + 1),
     xlab = "PC1",
     ylab = "PC2"
     )
    ## still good separation b/w 2 subgroups but not as clean as the plot of PC1 and PC2
    # PC2 explains more variance in the original data than PC3

# calculate variance of each component
pr.var <- wisc.pr$sdev^2
# variance explained by each principal component
pve <- pr.var/sum(pr.var)

#set up plotting grid (1x2)
par(mfrow = c(1,2))
# PLOT: variance explained by each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = "b")

# PLOT: cumulative proportion of variance explained by each principal component
plot(cumsum(pve), xlab = "Principlal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")
  #add lines at 80% (blue) and 90% (orange) of variance explained
  abline(h=0.8, col='blue')
  abline(h=0.9, col= "orange")

## PCA Results and Observations ##
    ## only 5 principal components are needed to explain at least 80% of the variance in the data
    ## 7 principal components are needed to explain at least 90% of the variance in the data
  #components of the loading vectors for all features in dataset for first 3 principal components
  wisc.pr$rotation[, 1:3] ##rotation component indicates directions of principal component vectors in terms of the original features

###############################
### Hierarchical Clustering ###
###############################
# scale data
data.scaled <- scale(wisc.data)

# calculate Euclidean distances
data.dist <- dist(data.scaled)

# create hierarchical clustering model using "complete" linkage method
wisc.hclust <- hclust(data.dist, method = "complete")

# view Cluster Dendrogram
plot(wisc.hclust)
    ## clustering model has 4 clusters at height=20

# cut tree to 4 clusters
wisc.hclust.clusters <- cutree(wisc.hclust, k=4) 
  #compare cluster membership to diagnosis
  hvDiag <- table(wisc.hclust.clusters, diagnosis)

##########################
### k-means Clustering ###
##########################
#using 2 clusters to represent the malignant/else diagnosis binary
wisc.data.scaled <- scale(wisc.data)
wisc.km <- kmeans(wisc.data.scaled, centers = 2, nstart = 20)
  #compare cluster membership to diagnosis
  kmvDiag <- table(wisc.km$cluster, diagnosis)

#################################
### Clustering on PCA results ###
#################################
# hierarchical clustering using complete linkage method on 
  #number of principal components needed to describe at least 90% of the variance in the original data (first 7 PCs)
  PC17 <- wisc.pr$x[, 1:7]  #selecting PC1-PC7
  PC17dist <- dist(PC17)    #distance calculated
  wisc.pr.hclust <- hclust(PC17dist, method = "complete")

# cut model into 4 clusters
wisc.pr.hclust.clusters <- cutree(wisc.pr.hclust, k=4)

############################################
### Results and Observations/Reflections ###
############################################
# Table to compare k-means clustering to actual diagnoses
  kmvDiag

# Table to compare hierarchical clustering on original data to actual diagnoses
  hvDiag

  ##clusters 1,2,4 from the hierarchical clustering model can be interpreted as a cluster 1 equivalent from the k-means clustering algorithm
    ##cluster 3 from the hierarchical clustering model can be interpreted as a k-means cluster 2 equivalent

# Table to compare hierarchical clustering on first 7 principal components and actual diagnoses
  table(wisc.pr.hclust.clusters, diagnosis)
  
  ##hierarchical clustering on the first 7 principal components resulted in many more observations being clustered into the second cluster
    ## more without malignant diagnosis but also more with malignant diagnosis
    ## (increased "True Negatives" as well as "False Positives")
    ## lower accuracy, lower precision than clustering algorithms without PCA

## Exercises follow along with guided Data Camp Case Study Project ##
