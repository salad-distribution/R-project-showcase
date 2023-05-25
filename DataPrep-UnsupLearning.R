### Preparing Data For Unsupervised Learning ###
# Data Source: Bennet and Mangasarian "Robust Linear Programming Discrimination of Two Linearly Inseparable Sets" THROUGH Data Camp
url <- "https://assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"

# Download the data
wisc.df <- read.csv(url)

# Convert features (columns 3-32) of the data to matrix
wisc.data <- as.matrix(wisc.df[3:32])

# Set row names of wisc.data
row.names(wisc.data) <- wisc.df$id

# Create diagnosis vector, "M" = 1, otherwise = 0
diagnosis <- as.numeric(wisc.df$diagnosis == 'M')

### Exploratory Data Analysis ###
#number of observations
nrow(wisc.data) 
    ##569 observations
#number of features suffixed with '_mean'
sum(endsWith(colnames(wisc.data), "_mean"), na.rm=TRUE) 
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

### Principal Component Analysis (PCA) of prepared data ###
wisc.pr <- prcomp(wisc.data,
                  scale = TRUE,
                  center = TRUE
                  )
#view SD, proportion of variance, and cumulative proportion of variance explained for each of the 30 principal components
summary(wisc.pr)

## Visualizations
# biplot visualization of first 2 principal components
biplot(wisc.pr) ## not super helpful ¯\_(ツ)_/¯ but we have it if we wanna look at it
# scatterplot of first and second principal components
plot(wisc.pr$x[, c(1,2)], col = (diagnosis + 1), xlab = "PC1", ylab = "PC2")
    ## great separation between the 2 subgroups
# scatterplot of first and third principal components
plot(wisc.pr$x[, c(1,3)], col = (diagnosis + 1), xlab = "PC1", ylab = "PC2")
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


### Results and Observations ###
    ## only 4 principal components are needed to explain 80% of the variance in the data
#components of the loading vectors for all features in dataset for first 3 principal components
wisc.pr$rotation[, 1:3]

## Exercises follow along with guided Data Camp Case Study Project ##
