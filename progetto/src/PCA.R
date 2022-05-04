# PCA


# to install from git:
library(devtools)

#install_github("vqv/ggbiplot")
library(ggbiplot)


## Principal Component Analysis
# Principal Component Analysis (PCA) is a useful technique for exploratory data analysis, 
# allowing to better visualize the variation present in a dataset to see the overall "shape" of the data, 
# identifying which samples are similar to one another and which are very different. 
# It is particularly helpful in the case of "wide" datasets, where you have many variables for each sample.

# We simplify the dataset by turning original variables into a smaller number of "Principal Components".
# Principal Components are the underlying structure in the data. They are the directions where there is the most variance, 
# the directions where the data is most spread out. 
# This means that we try to find the straight line that best spreads the data out when it is projected along it. 
# This is the first principal component, the straight line that shows the most substantial variance in the data.

# Every eigenvector has a corresponding eigenvalue. 
# The number of eigenvalues and eigenvectors that exits is equal to the number of dimensions the data set has. 
# An eigenvector is a direction while an eigenvalue is a number telling you how much variance there is in the data in that direction. 
# The eigenvector with the highest eigenvalue is, therefore, the first principal component.



# Selecting only numeric values:
df.num.i <- sapply(dataframe, is.numeric)
dataframe.num <- dataframe[df.num.i]



# correlation matrix
corr <- round(cor(dataframe.num), 1)

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE,
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))+
  ggtitle("Correlation matrix for numeric values")


# loudness, energy and acousticness are highly correlated. PCA will for sure address this, reducing dimensions of the dataset

#### test
tmp_df <- read.csv(file = 'tmp/data_pca.csv')
df.pca <- prcomp(tmp_df, center = TRUE, scale. = TRUE)


df.pca <- prcomp(dataframe.num, center = TRUE, scale. = TRUE)
summary(df.pca)

library(factoextra)
fviz_eig(df.pca, addlabels = TRUE, ylim = c(0, 50))

# In order to achieve 90% of the variance we need to take the first eight principal components (from PC1 to PC8).

ggbiplot(df.pca)

ggbiplot(df.pca, ellipse=TRUE, groups=dataframe$music_genre, alpha = 0.2)



# We will not use PCA because it doesn't provide a considerable reduction of the dataset

# scaling dataset
dataframe.num <- scale(dataframe.num)

dataframe.num <- as.data.frame(dataframe.num)
summary(dataframe.num)

# Merging music_genre (labels) into dataframe.num
dataframe.num$music_genre <- dataframe$music_genre

# Splitting dataset into train and test set
library(caTools)

set.seed(42)
split = sample.split(dataframe.num, SplitRatio = 0.75)
train = subset(dataframe.num, split == TRUE)
test = subset(dataframe.num, split == FALSE)














