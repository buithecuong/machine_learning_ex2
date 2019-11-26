'Install neccessary packages and functions'
install.packages("tidyverse") # data manipulation
library("tidyverse")  
install.packages("cluster") # clustering algorithms
library("cluster")    
install.packages("factoextra") # clustering algorithms & visualization
library("factoextra")) 
install.packages("NbClust")
library("NbClust")
install.packages("flexclust")
library("flexclust")


'DATA PREPARATION'
'Parsing wine dataset containing 13 chemical measurements on 178 Italian wine samples is analyzed.
The data originally come from the UCI Machine Learning Repository'
	
wine <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")
names(wine) <- c("Type", "Alcohol","MalicAcid","Ash","Alcalinity","Magnesium",
"Totalphenols","Flavanoids","Nonflavanoid","Proanthocyanins",
"Color","Hue","dilution","Proline")
head(wine)

'NA or Missing data processing'
'Number of missing data before processing'
no_na<-sum(is.na(df))
if (no_na >0){
	df<-na.omit(df)
	'Number of missing data after processing'
	no_na<-sum(is.na(df))
} else {
'No missing data from dataset'
}	

'Standardize data by scale wine data exclude type'
'Checking Mean and SD of data before standardization'
sapply(df,mean)
sapply(df,sd)

'Checking Mean and SD of data after standardization'
df<-as.data.frame(scale(wine[-1]))
head(df)
sapply(df,mean)
sapply(df,sd)

distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


'K-MEANS CLUSTERING ALGORITHMS'
# Computing k-means number = 2 
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k2
# fviz_cluster performs principal component analysis (PCA) if variables > 2
fviz_cluster(k2, data = df)

#Computing k-means number =9 clustering in R
k9 <- kmeans(df, centers = 9, nstart = 25)
str(k9)
k9
fviz_cluster(k9, data = df)

'Display graph how evaluation of different numbers of cluster'
'Define wssplot graph funtion to show different number of cluster
to suggest the best number of cluster'
wssplot <- function(data, nc=15, seed=1234){
               wss <- (nrow(data)-1)*sum(apply(data,2,var))
               for (i in 2:nc){
                    set.seed(seed)
                    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
                plot(1:nc, wss, type="b", xlab="Number of Clusters",
                     ylab="Within groups sum of squares")}
wssplot(df)


'Evaluate the best number of clusters'

'Elbow method '
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")	  

'Silhouette method'
set.seed(123)
fviz_nbclust(df, kmeans, method = "silhouette")

'gap statistic'
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

'NbClust package, published by Charrad et al., 2014'

set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
          xlab="Numer of Clusters", ylab="Number of Criteria",
          main="Number of Clusters Chosen by 26 Criteria")


'IMPLEMENT RESULTS'
no_clusters <- readline(prompt="Enter the best number of cluster from the graph")
set.seed(1234)
fit.km <- kmeans(df, as.integer(no_clusters), nstart=25)                           
fit.km$size
fit.km$centers
str(fit.km)

fviz_cluster(fit.km, data = df)

'Create agregated table with mean of k-mean'
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

#Perform cross-validation of Type (wine varietal) and cluster membership is given by
cv.km <- table(wine$Type, fit.km$cluster)
cv.km

#Evaluation (0-1) for satisfastion between type and cluster using an adjusted Rank index provided 
randIndex(cv.km)

  