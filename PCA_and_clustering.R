#1a


library(tidyverse)
#reading data using read_csv
data <- read_csv('breast_cancer_data.csv')
data_subset <- select(data,-id,-diagnosis)

#calculating principle components
pca_final = prcomp(data_subset,scale. = T )
summary(pca_final)
print(pca_final)

library(ggfortify)
require(ggbiplot)

#screeplot for total data from dataset
eigen_Val <- 100*((pca_final$sd)^2)/(sum((pca_final$sd)^2))
cumsum(eigen_Val)
ggplot(NULL,aes(x=1:5,y=eigen_Val[1:5]))+
  geom_col() +
  ggtitle('Scree plot of 5 components')+
  xlab('principle components')+
  ylab('percentage variance')

#loading plot
loadings <- as.data.frame(pca_final$rotation[,1:5])
loadings$variables = row.names(loadings)
loadings <- gather(loadings, key='Component', value='Values', -variables)
ggplot(loadings, aes(x=variables,y=Values)) +
  geom_bar(stat='identity') +
  facet_grid(Component~.) +
  ggtitle('Loadings for PC1-PC5') +
  theme(axis.text.x = element_text(angle=45, hjust=1))

#biplot PC1 vs PC2
autoplot(pca_final,
        loadings= TRUE,
         loadings.label=TRUE,
         frame = TRUE, 
         frame.type = 'norm',
         loadings.colour = "black",
         loadings.label.colour="black",
         loadings.label.repel=TRUE,
         data = data,colour='diagnosis')+ggtitle("Biplot for PC1 and PC2")
#PC1 (44.27%) + PC2 (18.97%) gives 63.24% of variance

#biplot PC2 vs PC3
autoplot(pca_final,x=2,y=3,loadings= TRUE,
         loadings.label=TRUE,
         frame = TRUE, 
         frame.type = 'norm',
         loadings.colour = "black",
         loadings.label.colour="black",
         loadings.label.repel=TRUE,
         data = data,colour='diagnosis')+ggtitle("Biplot for PC2 and PC3")
#PC2 (18.97%) + PC3 (9.39%) gives 28.36% of variance


#1b

#For first 10 columns/mean only    
data_subset_mean <- data[,1:12]
cancer_data_mean <- select(data_subset_mean,-id,-diagnosis)

#calculating principle components for mean only
pca_final_mean <- prcomp(cancer_data_mean,scale. = T )
summary(pca_final_mean)
print(pca_final_mean)

#screeplot
eigenVal_mean <- 100*((pca_final_mean$sd)^2)/(sum((pca_final_mean$sd)^2))
cumsum(eigenVal_mean)
ggplot(NULL,aes(x=1:10,y=eigenVal_mean))+
  geom_col() +
  ggtitle('Scree plot of 10 components')+
  xlab('principle components')+
  ylab('percentage variance')

#loading plot
loadings <- as.data.frame(pca_final_mean$rotation)
loadings$variables = row.names(loadings)
loadings <- gather(loadings, key='Component', value='Values', -variables)
ggplot(loadings, aes(x=variables,y=Values)) +
  geom_bar(stat='identity') +
  facet_grid(Component~.) +
  ggtitle('Loadings for PC1-PC5') +
  theme(axis.text.x = element_text(angle=30, hjust=1))

#biplot PC1 vs PC2
autoplot(pca_final_mean,
         loadings= TRUE,
         loadings.label=TRUE,
         frame = TRUE, 
         frame.type = 'norm',
         loadings.colour = "black",
         loadings.label.colour="black",
         loadings.label.repel=TRUE,
         data = data_subset_mean,colour='diagnosis')+ggtitle("Only mean biplot for PC1 and PC2")
#PC1 (54.79%) + PC2 (25.19%) gives 79.98% of variance

#biplot PC2 vs PC3
autoplot(pca_final_mean,x=2,y=3,loadings= TRUE,
         loadings.label=TRUE,
         frame = TRUE, 
         frame.type = 'norm',
         loadings.colour = "black",
         loadings.label.colour="black",
         loadings.label.repel=TRUE,
         data = data_subset_mean,colour='diagnosis')+ggtitle("Only mean biplot for PC2 and PC3")
#PC2 (25.19%) + PC3 (8.81%) gives 34% of variance



#2a



library(cluster)
scaled_data_subset <- scale(data_subset)

#Clustering
data_euclidean <- dist(scaled_data_subset, method="euclidean")

euclidean_ward <- agnes(data_euclidean, method='ward')
plot(euclidean_ward, which.plots=2,
     main='Cluster by using (Euclidean/Ward)')
clusters = cutree(euclidean_ward, k=4)
rect.hclust(euclidean_ward, k=4, border=3)

euclidean_single <- agnes(data_euclidean, method='single')
plot(euclidean_single, which.plots=2,
     main='Cluster by using (Euclidean/Single)')
clusters = cutree(euclidean_single, k=4)
rect.hclust(euclidean_single, k=4, border=3)

dist_manhattan <- dist(scaled_data_subset, method = "manhattan")

manhattan_average <- agnes(dist_manhattan, method = "average")
plot(manhattan_average, which.plots=2,
     main='Cluster by using (manhattan/average)')
clusters = cutree(manhattan_average, k=4)
rect.hclust(manhattan_average, k=4, border=3)

manhattan_complete <- agnes(dist_manhattan, method = "complete")
plot(manhattan_complete, which.plots=2,
     main='Cluster by using (manhattan/complete)')
clusters = cutree(manhattan_complete, k=4)
rect.hclust(manhattan_complete, k=4, border=3)


#Clustering only 10 samples(mean only)
scaled_data_mean <- scale(cancer_data_mean)
data_euclidean <- dist(scaled_data_mean, method="euclidean")

euclidean_ward <- agnes(data_euclidean, method='ward')
plot(euclidean_ward, which.plots=2,
     main='Cluster by using (Euclidean/Ward)')
clusters = cutree(euclidean_ward, k=4)
rect.hclust(euclidean_ward, k=4, border=3)

euclidean_single <- agnes(data_euclidean, method='single')
plot(euclidean_single, which.plots=2,
     main='Cluster by using (Euclidean/Single)')
clusters = cutree(euclidean_single, k=4)
rect.hclust(euclidean_single, k=4, border=3)

dist_manhattan <- dist(scaled_data_mean, method = "manhattan")

manhattan_average <- agnes(dist_manhattan, method = "average")
plot(manhattan_average, which.plots=2,
     main='Cluster by using (manhattan/average)')
clusters = cutree(manhattan_average, k=4)
rect.hclust(manhattan_average, k=4, border=3)

manhattan_complete <- agnes(dist_manhattan, method = "complete")
plot(manhattan_complete, which.plots=2,
     main='Cluster by using (manhattan/complete)')
clusters = cutree(manhattan_complete, k=4)
rect.hclust(manhattan_complete, k=4, border=3)


