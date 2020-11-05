library(ggplot2)
library(FactoMineR) #Used for standard implementation of PCA
library(shiny)

#Reading the dataset
data_pca <- read.csv('iris_clean.csv')

#Principal Components function, parsing using variance for the principal components
pca_calculation <- function(pca_dataset, variance_explained=0.9,center=T,scale=T)
{
  
  #Creating  empty list to store the mean, standard deviation, eigen vectors, cumulative variance of each columns
  pca_components = list()
  
  #calculate the mean of each column and store that to our empty list
  pca_components[['mean']]=colMeans(pca_dataset)
  
  #compute the standard deviation of the dataset
  pca_components[['standard_deviation']]=apply(pca_dataset,2,sd)
  
  #The data is standardized and normalized to be in range of [0,1]
  stand_pca_data=sweep(pca_dataset,2,pca_components[['mean']])
  stand_pca_data=stand_pca_data%*%diag(1/pca_components[['standard_deviation']])
  
  #covariance matrix A*A^T
  #As the dataframe has m = 147 rows n = 4 columns  we take A^T*A the resultant matrix will be 147*147 so we take A*A^T which gives us 4*4 matrix  
  eigen_cov=eigen(crossprod(stand_pca_data,stand_pca_data))
  
  i <- 0
  
  for (var in eigen_cov$values){
    i <- i+1
  }
  
  print(i)
  
  #from the eigen values, the contribution of each component towards the variance and the cumulative variance 
  pca_components[['cumvar']] <- cumsum(eigen_cov[['values']])
  
  #each contribution of each feature to variance
  last <- length(pca_components[['cumvar']])

  for (i in (1:last)){
    if(i == 1){
      pca_components[['variance_contribution']]<- append(pca_components[['variance_contribution']],(pca_components[['cumvar']][1]*100/pca_components[['cumvar']][last])) 
    }
    if(i > 1){
      pca_components[['variance_contribution']]<- append(pca_components[['variance_contribution']],((pca_components[['cumvar']][i]-pca_components[['cumvar']][i-1])*100/pca_components[['cumvar']][last]))
    }
    i = as.integer(i) + 1
  }
  
  for (variance in pca_components[['cumvar']]){
    pca_components[['individual_variance_contribution']]<- append(pca_components[['individual_variance_contribution']],(variance*100/pca_components[['cumvar']][last])) 
  }
  
  
  
  pca_components[['reqcomponents']] =sum((pca_components[['cumvar']]/sum(eigen_cov[['values']]))<variance_explained)+1
  
  
  pca_components[['fin_transform']] =eigen_cov[['vectors']][,1:pca_components[['reqcomponents']]]
  
  print(pca_components[['fin_transform']])
  
  attr(pca_components, "class") <- "final_principal_components"
  
  return(pca_components)
}

#Returns the direction in which data has increasing variance i.e. the principal components data
predict.pca_calculation <- function(pca_components,pca_data,..)
{
  #data is standardized and normalized to get the data values within the range of [0,1]
  stand_pca_data=sweep(pca_data,2,pca_components[['mean']])
  stand_pca_data=stand_pca_data%*%diag(1/pca_components[['standard_deviation']])
  
  #data multiplied by the transform obtained from pca gives direction in which the data variance increases
  return(stand_pca_data%*%pca_components[['fin_transform']])
}


pca1=pca_calculation(as.matrix(data_pca[,1:4]),1,scale=TRUE,center = TRUE)

projected <- predict.pca_calculation(pca1,as.matrix(data_pca[,1:4]))

ggplot(data=data_pca)+geom_point(aes(x=-projected[,1],y=projected[,2],color = Species))



pca_stats= PCA(as.matrix(data_pca[,1:4]))
#print(pca_stats)
projected_stats=predict(pca_stats,as.matrix(data_pca[,1:4]))$coord[,1:2]
ggplot(data=data_pca)+geom_point(aes(x=projected_stats[,1],y=projected_stats[,2],color= Species))+xlab('PC1')+ylab('PC2')+ggtitle('data_pca dataset projected on the two mains PC (FactomineR)')
