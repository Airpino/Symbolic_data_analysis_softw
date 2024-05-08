# PCA of interval data

library(RSDA)
data<-RSDA::oils

mins<-RSDA::interval.min(data)
maxs<-RSDA::interval.max(data)
centers<-RSDA::interval.centers(data)
radii<-(maxs-mins)/2

#create correlation matrix
means<-colMeans(centers)
variances_inter<-colMeans(centers^2)-means^2
variances_intra<-colMeans(1/12*(maxs-mins)^2)
TOT_variances<-variances_inter+variances_intra

covariance_inter<- 0 #???
covariance_intra<-t(as.matrix(centers))%*%as.matrix(centers)- 8*(means)%*%t(means)

# DO PCA
# Compute rotation
# COmpute scores
