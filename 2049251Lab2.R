mydata=read.csv('/Users/zenghuihuang/Documents/2324/CS3002AI/Lab/2/seeds_dataset.csv',sep=",")
plot(mydata)
mydata = na.omit(mydata)
mydata_unscaled <- mydata
mydata = scale(mydata)

source("/Users/zenghuihuang/Documents/2324/CS3002AI/Lab/2/WK_R.r")
seeds_real=read.csv("/Users/zenghuihuang/Documents/2324/CS3002AI/Lab/2/seeds_real.csv")


d <- dist(mydata, method = "euclidean") 

# calculate Hierarchical clustering with average linkage
fit <- hclust(d, method="average")
plot(fit)
Hgroups <- cutree(fit, k=3) 
rect.hclust(fit, k=3, border="red") 
plot(mydata, col=Hgroups)
wk_average = WK_R(Hgroups, seeds_real$Real)

# calculate Hierarchical clustering with complete linkage
fit <- hclust(d, method="complete")
plot(fit)
Hgroups <- cutree(fit, k=3) 
rect.hclust(fit, k=3, border="red") 
plot(mydata, col=Hgroups)
wk_complete = WK_R(Hgroups, seeds_real$Real)
# calculate Hierarchical clustering with single linkage
fit <- hclust(d, method="single")
plot(fit)
Hgroups <- cutree(fit, k=3) 
rect.hclust(fit, k=3, border="red") 
plot(mydata, col=Hgroups)
wk_single = WK_R(Hgroups, seeds_real$Real)
 # calculate k-means clustering
 fit_K<- kmeans(mydata, 5) # 5 cluster solution
 aggregate(mydata,by=list(fit_K$cluster),FUN=mean)
 Kgroups = fit_K$cluster
 plot(mydata, col=Kgroups).  # plot(mydata[4:5], col=Kgroups)
 wk5 = WK_R(Kgroups, seeds_real$Real)
 


 
 # calculate k-means clustering with k=4
 fit_K<- kmeans(mydata, 4) # 5 cluster solution
 aggregate(mydata,by=list(fit_K$cluster),FUN=mean)
 Kgroups = fit_K$cluster
 plot(mydata, col=Kgroups)
 wk4 = WK_R(Kgroups, seeds_real$Real)
 
 # calculate k-means clustering with k=3
 fit_K<- kmeans(mydata, 3) # 5 cluster solution
 aggregate(mydata,by=list(fit_K$cluster),FUN=mean)
 Kgroups = fit_K$cluster
 plot(mydata, col=Kgroups)
 wk3 = WK_R(Kgroups, seeds_real$Real)

 # calculate k-means clustering with k=2
 fit_K<- kmeans(mydata, 2) # 5 cluster solution
 aggregate(mydata,by=list(fit_K$cluster),FUN=mean)
 Kgroups = fit_K$cluster
 plot(mydata, col=Kgroups)
 wk2 = WK_R(Kgroups, seeds_real$Real)
 
 
 
 w_concatenated= c(wk_single,wk_average,wk_complete, wk2,wk3,wk4,wk5)
 plot(w_concatenated)

 
 
 