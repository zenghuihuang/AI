seeds= read.csv('/Users/zenghuihuang/Documents/2324/CS3002AI/Lab/supervised/seeds_dataset_class.csv',sep=",")
scale(seeds)
seedactualClass=seeds[,1]
seedactualValues=seeds[,-1]


seeds_rand=seeds[sample(209,209),] 

seedclass = seeds_rand[,1]
seedvalues = seeds_rand[,-1]
  
#training set
seedclassTrain=seedclass[1:150]
seedvaluesTrain=seedvalues[1:150,]

#Set up testset
seedclassTest = seedclass[151:209]
seedvalueTest= seedvalues[151:209,]

#build the decision tree
install.packages("rpart")
library(rpart) 
fit <- rpart(seedclassTrain~., method="class", data=seedvaluesTrain) 
print(fit)
plot(fit, uniform=TRUE, main="Decision Tree for Seeds dataset")
text(fit, use.n=TRUE, all=TRUE, cex=.8) 

#test the classifier using the test dataset
treepred <-predict(fit, seedvalueTest, type = 'class') 

#get the accuracy of the test class
n = length(seedclassTest) #the number of test cases 
ncorrect = sum(treepred==seedclassTest) #the number of correctly predicted
accuracy=ncorrect/n 
print(accuracy) 

#view the results as a table
table_mat = table(seedclassTest, treepred)
print(table_mat)

plot(seeds$Area, seeds$GrooveLength,col = seeds$Class)

#knn
library(class)
knn3pred = knn(seedvaluesTrain, seedvalueTest, seedclassTrain, k=3)
n = length(seedclassTest) #the number of test cases
ncorrect = sum(knn3pred==seedclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)

x <- c(4,5,6,7)

for (val in x) {
  knn3pred = knn(seedvaluesTrain, seedvalueTest, seedclassTrain, k=val)
  n = length(seedclassTest) #the number of test cases
  ncorrect = sum(knn3pred==seedclassTest) #the number of correctly predicted
  accuracy=ncorrect/n
  print(accuracy)
}



