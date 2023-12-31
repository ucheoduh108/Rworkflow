#APTECH CLASSIFICATION GERMAN-CREDIT DATA

install.packages("textir")
library(textir) ## needed to standardize the data
install.packages("class")
library(class) ## needed for knn(k-nearest neighbor)
library(e1071)
library(plyr)
library(caret)
library(mlbench)
library(rpart)
library(arules)

credit <- read.csv("C:/Users/user/Documents/germancredit.csv")
head(credit)
str(credit)
credit$Default <- factor(credit$Default)
str(credit)

#Re-level the credit history and a few other variables
credit$history <- factor(credit$history, levels = c('A30','A31','A32','A33','A34'))
levels(credit$history) <- c('good','good','poor','poor','terrible')

credit$foreign <- factor(credit$foreign, levels = c('A201','A202'), 
                              labels = c('foreign','german'))

credit$rent <- factor(credit$housing== 'A151')

credit$purpose <- factor(credit$purpose, levels = c('A40','A41','A42','A43','A44','A45',
                                      'A46','A47','A48','A49','A410'))
levels(credit$purpose) <- c('newcar','usedcar',rep('goods/repairs',4),
                            'edu','NA','edu','biz','biz')

## for demonstration, cut the dataset to these variables
credit <- credit [,c('Default','duration','amount','installment','age','history','purpose',
                     'foreign','rent')]

credit[1:3,]
summary(credit) #check out the data

## for illustration we consider just 3 loan characteristics:
## amount,duration,installment
## Standardization of the data is preferable, especially if
## units of the features are quite different
## We use the normalize or scale function in the R-package textir;
## it converts data frame columns to mean-zero sd-one

credit_scale <- scale(credit[,c(2,3,4)])
credit_scale[1:3,]

## training and prediction datasets
## training set of 900 borrowers; want to classify 100 new ones 

set.seed(1)
train <- sample(1:1000,900) #this is the training set of 900 borrowers

credit_scaleTrain <- credit_scale[train,]
credit_scaleTest <- credit_scale[-train,]

credit_defaultTrain <- credit$Default[train]
credit_defaultTest <- credit$Default[-train]

## k-nearest neighbor method
library(class)

nearest1 <- knn(train = credit_scaleTrain, test = credit_scaleTest,
                cl=credit_defaultTrain, k=1)

nearest3 <-  knn(train = credit_scaleTrain, test = credit_scaleTest,
                 cl=credit_defaultTrain, k=3)

data.frame(credit_defaultTest, nearest1, nearest3)[1:10,]

#cl=classification (of the y value for the training set )

# calculate the proportion of correct classifications
pcorrn1 = 100*sum(credit_defaultTest==nearest1)/100
pcorrn3 = 100*sum(credit_defaultTest==nearest3)/100
pcorrn1
pcorrn3

#plot for 3nn
plot(credit_scaleTrain[,c("amount","duration")],col=c(4,3,6,2)[credit[train,"installment
"]],pch=c(1,2)[as.numeric(credit_defaultTrain)],main="Predicted default, by 3 nearest
neighbors",cex.main=.95)

points(credit_scaleTest[,c("amount","duration")],bg=c(4,3,6,2)[credit[train,"installment"
]],pch=c(21,24)[as.numeric(nearest3)],cex=1.2,col=grey(.7))

legend("bottomright",pch=c(1,16,2,17),bg=c(1,1,1,1),legend=c("data 0","pred
0","data 1","pred 1"),title="default",bty="n",cex=.8)

legend("topleft",fill=c(4,3,6,2),legend=c(1,2,3,4),title="installment
%",horiz=TRUE,bty="n",col=grey(.7),cex=.8)

## above was for just one training set
## cross-validation (leave one out)
pcorr =dim(10)

for (k in 1:10) {
  pred=knn.cv(credit_scale,cl=credit$Default,k)
  pcorr[k]=100*sum(credit$Default==pred)/1000
}
pcorr

