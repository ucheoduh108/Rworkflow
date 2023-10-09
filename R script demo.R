x <- array (1:24,dim=c(3,4,2));x
x[3,,2]
x[3,2,,1]
a<- matrix(1:16,nrow=4,ncol=4); a
a[2,3]

b <- factor(c('male','female','male','female'));b
b[3]

name <- c('Uche','Osone')
age <- c(31,30)
sex <- c('M','F')
df <- data.frame(name,age,sex)
df$name

s <- list('Ria','F',22,0.5);s
s[2:3]

setwd("C:/Users/user/Documents")
getwd()
AssociationRules <- read_excel("C:/Users/user/Desktop/DMX/Datasets/Additional Datasets/Association Rules Ex 2.xlsx")
AssociationRules

str(AssociationRules)

View(AssociationRules)

category <- read.csv("C:/Users/user/Desktop/DMX/Datasets/Additional Datasets/Classification Ex 1.csv")
view(category)                
str(category)               
View(category)
category <- read.csv("C:/Users/user/Desktop/DMX/Datasets/Additional Datasets/Classification Ex 1.csv",stringsAsFactors = FALSE)



library(gdata)               
mydata <- read.xls('mydata.xls')                 

m <- matrix(c(1,2,3,4),2,2);m                
apply(m,1,sum)                
apply(m,2,sum)

list <- list (a=c(1,1), b=c(2,2), c=c(3,3) )
lapply(list,sum)
lapply(list,mean)

mtcars
select(mtcars,mpg,disp)
select(mtcars,mpg:hp)
select(iris,starts_with ('petal'))
select(iris,ends_with('width'))
select(iris,contains('etal'))
select(iris,matches('.t.'))


filter(mtcars,cyl==8)
filter(mtcars,cyl<6)
filter(mtcars,cyl<6 & vs<1)
filter(mtcars,cyl<6 | vs==1)

arrange(mtcars, desc(disp))
arrange(mtcars,cyl,disp,hp)

mutate(mtcars, my_custom_disp = disp/1.0237)

summarise(group_by(mtcars,cyl),mean(disp))
summarise(group_by(mtcars,cyl),m = mean(disp),sd = sd(disp))
summarise(mtcars,first(disp))
summarise(mtcars,last(qsec))
summarise(mtcars,nth(disp,5))
summarise(mtcars,n())
summarise(mtcars,n_distinct(qsec))
summarise(mtcars,length(mpg))
summarise(mtcars,length(mpg))
summarise(mtcars,IQR(disp))
summarise(mtcars,var(mpg))
summarise(mtcars,mode(disp))
summarise(mtcars,median(wt))

install.packages('plyr')
library(plyr)

mtcars <- rename(mtcars, c('disp'='disp7'))
View(mtcars)
view(cars)
mtcars
max(mtcars$disp)
min(mtcars$wt)
mtcarsCylTransform <- transform(mtcars,cyltrans=ifelse(cyl<6,'weak','strong'))
mtcars
table(mtcarsCylTransform$cyltrans,mtcarsCylTransform$poutcome)

#AESTHETICS
Generation <- c('Z','Y','X','Baby Boomers')
Year <- c('Born 1996 and later','Born 1977 to 1995',
          'Born 1965 to 1976','Born 1946 to 1964')
BankCustomer<-data.frame(Generation,Year)
BankCustomer



install.packages("tidyverse")
library('tidyverse')
install.packages(c('Lahman','nycfights13','gapminder'))
2+3

#VISUALIZATION

#ggplot
View(mpg)
mpg
ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy))

ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy,color=class))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy,size=class))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy,alpha=class))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy,shape=class))

ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy),color='blue')
ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy),size=5)
ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy),shape=21)

ggplot(data=mtcars)+ geom_point(mapping=aes(x=mpg,y=disp))

ggplot(data=mpg) + geom_point(mapping=aes(x=class,y=drv))

ggplot(data=mpg) + geom_density(mapping=aes(x=displ))

ggplot(data=airquality) + geom_histogram(mapping=aes(x=Ozone),
 color='black',fill='blue')

#FACETS
ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy)) +
  facet_wrap(~class,nrow=2)
#OR
ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy))+
  facet_grid(.~cyl)

#OR facet_grid(cyl~.)

ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy)) +
  facet_grid(drv~cyl)

#GEOMETRIC OBJECTS
ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy))
ggplot(data=mpg) + geom_smooth(mapping=aes(x=displ,y=hwy))
ggplot(data=mpg) + geom_smooth(mapping=aes(x=displ,y=hwy,linetype=drv))
ggplot(data=mpg) + geom_smooth(mapping=aes(x=displ,y=hwy,group=drv))
ggplot(data=mpg) + geom_smooth(mapping=aes(x=displ,y=hwy,color=drv),
                               show.legend=FALSE)

#MULTIPLE GEOM
ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy)) +
  geom_smooth(mapping=aes(x=displ,y=hwy))
#OR
ggplot(data=mpg, mapping=aes(x=displ,y=hwy)) + 
  geom_point()+geom_smooth()

ggplot(data=mpg, mapping=aes(x=displ,y=hwy))+
  geom_point(mapping=aes(color=class)) + geom_smooth()

ggplot(data=mpg, mapping=aes(x=displ,y=hwy)) +
  geom_point(mapping=aes(color=class)) + 
  geom_smooth(data = filter (mpg, class == "subcompact"),se=FALSE)

ggplot(data=mpg,mapping=aes(x=displ,y=hwy)) +
  geom_point() + geom_smooth(se=FALSE)

ggplot() + geom_point(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_smooth(data=mpg, mapping=aes(x=displ, y=hwy))

ggplot() +
  geom_point(data=mpg,mapping=aes(x=displ,y=hwy,color=drv,
                                  size=drv,shape=drv,alpha=drv),size=5) + 
  geom_smooth(data=mpg,mapping=aes(x=displ,y=hwy,
                                   color=drv,linetype=drv))
#STATISTICAL TRANSFORMATION
diamonds

ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut))
#OR
ggplot(data=diamonds) + stat_count(mapping=aes(x=cut))
?stat_bin
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, y= ..prop..,
                                           group=1 ))
ggplot(data=diamonds) + stat_summary(mapping=aes(x=cut,y=depth),
  fun.ymin = min,
  fun.ymax = max,
  fun.y = median)
?geom_col()
ggplot(data=diamonds) + geom_col(mapping=aes(x=cut,y=depth))
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, fill=color,
                                             y = ..prop..))
#POSITION ARGUMENTS
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut,fill=cut))
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut,color=cut)) 
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, fill=clarity))
#or
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut,fill=clarity),
                                 position='dodge')
#COORDINATE SYSTEMS
nz <- map_data('nz')
ggplot(nz,aes(long,lat,group=group)) + geom_polygon(fill='white',
                  color='black') + coord_quickmap()
mtcars
ggplot(data=mtcars) + geom_bar(mapping=aes(x=gear))

######
counts<-table(mtcars$gear)
barplot(counts,horiz=TRUE)

counts <- table(mtcars$gear)
barplot(counts,
main = 'Simple Bar Plot',
xlab = 'Improvement',
ylab = 'Frequency',
legend = rownames(counts),
col = c('red','yellow','green')) 

counts <- table(mtcars$vs,mtcars$gear)
barplot(counts,
main= 'Car Distribution by Gears and VS',
xlab= 'Nmuber of Gears',
col= c('grey','cornflowerblue'),
legend= rownames(counts),beside=TRUE)

#PIE CHART
slices <- c(10,12,4,16,8)
lbls <- c('US','UK','Australia','Germany','France')
pie(slices, labels=lbls, main='Simple Pie Chart')

slices <- c(10, 12, 4, 16, 8)
pct <- round(slices/sum(slices)*100)
lbls <- paste(c('US','UK','Australia','Germany','France'),
                ' ',pct,'%',sep = '')
pie(slices, labels=lbls, col=rainbow(5), main='Pie Chart With Percentages') 

#3 dimensional pie chart
library(plotrix)
slices <- c(10, 12, 4, 16, 8)
pct <- round(slices/sum(slices)*100)
lbls <- paste(c('US','UK','Australia','Germany','France'),
              '',pct,'%',sep='')
pie3D(slices, labels=lbls, explode=0.0, main='3D Pie Chart')

#Histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks=3, col='darkgreen')
#breaks=controls the number of bins.

#kernel density plots
density_data<-density(mtcars$mpg)
plot(density_data)

density_data<-density(mtcars$mpg)
plot(density_data,main='Kernel Density of Miles Per Gallon')
polygon(density_data,col='skyblue',border='black')

#Line chart
weight<- c(2.5, 2.8, 3.2, 4.8, 5.1, 5.9, 6.8, 7.1, 7.8, 8.1)
months<- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
plot(months,weight, type='b', main= 'Baby Weight Chart')

weight<- c(2.5, 2.8, 3.2, 4.8, 5.1, 5.9, 6.8, 7.1, 7.8, 8.1)
months<- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
plot(months,weight, type='b', main= 'Baby Weight Chart',col='Red')

#Boxplot
boxplot(airquality$Ozone, main='Mean Ozone in parts per billion at 
        Roosevelt Island', xlab ='Parts Per Billion', ylab='Ozone',
        horizontal = TRUE,col='green')
airquality

#wordcloud
data <- read.csv('TEXT.csv', header = TRUE)
wordcloud (words=data$word,
min.freq=2, max.words = 100, random.order = FALSE)

#File formats of graphics
jpeg('myplot.jpeg')
counts <- table(mtcars$gear)
barplot(counts)
dev.off()

pdf('myplot.pdf')
counts <- table(mtcars$gear)
barplot(counts,
xlab = 'Improvement',
ylab = 'Frequency',
main = 'Simple Bar Plot',
legend = rownames(counts),
col = c('red','green','yellow'))
dev.off()



bmp('myplot.bmp') 
counts <- table(mtcars$gear)
barplot(counts,
        xlab = 'Improvement',
        ylab = 'Frequency',
        main = 'Simple Bar Plot',
        legend = rownames(counts),
        col = c('red','green','yellow'))
dev.off()

library(help = 'stats')

#Parametric test(z-test)
pnorm(84, mean=72, sd=15.2, lower.tail = FALSE)

#t-test
qt(c(0.025,0.975),df=5)

#ANOVA


Item1 <- c(22, 42, 44, 52, 45, 37)
Item2 <- c(52, 33, 8, 47, 43, 32)
Item3 <- c(16, 24, 19, 18, 34, 39)
Menu_sales <- data.frame(Item1, Item2, Item3); Menu_sales

r <- c(t(as.matrix(Menu_sales))); r

f <- c('Item1', 'Item2', 'Item3')
k = 3
n = 6

tm <- gl(k, 1, n*k, factor(f)); tm

av <- aov(r ~ tm)
summary(av)

#TWO WAY ANOVA

ToothGrowth
head(ToothGrowth)
ToothGrowth$dose <- factor(ToothGrowth$dose,
      levels = c(0.5,1,2), labels = c('D0.5', 'D1', 'D2'))
res <- aov(len ~ supp + dose, data = ToothGrowth)
summary(res)


#How to Interpret P-values and Coefficients in Regression Analysis
#By Jim Frost 250 Comments

#if the p-value for a variable is less than your significance level, 
#your sample data provide enough evidence to reject the null hypothesis for the 
#entire population. Your data favor the hypothesis that there is a non-zero correlation. 
#Changes in the independent variable are associated with changes in the dependent variable 
#at the population level. This variable is statistically significant and probably a 
#worthwhile addition to your regression model.

#On the other hand, when a p value in regression is greater than the significance level, 
#it indicates there is insufficient evidence in your sample to conclude that 
#a non-zero correlation exists.


#CHI-TEST
library(MASS)
head(survey)
tbl = table(survey$Smoke, survey$Exer); tbl
chisq.test(tbl)

#ASSIGNMENT - 2 WAY ANOVA
CO2
head(CO2)
CO2$Treatment <- factor(CO2$Treatment, levels = c('nonchilled','chilled'))
levels = c('0','1')
head(CO2)
gas <- aov(uptake ~ conc + Treatment, data = CO2)
summary(gas)

#LInear regression analysis

#STUDENT DATA
getwd()

std_doc <- read.csv ("C:/Users/user/Documents/Demo 1_Perform simple linear regression.csv")
View(std_doc)
str(std_doc)
summary(std_doc)

getwd()
results <- lm(formula = Height ~ Weight, data = std_doc)
results
#oR 
results1 <- lm(formula = std_doc$Weight ~ std_doc$Height, data = std_doc)
results1
summary(results)

#CARS
setwd("C:/Users/user/Documents")
cars_data <- read.csv("C:/Users/user/Documents/Demo 2_ Perform Regression Analysis with multiple variables.csv")
View(cars_data)
summary(cars_data)
str(cars_data)
cars_results <- lm(formula = MPG_City ~ Type + Origin + DriveTrain +
      EngineSize + Cylinders + Horsepower + Weight + 
        Wheelbase + Length, data = cars_data)
cars_results
summary(cars_results)

#Boston
Boston
View(Boston)
summary(Boston)
str(Boston)
Boston_results <- lm(formula = medv ~ ., data = Boston)
Boston_results
summary(Boston_results)

# York_data
york_data <- read.csv("C:/Users/user/Documents/Regression Analysis Ex 1.csv")
View(york_data)
summary(york_data)
str(york_data)
york_result <- lm(formula = Price ~ . , data = york_data)
york_result
summary(york_result)

longley
View(longley)
summary(longley)
str(longley)
eco_stat <- lm(formula = Employed ~ . , data = longley)
eco_stat
summary(eco_stat)

#CLASSIFICATION
getwd()
setwd("C:/Users/user/Documents")
customer_churn <- read.csv("C:/Users/user/Documents/Demo 1_ Support Vector Machines.csv")
View(customer_churn)

count(customer_churn$Churn)
#483 customers have churned denoted as '1' & 2850 has not churned '0'

str(customer_churn)
customer_churn$Churn <- sapply(customer_churn$Churn,factor)
str(customer_churn)

#split the data
sample_split <- floor(.7* nrow(customer_churn))
set.seed(1)
training <- sample(seq_len(nrow(customer_churn)), size=sample_split)
churn_train <- customer_churn[training, ]
churn_test  <- customer_churn[-training, ]

#build a support vector machine(SVN)
install.packages('e1071')
library(e1071)

svm_churn <- svm(Churn ~ .,churn_train)

install.packages('caret')
library(caret)

confusionMatrix(churn_train$Churn, predict(svm_churn),str(customer_churn),
                positive = '1')

#test data
prediction <- predict(svm_churn, churn_test[-1])
prediction_results <- table(pred=prediction, true=churn_test[,1])
print(prediction_results)

#Naive Bayes
install.packages("mlbench")
library(mlbench)
library(e1071)
library(caret)
library(mlbench)
library(plyr)
library(rpart)

#load & verify bank data
bank_loan <- read.csv("C:/Users/user/Documents/Demo 2_ Naive Bayes Classifier.csv")
View(bank_loan)
str(bank_loan)

#convert from int into a factor
bank_loan$Default <- sapply(bank_loan$Default,factor)
str(bank_loan)

#build the model
naive_model <- naiveBayes(Default~., data=bank_loan)
print(naive_model)

#the model creates conditional probability for each feature seperately
#we also have the apriori probabilities which indicates the distribution
#of our data

#predict
naive_predict <- predict(naive_model,bank_loan)
print(naive_predict)
table(naive_predict,bank_loan$Default)
nycflights13::

#DECISION TREE CLASSIFICATION
install.packages('rpart')
library(rpart)
install.packages('e1071','caret,mlbench','plyr','rpart')

bank_loan <- read.csv("C:/Users/user/Documents/Demo 2_ Naive Bayes Classifier.csv")
View(bank_loan)
str(bank_loan)
bank_loan$Default <- sapply(bank_loan$Default,factor)
str(bank_loan)

#build the model
tree_model <- rpart(Default ~ ., data= bank_loan, method = 'class' )
tree_model

#analyze results
printcp(tree_model)
plotcp(tree_model)
print(tree_model)
summary(tree_model)
plot(tree_model)

#k-fold validation

bank_loan <- read.csv("C:/Users/user/Documents/Demo 2_ Naive Bayes Classifier.csv")
View(bank_loan)
str(bank_loan)
bank_loan$Default <- sapply(bank_loan$Default,factor)
str(bank_loan)

#build the model
tree_model <- rpart(Default ~ ., data= bank_loan, method = 'class' )
tree_model

#analyze results
printcp(tree_model)
plotcp(tree_model)
print(tree_model)
summary(tree_model)
plot(tree_model)

#k-fold cross validation
folded_up <-createFolds(bank_loan, k=10,list=TRUE, returnTrain=FALSE)
train_set <- names(folded_up[1])
bank_loan[folded_up$train_set,]

#CLUSTERing
library(cluster)
getwd()
sch_performance <- read.csv("C:/Users/user/Documents/Demo 2_ Perform hierarchical clustering.csv")
View(sch_performance)
sch_performance <- agnes(sch_performance, diss = FALSE, 
                         metric = 'euclidian' )
plot(sch_performance)
print(sch_performance) #dendrogram

#k-mean clustering
bank_customer <- read.csv("C:/Users/user/Documents/Demo 4_K-Fold Cross validation.csv")
View(bank_customer) 
str(bank_customer)
set.seed(111)

cluster_up <- kmeans(bank_customer,3 , iter.max = 10)

#cleaning-up
# for k-mean cluster - it works majorly with numeric values
del_var <- names(bank_customer) %in% c('job','marital','education',
      'default','housing','loan','contact','month','poutcome')
bank_customer_num <- bank_customer[!del_var]
bank_customer_num <- na.omit(bank_customer_num)
bank_customer_num <- scale(bank_customer_num)
View(bank_customer_num)

# k-mean Clustering
cluster_up <- kmeans(bank_customer_num, 3, iter.max = 10)
str(cluster_up)
bank_customer_num <- cbind(bank_customer_num,
                           ClusterNum = cluster_up$cluster)
View(bank_customer_num)

#Graph and count of expected clusters
install.packages("mclust")
library(mclust)

fit <- Mclust(bank_customer_num)
plot(fit)

#Example 2 of clustering

unempstates <- read.csv("C:/Users/user/Documents/unempstates.csv")
View(unempstates)
str(unempstates)
rawt <- matrix(nrow=50, ncol = 50)
rawt <- t(rawt)

set.seed(1)
grpunemp2  <- kmeans(unempstates, centers = 3, nstart = 10)
(grpunemp2)
sort(grpunemp2$cluster)

#Heirarchical clustering

bank_customer <- read.csv("C:/Users/user/Documents/Demo 4_K-Fold Cross validation.csv")
View(bank_customer) 
str(bank_customer)
set.seed(111)

#Data Cleaning
bank_customer <- na.omit(bank_customer)
str(bank_customer)

#heirarchical clustering

cluster_h <- dist(bank_customer, method = 'euclidian')
fit <- hclust(cluster_h, method = 'ward.D2')
groups <- cutree(fit,k=3)
groups

bank_customer <- cbind(bank_customer, ClusterNum = groups)
View(bank_customer)
plot(fit)

#ASSOCIATION
library(arules)
Groceries_item <- read.transactions("C:/Users/user/Documents/Demo 1_Perform Association Using the Apriori Algorithm .csv")
inspect(Groceries_item[1:10])

AprioriForGroceries <- apriori(Groceries_item, 
      parameter = list(support= .006, confidence= .5))
summary(AprioriForGroceries)
inspect(AprioriForGroceries)
inspect(sort(AprioriForGroceries, by='confidence'))


