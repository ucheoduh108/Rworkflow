#REGRESSION ANALYSIS
#PROBLEM --- The data documents the amount of energy generated
#in a thermal power plant and the factors affecting the production.
#Find correlation between the dependent and the independent variables.
#Develop a regression model to predict the produced electricity.

#SOLUTION
power_plant <- read.csv("C:/Users/user/Documents/Regression Analysis Ex 2.csv")
head(power_plant)
str(power_plant)
summary(power_plant)
View(power_plant)

#Cleaning Up
del_var <- names(power_plant) %in% c('X','X.1','X.2','X.3')
power_plant_num <- power_plant[!del_var]
power_plant_num <- na.omit(power_plant_num)

View(power_plant_num)

power_plantResult <- lm(formula = PE ~ ., data = power_plant_num)
power_plantResult
summary(power_plantResult)

#since the P value of AT, V, AP and RH are all less than the significant level(0.05),
#the variables are all statistically significant.

#SAMPLE SPLIT
sample_split <- floor(.7* nrow(power_plant_num))
set.seed(1)

training <- sample(seq_len(nrow(power_plant_num)), size = sample_split)
power_plant_train <- power_plant_num[training,]
power_plant_test <- power_plant_num[-training,]

#BUILDING A SUPPORT VECTOR MACHINE

library(e1071)
svm_PE <- svm(PE ~ .,power_plant_train)

library('caret')
confusionMatrix(power_plant_train$PE, predict(svm_PE), str(power_plant_num),
               positive = '1' )



prediction <- predict(svm_PE, power_plant_test[-1])
prediction_results <- table(pred=prediction, true = power_plant_test[,1])
prediction_results


