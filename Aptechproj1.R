#         CASE STUDY
#According to a recent study, a worldwide telecom industry is expected to 
#be valued at close to 1.2 trillion Euros at the end of 2023, currently valued at 
#1.196 trillion Euros. Due to the stiff competition among the service providers,
#with so many service providers and incentives, customers switch to a new one 
#and are spoilt for choice.

#Variables	          Description
#Churn	             1 if customer cancelled service, 0 if not
#AccountWeeks	       number of weeks customer has had active account
#ContractRenewal	   1 if customer recently renewed contract, 0 if not
#DataPlan	           1 if customer has data plan, 0 if not
#DataUsage	         gigabytes of monthly data usage
#CustServCalls	     number of calls into customer service
#DayMins	           average daytime minutes per month
#DayCalls            average number of daytime calls
#MonthlyCharge	     average monthly bill
#OverageFee	         largest overage fee in last 12 months
#RoamMins	           average number of roaming minutes

#         PROPOSED SOLUTION
#Build a predictive ML model that could predict the customer churn for this 
#anonymous telecom giant and help them prevent it as much as possible.
#Identify the properties,pattern & trends that lead to customer churn.
#Determine the Accuracy and Recall of the ML model. 
#Recall - how often the model predicts correctly.

#         PROJECT PLAN
#Data pre-processing and Exploratory Data Analysis.
#Model building (Training, test and Prediction Phase).
#Validation
#Deployment
#Monitoring


telecom <- read.csv("C:/Users/user/Desktop/DMX/Datasets/Additional Datasets/Classification Ex 2.csv")
                    

head(telecom)
str(telecom)
summary(telecom)
View(telecom)
sum(is.na(telecom))

#LINEAR REGRESSION
telecom_result <- lm(formula = Churn ~ . , data = telecom )

telecom_result

summary(telecom_result)


# Convert categorized data into factor 
telecom$Churn <- sapply(telecom$Churn, factor)
str(telecom$Churn)

telecom$ContractRenewal <- sapply(telecom$ContractRenewal, factor)
str(telecom$ContractRenewal)

telecom$DataPlan <- sapply(telecom$DataPlan, factor)
str(telecom$DataPlan)

telecom$CustServCalls <- sapply(telecom$CustServCalls, factor)
str(telecom$CustServCalls)

#CLASSIFICATION 

#split the data
sample_split <- floor(.7* nrow(telecom))

set.seed(1)

training <- sample(seq_len(nrow(telecom)), size=sample_split)

churn_train <- telecom[training, ]

churn_test  <- telecom[-training, ]

#build a support vector machine(SVN)
library(e1071)

svm_churn <- svm(Churn ~ .,churn_train)


#test data/Prediction
prediction <- predict(svm_churn, churn_test[-1])

prediction_results <- table(pred=prediction, true=churn_test[,1])

print(prediction_results)


# EVALUATION OF MODEL
library(caret)

confusionMatrix(churn_train$Churn, predict(svm_churn),str(telecom),
                positive = '1')


# RANDOM FOREST CLASSIFICATION

install.packages('randomForest')
library('randomForest')


sample_split <- floor(.7* nrow(telecom))

set.seed(123)

training <- sample(seq_len(nrow(telecom)), size=sample_split)

churn_train <- telecom[training, ]

churn_test  <- telecom[-training, ]

#CREATING THE RANDOM FOREST MODEL
rf_model <- randomForest(Churn ~ . , data = churn_train, ntree =100)

#MAKE PREDICTIONS ON THE TEST SET
predictions <- predict(rf_model, churn_test)

#EVALUATE THE MODEL
confusion_matrix <- table(Actual = churn_test$Churn, 
                          Predicted =predictions)

print(confusion_matrix)

#CALCULATE ACCURACY
accuracy <- sum(diag(confusion_matrix)) /
  
sum(confusion_matrix)

print(paste('Accuracy :',accuracy))


#EXPLORATORY ANALYSIS
library(ggplot2)
library('dplyr')

ggplot(data=telecom) + stat_count(mapping=aes(x=Churn))

ggplot(data=telecom) + geom_col(mapping=aes(x=Churn,y=RoamMins))

ggplot(data=telecom) + geom_col(mapping=aes(x=Churn,y=DataUsage))

ggplot(data=telecom) + geom_col(mapping=aes(x=Churn,y=MonthlyCharge))

ggplot(data=telecom) + geom_col(mapping=aes(x=Churn,y=OverageFee))

ggplot(data=telecom) + geom_col(mapping=aes(x=Churn,y=CustServCalls))



counts <- table( telecom$CustServCalls)
barplot(counts,
        main= 'Churn Distribution vs CustServCalls',
        xlab= 'Churn',
        ylab = 'CustServCalls',
        col= c('grey','cornflowerblue'),
        legend= rownames(counts),beside=TRUE)

counts <- table(telecom$ContractRenewal)
barplot(counts,
        main= 'Churn Distribution vs ContractRenewal',
        xlab= 'Churn',
        ylab = 'ContractRenewal',
        col= c('grey','cornflowerblue'),
        legend= rownames(counts),beside=TRUE)




#                  INTERPRETATION


#  LINEAR REGRESSION ANALYSIS

#  P-VALUE
#The p-value in Linear regression is a critical tool for assessing the significance
#of individual predictors, making decisions about variable inclusion, 
#interpretating model results and ensuring that the model assumptions are met.
#It plays a central role in hypothesis testing and model building. A low p-value
#(typically less than a chosen significance level ,eg 0.05) suggests that you can
#reject the null hypothesis, indicating the variable has a statistically 
#significant impact on the dependent variable.
#From the the above values, the p-values of ContractRenewal = <2e-16,
#CustServCalls = <2e-16, and RoamsMins = 0.000118,indicates that ContractRenewal,
#CustServCalls and RoamMins have statistical significant impact on a 
#customer cancelling services and leaving the telecommunication company.

#  COEFFICIENTS
#Coefficient in Linear Regression determines the impact of each independent 
#variable on the dependent variable. Coeffiecients offers insight into the 
#direction  and strength of the relationships. A positive coefficent indicates 
#a positive correlation, while a negative coefficient signifies a negative 
#correlation. The magnitude of the coefficient indicates the strength of the 
#relationship.
#The following are the variables with positive coefficients:AccountWeeks= 6.888e-05,
#CustServCalls =5.860e-02, DayCalls=1.397e-03, DayCalls=4.261e-04,
#Overagefee=1.398e-02, RoamMins = 8.949e-03.
#In relation with the p-values, CustServCalls has the strongest relationship 
#with the tendency of a customer to leave the telecommunication company, followed
#by the RoamMins.

#  RESIDUAL STANDARD ERROR(RSE)
#A high RSE signifies that the model's prediction deviates, on average, 
#significantly from the actual data points.It means the model explains a smaller
#portion of the variance in the dependent variable.
#On the other hand , an RSE value of 0.321 should be considered relatively low. 
#A low RSE indicates that the model's predictions are, on average, very close to 
#the actual data point and the model provides a good fit to the data.
#This means that the residuals(differences between observed and predicted values)
#are small, and the model explains a significant portion of the variance in the
#dependent variable.

#F-Statistic
#The F-statistic value of 70.35 with 10 and 3267 degree of freedom suggests that 
#the linear regression model is statistically significant, indicating that at 
#least one of the dependent variables in the model is contributing significantly
#to explaining the variance in the dependence variable. It is a strong indication
#of the model's overall goodness of fit and the significance of the included 
#independent variables.


#CLASSIFICATION

#USING SVM

#Accuracy of the model = 89.19%

#p-value = <2e-16

#SENSITIVITY = 92%
#Sensitivity in the context of svm is also known as the 'True Positive Rate' or 
#'Recall'. It measures the ability of the svm to correctly identify or detect
#positive instances or cases. A high sensitivity ensures that fewer positive cases 
#are missed.It helps in understanding how well the model performs in identifying  
#the target class. 


#RANDOM FOREST CLASSIFICATION

#Accuracy = 92.9878%




#CONCLUSION

#With a prediction accuracy of 92.9878%, the Random Forest model is the best 
#model to predict possibly customer churn from this anonymous telecommunication
#giant and should be adopted.

#Finally, i suggest that the company should focus on and improve CustServCalls,
#RoamsMins and maybe the ContractRenewal to prevent future migration of customers 
#from the company.




