#------------library-----
library(car)
library(MASS)
library(ca)

#--------loading data-------
#clean NAs, save as an new file
IBM <- na.omit(IBM_HR_Data_new)

#delete variables: 
#Daily Rate, Employee Count, Application ID, Over 18, Standard Hours, Monthly Rate
IBM <- IBM[, -c(4, 9, 11, 21, 23, 28)]

summary(IBM)

#--------convert variables------

#convert char into factor type
#Attrition, Gender, OverTime
lookup <- c("Current employee" = 1, "Voluntary Resignation" = 0)
IBM$Attrition <- lookup[IBM$Attrition]
IBM$Attrition <- factor(IBM$Attrition)

#convert char into factor type
#BusinessTravel, Department, EducationField, JobRole, MaritalStatus, Employee Source

IBM$Gender <- factor(IBM$Gender)
IBM$OverTime <- factor(IBM$OverTime)
IBM$BusinessTravel <- factor(IBM$BusinessTravel)
IBM$Department <- factor(IBM$Department)
IBM$EducationField <- factor(IBM$EducationField)
IBM$JobRole <- factor(IBM$JobRole)
IBM$MaritalStatus <- factor(IBM$MaritalStatus)
IBM$`Employee Source` <- factor(IBM$`Employee Source`)

#-------separate train and test----
set.seed(489)

#Grab test and training sets
n = nrow(IBM)
s = sample(n, n/2)
IBM_Train = IBM[s,]
IBMt_Test = IBM[-s,]

#-------------ordinal factor analysis: polychoric-----
lookup <- c("Female" = 1, "Male" = 0)
IBM$Gender <- lookup[IBM$Gender]

lookup <- c("Single" = 0, "Married" = 1, "Divorced" = 2)
IBM$MaritalStatus <- lookup[IBM$MaritalStatus]

lookup <- c("Yes" = 1, "No" = 0)
IBM$OverTime <- lookup[IBM$OverTime]

lookup <- c("Non-Travel" = 0, "Travel_Rarely" = 1, "Travel_Frequently" = 2)
IBM$BusinessTravel <- lookup[IBM$BusinessTravel]



IBM.co <- IBM[, c(2,3,6,9,10,12,13,15,16,19,21,22,23,26)]
head(IBM.co)
summary(IBM.co)

library(corrplot)
c=cor(IBM.co)
corrplot(c, method = "ellipse")

install.packages("polycor")
library(polycor)
library(psych)

round(cor(IBM.co),2)

hetcor(IBM.co)

IBM.co$Gender = factor(IBM.co$Gender, levels = c(0,1), ordered  = T)
IBM.co$BusinessTravel = factor(IBM.co$BusinessTravel, levels = c(0,1,2), ordered  = T)
IBM.co$Education = factor(IBM.co$Education, levels = c(1,2,3,4,5), ordered  = T)
IBM.co$JobInvolvement = factor(IBM.co$JobInvolvement, levels = c(1,2,3,4), ordered  = T)
IBM.co$MaritalStatus = factor(IBM.co$MaritalStatus, levels = c(0,1,2), ordered  = T)
IBM.co$OverTime = factor(IBM.co$OverTime, levels = c(0,1), ordered  = T)
IBM.co$JobSatisfaction = factor(IBM.co$JobSatisfaction, levels = c(1,2,3,4), ordered  = T)
IBM.co$EnvironmentSatisfaction = factor(IBM.co$EnvironmentSatisfaction, levels = c(1,2,3,4), ordered  = T)
IBM.co$JobLevel = factor(IBM.co$JobLevel, levels = c(1,2,3,4,5), ordered  = T)
IBM.co$PerformanceRating = factor(IBM.co$PerformanceRating, levels = c(3,4), ordered  = T)
IBM.co$RelationshipSatisfaction = factor(IBM.co$RelationshipSatisfaction, levels = c(1,2,3,4), ordered  = T)
IBM.co$StockOptionLevel = factor(IBM.co$StockOptionLevel, levels = c(0,1,2,3), ordered  = T)
IBM.co$WorkLifeBalance = factor(IBM.co$WorkLifeBalance, levels = c(1,2,3,4), ordered  = T)

hetcor(IBM.co)

h = hetcor(IBM.co)
hCor = h$correlations
hCor

p = princomp(covmat = hCor, cor = T)
summary(p)
plot(p)
abline(1, 0, col = "red")

p2 = principal(hCor, nfactors = 7)
summary(p2)
print(p2$loadings, cutoff=.4)

#_--------basic model------
plot(model1)

model2 <- lm(Attrition ~ Department + JobLevel + HourlyRate, data = IBM)
summary(model2)

model2 <- lm(Attrition ~Age + DistanceFromHome + EnvironmentSatisfaction +
               logHourlyRate + JobInvolvement + JobLevel + JobSatisfaction + NumCompaniesWorked +
               logPercentSalaryHike + StockOptionLevel + TrainingTimesLastYear + WorkLifeBalance +
               YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion + YearsWithCurrManager, data=IBM)
summary(model2)
vif(model2)
hist(IBM$YearsWithCurrManager)


#----TRANSFORM log-----
IBM$logWorkingYears = log(IBM$TotalWorkingYears)
IBM$logEmployeeNumber = log(IBM$EmployeeNumber)
IBM$logHourlyRate = log(IBM$HourlyRate)
IBM$logPercentSalaryHike = log(IBM$PercentSalaryHike)
IBM$logYearsAtCompany = log(IBM$YearsAtCompany)
IBM$logYearsInCurrentRole = log(IBM$YearsInCurrentRole)
IBM$logYearsWithCurrManager = log(IBM$YearsWithCurrManager)

model2 <- lm(Attrition ~ Age + DistanceFromHome + EmployeeNumber + EnvironmentSatisfaction +
               HourlyRate + JobInvolvement + JobLevel + JobSatisfaction + NumCompaniesWorked +
               PercentSalaryHike + StockOptionLevel + TrainingTimesLastYear + WorkLifeBalance +
               YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, data = IBM)

model <- glm(Attrition ~.,family = "binomial", data = IBM)
summary(model)

model <- glm(Attrition ~ Age + BusinessTravel + Department + DistanceFromHome + Education
             EducationField + EmployeeNumber + EnvironmentSatisfaction + 
               Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + 
               JobSatisfaction + MaritalStatus + MothlyIncome + NumCompaniesWorked + OverTime + PercentSalaryHike + 
               RelationshipSatisfaction + StockOptionLevel + TrainingTimesLastYear + 
               WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion + 
               YearsWithCurrManager,data = IBM, family = binomial(link = "logit"))


#create the basic model
model1 <- lm(Attrition ~ Age  + DistanceFromHome + 
               EmployeeNumber + EnvironmentSatisfaction + 
               Gender + HourlyRate + JobInvolvement + JobLevel + 
               JobSatisfaction  + OverTime + PercentSalaryHike + 
               RelationshipSatisfaction + StockOptionLevel + TrainingTimesLastYear + 
               WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion + I(Age^2) + I(DistanceFromHome^2) + 
               I(EmployeeNumber^2) + I(EnvironmentSatisfaction^2) + I(JobInvolvement^2) + 
               I(PercentSalaryHike^2) + I(RelationshipSatisfaction^2) + 
               I(StockOptionLevel^2) + I(TrainingTimesLastYear^2) + I(YearsWithCurrManager^2) + 
               NumCompaniesWorked + YearsAtCompany +  DistanceFromHome:EnvironmentSatisfaction + 
               Age:JobSatisfaction + HourlyRate:JobSatisfaction + JobInvolvement:NumCompaniesWorked + 
               JobSatisfaction:PercentSalaryHike + JobLevel:RelationshipSatisfaction + 
               JobSatisfaction:RelationshipSatisfaction + JobLevel:StockOptionLevel + 
               JobSatisfaction:StockOptionLevel + Age:YearsAtCompany + TrainingTimesLastYear:YearsAtCompany + 
               YearsInCurrentRole:YearsAtCompany + TrainingTimesLastYear:YearsSinceLastPromotion + 
               YearsSinceLastPromotion:YearsAtCompany + YearsInCurrentRole:YearsSinceLastPromotion + 
               YearsSinceLastPromotion:YearsWithCurrManager + JobInvolvement:JobSatisfaction , data = IBM)

summary(model1)
vif(model1)
#-------stepwise regression--
set.seed(123)
runif(1)

help("stepAIC")
step$anova


model_back <- lm(Attrition ~ Age  + DistanceFromHome + 
                   EmployeeNumber + EnvironmentSatisfaction + 
                   Gender + HourlyRate + JobInvolvement + JobLevel + 
                   JobSatisfaction  + OverTime + PercentSalaryHike + 
                   RelationshipSatisfaction + StockOptionLevel + TrainingTimesLastYear + 
                   WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion + I(Age^2) + I(DistanceFromHome^2) + 
                   I(EmployeeNumber^2) + I(EnvironmentSatisfaction^2) + I(JobInvolvement^2) + 
                   I(PercentSalaryHike^2) + I(RelationshipSatisfaction^2) + 
                   I(StockOptionLevel^2) + I(TrainingTimesLastYear^2) + I(YearsWithCurrManager^2) + 
                   NumCompaniesWorked + YearsAtCompany +  DistanceFromHome:EnvironmentSatisfaction + 
                   Age:JobSatisfaction + HourlyRate:JobSatisfaction + JobInvolvement:NumCompaniesWorked + 
                   JobSatisfaction:PercentSalaryHike + JobLevel:RelationshipSatisfaction + 
                   JobSatisfaction:RelationshipSatisfaction + JobLevel:StockOptionLevel + 
                   JobSatisfaction:StockOptionLevel + Age:YearsAtCompany + TrainingTimesLastYear:YearsAtCompany + 
                   YearsInCurrentRole:YearsAtCompany + TrainingTimesLastYear:YearsSinceLastPromotion + 
                   YearsSinceLastPromotion:YearsAtCompany + YearsInCurrentRole:YearsSinceLastPromotion + 
                   YearsSinceLastPromotion:YearsWithCurrManager + JobInvolvement:JobSatisfaction, data = IBM)

step <- stepAIC(model_back, direction = "backward")
#delete TotalWorkingYears, Education, PerformanceRating, MonthlyIncome, JobLevel
summary(step)    #R^2 0.1163

#--------------forward-----
model_full <- lm(Attrition ~ . , data = IBM)
model_empty <- lm(Attrition ~ . , data = IBM)
summary(model_empty)

step <- stepAIC(model_empty, direction = "forward", scope=list(upper=model_full, lower=model_empty))
#delete none
summary(step)  #R^2 0.1118

#-------check Overfitting and Regularized----
set.seed(489)

#Grab test and training sets
n = nrow(IBM)
s = sample(n, n/2)
IBMOfit_Train = IBM[s,]
IBMOfit_Test = IBM[-s,]

#Compute least-squares fit
olsFit = lm(Attrition ~ Age  + DistanceFromHome + EmployeeNumber + EnvironmentSatisfaction + 
              Gender + HourlyRate + JobInvolvement + JobLevel + JobSatisfaction  + 
              OverTime + PercentSalaryHike + RelationshipSatisfaction + StockOptionLevel + 
              TrainingTimesLastYear + WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion + 
              I(Age^2) + I(DistanceFromHome^2) + I(EmployeeNumber^2) + I(EnvironmentSatisfaction^2) +
              I(JobInvolvement^2) + I(PercentSalaryHike^2) + I(RelationshipSatisfaction^2) + 
              I(StockOptionLevel^2) + I(TrainingTimesLastYear^2) + I(YearsWithCurrManager^2) + 
              NumCompaniesWorked + YearsAtCompany +  DistanceFromHome:EnvironmentSatisfaction + 
              Age:JobSatisfaction + HourlyRate:JobSatisfaction + JobInvolvement:NumCompaniesWorked + 
              JobSatisfaction:PercentSalaryHike + JobLevel:RelationshipSatisfaction + 
              JobSatisfaction:RelationshipSatisfaction + JobLevel:StockOptionLevel + 
              JobSatisfaction:StockOptionLevel + Age:YearsAtCompany + TrainingTimesLastYear:YearsAtCompany + 
              YearsInCurrentRole:YearsAtCompany + TrainingTimesLastYear:YearsSinceLastPromotion + 
              YearsSinceLastPromotion:YearsAtCompany + YearsInCurrentRole:YearsSinceLastPromotion + 
              YearsSinceLastPromotion:YearsWithCurrManager + JobInvolvement:JobSatisfaction, data = IBM)
summary(olsFit) #R^2 is very small

#Find the rmse
rmseOlsTrain = sqrt(mean(olsFit$residuals^2))
rmseOlsTrain

#Predict on the test set and compute error
olsPred = predict(olsFit, IBMOfit_Test)
rmseOlsTest = sqrt(mean((olsPred - IBMOfit_Test$Attrition)^2))
rmseOlsTest


#------------LDA----
IBM$logHourlyRate = log(IBM$HourlyRate)
IBM$logPercentSalaryHike = log(IBM$PercentSalaryHike)


install.packages("rattle")
plot(IBM[1,5,8,9,10,11,12], pch = 16, col=IBM$Attrition)

IBM.lda = lda(Attrition ~ Age + DistanceFromHome + EmployeeNumber + EnvironmentSatisfaction +
                HourlyRate + JobInvolvement + JobLevel + JobSatisfaction + NumCompaniesWorked +
                PercentSalaryHike + StockOptionLevel + TrainingTimesLastYear + WorkLifeBalance +
                YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, data = IBM)

IBM.lda = lda(Attrition ~ Age  + DistanceFromHome + EmployeeNumber + EnvironmentSatisfaction + 
                Gender + HourlyRate + JobInvolvement + JobLevel + JobSatisfaction  + 
                OverTime + PercentSalaryHike + RelationshipSatisfaction + StockOptionLevel + 
                TrainingTimesLastYear + WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion + 
                I(Age^2) + I(DistanceFromHome^2) + I(EmployeeNumber^2) + I(EnvironmentSatisfaction^2) +
                I(JobInvolvement^2) + I(PercentSalaryHike^2) + I(RelationshipSatisfaction^2) + 
                I(StockOptionLevel^2) + I(TrainingTimesLastYear^2) + I(YearsWithCurrManager^2) + 
                NumCompaniesWorked + YearsAtCompany +  DistanceFromHome:EnvironmentSatisfaction + 
                Age:JobSatisfaction + HourlyRate:JobSatisfaction + JobInvolvement:NumCompaniesWorked + 
                JobSatisfaction:PercentSalaryHike + JobLevel:RelationshipSatisfaction + 
                JobSatisfaction:RelationshipSatisfaction + JobLevel:StockOptionLevel + 
                JobSatisfaction:StockOptionLevel + Age:YearsAtCompany + TrainingTimesLastYear:YearsAtCompany + 
                YearsInCurrentRole:YearsAtCompany + TrainingTimesLastYear:YearsSinceLastPromotion + 
                YearsSinceLastPromotion:YearsAtCompany + YearsInCurrentRole:YearsSinceLastPromotion + 
                YearsSinceLastPromotion:YearsWithCurrManager + JobInvolvement:JobSatisfaction, data = IBM)

IBM.lda = lda(Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction +
                logHourlyRate + JobInvolvement + JobLevel + JobSatisfaction + NumCompaniesWorked +
                logPercentSalaryHike + StockOptionLevel + TrainingTimesLastYear + WorkLifeBalance +
                YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion + YearsWithCurrManager, data = IBM)

print(IBM.lda)

#PC's positive componen
print(IBM.lda$scaling[order(IBM.lda$scaling[,1]),])

#we can use "predict" just like 'lm'. 
#note here that we are predicting on the training set!
IBM.lda.values = predict(IBM.lda)


#look at the separation obtained by the two variates
par(mar=c(0,0,0,0))
ldahist(data=IBM.lda.values$x[,1], g=IBM$Attrition)

#now, let's plot the transformed data so we can see the classification
IBM.lda.values$x      #the scores are stored in the x-parameter
plot(IBM.lda.values$x[, 1], col = factor(IBM$Attrition), pch=16)

#Compute a confusion matrix
table(IBM$Attrition, IBM.lda.values$class)
print(IBM.lda$scaling)

#Another confusion matrix function that gives percentage errors in the confusion matrix
source("Confusion.R")
table(IBM$Attrition, p$class)
confusion(IBM.lda.values)

#Now， lets separate into test and training. This first version is too simplistic
#beacuse it doesn't obey the distribution of IBM in the original datase
s.lda = sample(nrow(IBM), nrow(IBM) * .5)
IBMTrain = IBM[s.lda,]
IBMTest = IBM[-s.lda,]

#build the model on the trainig set
IBM.lda = lda(Attrition ~ Age  + DistanceFromHome + EmployeeNumber + EnvironmentSatisfaction + 
                Gender + HourlyRate + JobInvolvement + JobLevel + JobSatisfaction  + 
                OverTime + PercentSalaryHike + RelationshipSatisfaction + StockOptionLevel + 
                TrainingTimesLastYear + WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion + 
                I(Age^2) + I(DistanceFromHome^2) + I(EmployeeNumber^2) + I(EnvironmentSatisfaction^2) +
                I(JobInvolvement^2) + I(PercentSalaryHike^2) + I(RelationshipSatisfaction^2) + 
                I(StockOptionLevel^2) + I(TrainingTimesLastYear^2) + I(YearsWithCurrManager^2) + 
                NumCompaniesWorked + YearsAtCompany +  DistanceFromHome:EnvironmentSatisfaction + 
                Age:JobSatisfaction + HourlyRate:JobSatisfaction + JobInvolvement:NumCompaniesWorked + 
                JobSatisfaction:PercentSalaryHike + JobLevel:RelationshipSatisfaction + 
                JobSatisfaction:RelationshipSatisfaction + JobLevel:StockOptionLevel + 
                JobSatisfaction:StockOptionLevel + Age:YearsAtCompany + TrainingTimesLastYear:YearsAtCompany + 
                YearsInCurrentRole:YearsAtCompany + TrainingTimesLastYear:YearsSinceLastPromotion + 
                YearsSinceLastPromotion:YearsAtCompany + YearsInCurrentRole:YearsSinceLastPromotion + 
                YearsSinceLastPromotion:YearsWithCurrManager + JobInvolvement:JobSatisfaction, data = IBMTrain)

IBM.lda = lda(Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction +
                logHourlyRate + JobInvolvement + JobLevel + JobSatisfaction + NumCompaniesWorked +
                logPercentSalaryHike + StockOptionLevel + TrainingTimesLastYear + WorkLifeBalance +
                YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion + YearsWithCurrManager, data = IBMTrain)

#look at the output
IBM.lda

#The "scaling" in the result is the "loadings" of the original variables
#and can help us interpret the result
print(IBM.lda$scaling[order(IBM.lda$scaling[,1]),])
IBM.lda.values = predict(IBM.lda, IBMTest)

#LOOK AT THE SEPARATION OBTAINED BY THE variate
ldahist(data = IBM.lda.values$x[,1], g = IBMTest$Attrition)

IBM.lda.values$x
plot(IBM.lda.values$x[,1], col = factor(IBMTest$Attrition), pch=16)

table(IBMTest$Attrition, IBM.lda.values$class)


#---try logistic regression-----
s.lda = sample(nrow(IBM), nrow(IBM) * .8 )
IBMTrain = IBM[s.lda,]
IBMTest = IBM[-s.lda,]

model_logistic = glm(Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction +
                       logHourlyRate + JobInvolvement + JobLevel + JobSatisfaction + NumCompaniesWorked +
                       logPercentSalaryHike + StockOptionLevel + TrainingTimesLastYear + WorkLifeBalance +
                       YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion + YearsWithCurrManager, 
                     family = binomial(link='logit'), data = IBMTrain)
summary(model_logistic)

pred = predict(model_logistic, newdata = IBMTest, type = 'response')
print(pred)   #we get predicted probabilities

#let's use the .5 cutoff to classify them
pred = ifelse(pred > 0.5, 1, 0)
table(pred, IBMTest$Attrition)      #look at false positives and false negative

misClasificError = mean(pred != IBMTest$Attrition)
print(paste('Accuracy', 1-misClasificError))


#--------let's look at the ROC curve
library(ROCR)
p = predict(model_logistic, newdata = IBMTest, type = "response")
pr = prediction(p, IBMTest$Attrition)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)   #performance measure and plotting

#compute the area under the curve
auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
auc     #0.6806557 which is not the best

# ------linear discriminant analysis----
fit = lda(Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction +
            logHourlyRate + JobInvolvement + JobLevel + JobSatisfaction + NumCompaniesWorked +
            logPercentSalaryHike + StockOptionLevel + TrainingTimesLastYear + WorkLifeBalance +
            YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion + YearsWithCurrManager, data = IBM)
print(fit)
pred = predict(fit, IBMTest)
pred

table(IBMTest$Attrition, pred$class)

library(ROCR)
pred = prediction(pred$posterior[,2], IBMTest$Attrition)
perf = performance(pred, "tpr", "fpr")
par(mar=c(4,4,4,4))
plot(perf)

#compute the area under the curve
auc = performance(pred, measure = "auc")
auc = auc@y.values[[1]]
auc



#------------cluster analysis
summary(IBM_Train)

IBM_cluster <- IBM[,-c(4,7,14,31)]
s.lda = sample(nrow(IBM_cluster), nrow(IBM_cluster) * .2)
IBM_Train = IBM_cluster[s.lda,]
IBM_Test = IBM_cluster[-s.lda,]
head(IBM_Train)

#---seprate
set.seed(489)

#Grab test and training sets
n = nrow(IBM)
s = sample(n, n/2)
IBM_Train = IBM[s,]
IBMt_Test = IBM[-s,]

#--------
IBM_Train$Gender = factor(IBM_Train$Gender, levels = c(0,1), ordered  = T)
IBM_Train$BusinessTravel = factor(IBM_Train$BusinessTravel, levels = c(0,1,2), ordered  = T)
IBM_Train$Education = factor(IBM_Train$Education, levels = c(1,2,3,4,5), ordered  = T)
IBM_Train$JobInvolvement = factor(IBM_Train$JobInvolvement, levels = c(1,2,3,4), ordered  = T)
IBM_Train$MaritalStatus = factor(IBM_Train$MaritalStatus, levels = c(0,1,2), ordered  = T)
IBM_Train$OverTime = factor(IBM_Train$OverTime, levels = c(0,1), ordered  = T)
IBM_Train$JobSatisfaction = factor(IBM_Train$JobSatisfaction, levels = c(1,2,3,4), ordered  = T)
IBM_Train$EnvironmentSatisfaction = factor(IBM_Train$EnvironmentSatisfaction, levels = c(1,2,3,4), ordered  = T)
IBM_Train$JobLevel = factor(IBM_Train$JobLevel, levels = c(1,2,3,4,5), ordered  = T)
IBM_Train$PerformanceRating = factor(IBM_Train$PerformanceRating, levels = c(3,4), ordered  = T)
IBM_Train$RelationshipSatisfaction = factor(IBM_Train$RelationshipSatisfaction, levels = c(1,2,3,4), ordered  = T)
IBM_Train$StockOptionLevel = factor(IBM_Train$StockOptionLevel, levels = c(0,1,2,3), ordered  = T)
IBM_Train$WorkLifeBalance = factor(IBM_Train$WorkLifeBalance, levels = c(1,2,3,4), ordered  = T)



IBM.x = as.matrix(IBM_Train)

IBM.dist = dist(IBM.x)
IBM.mds = isoMDS(IBM.dist)
IBM.mds$stress

plot(IBM.mds$points, type = "n")
text(IBM.mds$points, labels = as.character(1:nrow(IBM.x)))

cluster1 = hcluster(IBM.dist)

plot(cluster1, cex = 0.6, hang = -1)
clusters = cutree(cluster1, k=5)

head(clusters)

plot(IBM.mds$points, col = clusters)
text(IBM.mds$points, labels = as.character(1:nrow(IBM.x)))

#---------logistic regression----
library(glmnet)
s.lda = sample(nrow(IBM), nrow(IBM) * .8)
IBM_Train = IBM[s.lda,]
IBM_Test = IBM[-s.lda,]

x <- model.matrix(Attrition ~ Age + DistanceFromHome + EnvironmentSatisfaction +
                         logHourlyRate + JobInvolvement + JobLevel + JobSatisfaction + NumCompaniesWorked +
                         logPercentSalaryHike + StockOptionLevel + TrainingTimesLastYear + WorkLifeBalance +
                         YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion + YearsWithCurrManager, IBM_Train) [,-1]

x <- model.matrix(Attrition ~logHourlyRate + JobInvolvement + logPercentSalaryHike + TrainingTimesLastYear, IBM_Train) [,-1]

y <- ifelse(IBM_Train$Attrition == "pos", 1,0)

y <- as.matrix(sample(0:1,18560,replace = TRUE))

set.seed(123)
cv.lasso <- cv.glmnet(x,y, alpha = 1, family = "binomial")
model <- glmnet(x,y,alpha = 1, family = "binomial", lambda = cv.lasso$lambda.max)
coef(model)
rmsetrain = sqrt(mean(model$residuals^2))
#-----predict-------
library(dplyr)
x.test <- model.matrix(Attrition ~Age + DistanceFromHome + EnvironmentSatisfaction +
                         logHourlyRate + JobInvolvement + JobLevel + JobSatisfaction + NumCompaniesWorked +
                         logPercentSalaryHike + StockOptionLevel + TrainingTimesLastYear + WorkLifeBalance +
                         YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion + YearsWithCurrManager, IBM_Test) [,-1]

x.test <- model.matrix(Attrition ~logHourlyRate + JobInvolvement + logPercentSalaryHike + TrainingTimesLastYear, IBM_Test) [,-1]

probabilies <- model %>% predict(newx = x.test)
predict.classes <- ifelse(probabilies > 0.5, 1, 0)
observed.classes <- IBM_Test$Attrition
mean(predict.classes == observed.classes)
