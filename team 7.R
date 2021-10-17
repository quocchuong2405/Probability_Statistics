# 1.  Installing libraries and importing data
install.packages("dplyr")
install.packages("GGally")
install.packages("broom")

library(ggplot2)
library(devtools)
library(GGally)
library(dplyr)
library(broom)
library(readr)

gradeData <- read_csv("D:/CC19KTM - Documents/Probability and Statistics/Assignment/Data/grade.csv")
View(gradeData)

# 2.  Data cleaning
# Replacing NA values in the column G2 (median)
gradeData[is.na(gradeData)] = median(gradeData$G2, na.rm = TRUE)
head(gradeData)

length(gradeData$G2)

# 3.  Data Visualization
#   A. Data transformation
gradeData[gradeData == "GP"] <- '0'
gradeData[gradeData == "MS"] <- '1'

gradeData[gradeData == "M"] <- '0'
gradeData[gradeData == "F"] <- '1'

gradeData[gradeData == "U"] <- '0'
gradeData[gradeData == "R"] <- '1'

gradeData[gradeData == "GT3"] <- '0'
gradeData[gradeData == "LE3"] <- '1'

gradeData[gradeData == "A"] <- '0'
gradeData[gradeData == "T"] <- '1'

gradeData[gradeData == "at_home"] <- '0'
gradeData[gradeData == "services"] <- '1'
gradeData[gradeData == "teacher"] <- '2'
gradeData[gradeData == "health"] <- '3'
gradeData$Mjob[gradeData$Mjob == "other"] <- '4'
gradeData$Fjob[gradeData$Fjob == "other"] <- '4'

gradeData[gradeData == "course"] <- '0'
gradeData[gradeData == "home"] <- '1'
gradeData[gradeData == "reputation"] <- '2'
gradeData$reason[gradeData$reason == "other"] <- '3'

gradeData[gradeData == "father"] <- '0'
gradeData[gradeData == "mother"] <- '1'
gradeData$guardian[gradeData$guardian == "other"] <- '3'

gradeData[gradeData == "yes"] <- '0'
gradeData[gradeData == "no"] <- '1'

head(gradeData)

#   B. Descriptive statistics for each of the variables
class(gradeData$school) <- "numeric"
class(gradeData$sex) <- "numeric"
class(gradeData$address) <- "numeric"
class(gradeData$famsize) <- "numeric"
class(gradeData$Pstatus) <- "numeric"
class(gradeData$Mjob) <- "numeric"
class(gradeData$Fjob) <- "numeric"
class(gradeData$reason) <- "numeric"
class(gradeData$guardian) <- "numeric"
class(gradeData$schoolsup) <- "numeric"
class(gradeData$famsup) <- "numeric"
class(gradeData$paid) <- "numeric"
class(gradeData$activities) <- "numeric"
class(gradeData$nursery) <- "numeric"
class(gradeData$higher) <- "numeric"
class(gradeData$internet) <- "numeric"
class(gradeData$romantic) <- "numeric"

summary(gradeData)

#   C. Graphs (Box plot, Hist, Pairs, Quantile-quantile plot)
options(repr.plot.width=30, repr.plot.height=15)
par(mfrow=c(4,4))
boxplot(gradeData$G1,horizontal = TRUE, main = "G1", col = "green")
boxplot(gradeData$G2, horizontal = TRUE, main = "G2", col = "yellow")
boxplot(gradeData$G3, horizontal = TRUE, main = "G3", col = "orange")
boxplot(gradeData$age, horizontal = TRUE, main = "age")
boxplot(gradeData$absences, horizontal = TRUE, main = "absences")
boxplot(gradeData$studytime, horizontal = TRUE, main = "studytime")
boxplot(gradeData$health, horizontal = TRUE, main = "health")
boxplot(gradeData$goout, horizontal = TRUE, main = "goout")
boxplot(gradeData$freetime, horizontal = TRUE, main = "freetime")
boxplot(gradeData$Medu, horizontal = TRUE, main = "Medu")
boxplot(gradeData$Fedu, horizontal = TRUE, main = "Fedu")
boxplot(gradeData$famrel, horizontal = TRUE, main = "famrel")
boxplot(gradeData$Dalc, horizontal = TRUE, main = "Dalc")
boxplot(gradeData$Walc, horizontal = TRUE, main = "Walc")
boxplot(gradeData$traveltime, horizontal = TRUE, main = "traveltime")
boxplot(gradeData$failures, horizontal = TRUE, main = "failures")
#####
options(repr.plot.width=30, repr.plot.height=15)
par(mfrow=c(3,3))
boxplot(gradeData$G3 ~ gradeData$G1, horizontal = TRUE, main = "G1-G3", col = "green")
boxplot(gradeData$G3 ~ gradeData$G2, horizontal = TRUE, main = "G2-G3", col = "yellow")
boxplot(gradeData$G3 ~ gradeData$Medu, horizontal = TRUE, main = "Medu-G3")
boxplot(gradeData$G3 ~ gradeData$Fedu, horizontal = TRUE, main = "Fedu-G3")
boxplot(gradeData$G3 ~ gradeData$age, horizontal = TRUE, main = "age-G3")
boxplot(gradeData$G3 ~ gradeData$freetime, horizontal = TRUE, main = "absences-G3")
boxplot(gradeData$G3 ~ gradeData$studytime, horizontal = TRUE, main = "studytime-G3")
boxplot(gradeData$G3 ~ gradeData$health, horizontal = TRUE, main = "health-G3")
boxplot(gradeData$G3 ~ gradeData$goout, horizontal = TRUE, main = "goout-G3")
#####
options(repr.plot.width=30, repr.plot.height=15)
par(mfrow=c(3,3))
boxplot(gradeData$G3 ~ gradeData$school, horizontal = TRUE, main = "school-G3")
boxplot(gradeData$G3 ~ gradeData$address, horizontal = TRUE, main = "address-G3")
boxplot(gradeData$G3 ~ gradeData$sex, horizontal = TRUE, main = "sex-G3")
boxplot(gradeData$G3 ~ gradeData$higher, horizontal = TRUE, main = "higher-G3")
boxplot(gradeData$G3 ~ gradeData$failures, horizontal = TRUE, main = "failures-G3")
boxplot(gradeData$G3 ~ gradeData$famrel, horizontal = TRUE, main = "famrel-G3")
boxplot(gradeData$G3 ~ gradeData$reason, horizontal = TRUE, main = "reason-G3")
boxplot(gradeData$G3 ~ gradeData$romantic, horizontal = TRUE, main = "romantic-G3")
boxplot(gradeData$G3 ~ gradeData$nursery, horizontal = TRUE, main = "nursery-G3")
#####
options(repr.plot.width=30, repr.plot.height=15)
par(mfrow=c(4,4))
hist(gradeData$G1, main = "G1", col = "green")
hist(gradeData$G2, main = "G2", col = "yellow")
hist(gradeData$G3, main = "G3", col = "orange")
hist(gradeData$age, main = "age")
hist(gradeData$absences, main = "absences")
hist(gradeData$studytime, main = "studytime")
hist(gradeData$health, main = "health")
hist(gradeData$goout, main = "goout")
hist(gradeData$freetime, main = "freetime")
hist(gradeData$Medu, main = "Medu")
hist(gradeData$Fedu, main = "Fedu")
hist(gradeData$famrel,  main = "famrel")
hist(gradeData$Dalc, main = "Dalc")
hist(gradeData$Walc, main = "Walc")
hist(gradeData$traveltime, main = "traveltime")
hist(gradeData$failures, main = "failures")
#####
options(repr.plot.width=30, repr.plot.height=8)
par(mfrow=c(1,3))
qqnorm(gradeData$G1, frame = TRUE, main = "G1")
qqline(gradeData$G1, col = "steelblue", lwd = 2)

qqnorm(gradeData$G2, frame = TRUE, main = "G2")
qqline(gradeData$G2, col = "steelblue", lwd = 2)

qqnorm(gradeData$G3, frame = TRUE, main = "G3")
qqline(gradeData$G3, col = "steelblue", lwd = 2)
#####
subData = gradeData[,c("failures","age", "higher","absences","famrel","Medu", "Fedu", "G1", "G2", "G3")]
head(subData)
#####
options(repr.plot.width=30, repr.plot.height=15)
ggpairs(subData) + theme_bw()

# 4.  Build Model Prediction
#   Fitting linear regression models
LinearModel <- lm(G3 ~ .,data=gradeData)
summary(LinearModel)
#####
LinearModel_1 <- lm(G3 ~ ...1 +school+ famrel + absences + G1 + G2 , data = gradeData)
LinearModel_2 <- lm(G3 ~ school + famrel + absences + G1 + G2, data= gradeData)
LinearModel_3 <- lm(G3 ~ famrel + absences + G1 + G2, data = gradeData)
LinearModel_4 <- lm(G3 ~ absences + G1 + G2, data = gradeData)
LinearModel_5 <- lm(G3 ~ G1 + G2, data = gradeData)
LinearModel_6 <- lm(G3 ~ G2, data = gradeData)
#####
anova(LinearModel_6,LinearModel_5,LinearModel_4,LinearModel_3,LinearModel_2,LinearModel_1,LinearModel)
#####
summary(LinearModel_2)
#####
plot(LinearModel_2)

# 5.  Prediction
#   Evaluation
evaluate = gradeData$G3
evaluate = ifelse(evaluate >=10,"pass","fail")
observe = table(evaluate)
View (observe)
#####
Predict_G3 = predict(LinearModel_2,gradeData)
Predict_G3 = ifelse(Predict_G3>=10, "pass", "fail")
observe = table(Predict_G3)
View (observe)

#   Prediction a new data
newd = data.frame(school = 1,famrel =5,absences =20, G1 =10, G2 =11)
G3_predict = predict(LinearModel_2,newd)
round(G3_predict, digits = 4)
