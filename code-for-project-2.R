
###project-2 for MSDS6372
kobe <- read.csv("C:\\Users\\chu001\\Documents\\Yongjun-Chu files\\SMU-data-science-application\\MSDS-6372\\Preject_2\\project2Data.csv")
names <- colnames(kobe)
dim(kobe)
kobe1=kobe

hist(kobe1$game_event_id)
hist(kobe1$game_id)
hist(kobe1$period)
hist(kobe1$shot_id)
hist(kobe1$arena_temp)
hist(kobe1$loc_y)


install.packages("klaR")
library(klaR)
#setwd("C:/Users/Adam/CatCluster/kmodes")
#data.to.cluster <- read.csv('dataset.csveader = TRUE, sep = ',')
cluster.results <-kmodes(kobe[,2], 10, iter.max = 10, weighted = FALSE ) #don't use the record ID as a clustering variable!

#create a matrix of correlation coefficients
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data <- kobe[, c(6,7,8,9,10,11,14,15,27, 28,29)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

#find the nature of the data frame
str(kobe1)
levels(kobe1$action_type)
levels(kobe1$combined_shot_type)

#change game_data into a "Date" type
library(xts)
kobe1$game_date = as.Date(kobe1$game_date,format="%m/%d/%Y")

#create another column as "action_type_old" the sane as the "action_type"
kobe1$action_type_old = kobe1$action_type

#change "action_type" into a charater type
kobe1$action_type <- as.character(kobe1$action_type)

#collapsing the "action_type" into 8 levels
collapse <- function (x) {
  if (grepl("Dunk\\sshot", x, ignore.case = T)) { x = "Dunk shot"}
  if (grepl("Layup\\sshot", x, ignore.case = T)) { x = "Layup shot"}
  if (grepl("Bank\\sshot", x, ignore.case = T))  { x = "Bank shot"}   
  if (grepl("Roll\\sshot", x, ignore.case = T)) { x = "Roll shot"}    
  if (grepl("Jump\\sshot", x, ignore.case = T)) { x = "Jump shot"}
  if (grepl("Hook\\sshot", x, ignore.case = T)) { x = "Hook shot"}
  if (grepl("Tip\\sshot", x, ignore.case = T))  { x = "Tip shot"}
  if (grepl("Fadeaway\\sshot", x, ignore.case = T)) { x = "Fadeaway shot"}    
  return (x)
}
kobe1$action_type <- as.factor(sapply(kobe1$action_type, collapse))
levels(kobe1$action_type)
str(kobe1)

#collapsing the "action_type" into 49 levels frm 55 levels
collapse <- function (x) {
  if (grepl("^Driving Floating Bank Jump Shot$", x, ignore.case = T)) { x = "Slam Dunk Shot"}
  if (grepl("^Hook Bank Shot$", x, ignore.case = T)) { x = "Slam Dunk Shot"}
  if (grepl("^Reverse Slam Dunk Shot$", x, ignore.case = T))  { x = "Slam Dunk Shot"}   
  if (grepl("^Running Slam Dunk Shot$", x, ignore.case = T)) { x = "Slam Dunk Shot"}    
  if (grepl("^Running Tip Shot$", x, ignore.case = T)) { x = "Tip Shot"}
  if (grepl("^Turnaround Finger Roll Shot$", x, ignore.case = T)) { x = "Slam Dunk Shot"}
  return (x)
}
kobe1$action_type <- as.factor(sapply(kobe1$action_type, collapse))
levels(kobe1$action_type)
str(kobe1)


#chnge the "matchup" into "Home" or "Away"
simple <- function (x) {
  if (grepl("@", x, ignore.case = T)) { x = "Away"}
  else { x = "Home"}
  return (x)
}

kobe1$matchup <- as.character(kobe1$matchup)
kobe1$matchup <- as.factor(sapply(kobe1$matchup, simple))
levels(kobe1$matchup)
str(kobe1)
write.csv(kobe1, file="C:\\Users\\chu001\\Documents\\Yongjun-Chu files\\SMU-data-science-application\\MSDS-6372\\Preject_2\\project2Data_new.csv", row.names = F)

levels(kobe1$shot_zone_area)
levels(kobe1$shot_zone_range)
levels(kobe1$shot_zone_basic)


###check the prediction efficiency with the logistic test dataset I generated in SAS
test1 <- read.csv("C:\\Users\\chu001\\Documents\\Yongjun-Chu files\\SMU-data-science-application\\MSDS-6372\\Preject_2\\test\\TEST.csv")
kobeout1 <- read.csv("C:\\Users\\chu001\\Documents\\Yongjun-Chu files\\SMU-data-science-application\\MSDS-6372\\Preject_2\\test\\KOBEOUT1.csv")
str(test1)
dim(test1)
str(kobeout1)
dim(kobeout1)

test1$shot_id
colnames(kobeout1)[1] <- "shot_id"
new = inner_join(kobeout1, test1)
head(new)

new1 <- data.frame(new$shot_made_flag, new$X_INTO_, new$IP_1, new$IP_0)
head(new1)
str(new1)
colnames(new1) <- c("obs","pred", "class1", "class0")

#to get LOSS function
str(new1)
loss = -1/length(new1$obs) * ( sum (new1$obs*log(new1$class1)) + sum((1-new1$obs)*log(new1$class0)) )
#0.6182

classes <- c("class1", "class0")

#change "1" to class1 and "0" to class0 so that new1 can be used in the following package
library(plyr)
new1$obs <- as.factor(new1$obs)
new1$pred <- as.factor(new1$pred)
new1$obs <- revalue(new1$obs, c("1"="class1", "0"="class0"))
new1$pred <- revalue(new1$pred, c("1"="class1", "0"="class0"))


###find the confusion matrix
table(new1$pred)
table(new1$obs)
nrow(filter(new1,pred=="class1" & obs=="class1" ))

#new$result <- ifelse((new$pred - new$shot_made_flag)==0, "Y", "N")
#table(new$result)

#or using the following package
library(caret)
confusionMatrix(new1$pred, new1$obs, positive = "class1", dnn = c("Prediction", "Reference"))

#more output
defaultSummary(new1, lev = classes)
twoClassSummary(new1, lev = classes)
install.packages("MLmetrics")
library(MLmetrics)
prSummary(new1, lev = classes)
mnLogLoss(new1, lev = classes)



###check the prediction efficiency with the LDA test dataset I generated in SAS
tout <- read.csv("C:\\Users\\chu001\\Documents\\Yongjun-Chu files\\SMU-data-science-application\\MSDS-6372\\Preject_2\\LDA_test\\TOUT.csv")
str(tout)
dim(tout)

tout1 <- data.frame(tout$shot_made_flag, tout$X_INTO_, tout$X1, tout$X0)
colnames(tout1) <- c("obs","pred", "class1", "class0")

#to get LOSS function
str(tout1)
loss = -1/length(tout1$obs) * ( sum (tout1$obs*log(tout1$class1)) + sum((1-tout1$obs)*log(tout1$class0)) )
#0.6972

classes <- c("class1", "class0")
head(tout1)

#change "1" to class1 and "0" to class0 so that new1 can be used in the following package
library(plyr)
tout1$obs <- as.factor(tout1$obs)
tout1$pred <- as.factor(tout1$pred)
tout1$obs <- revalue(tout1$obs, c("1"="class1", "0"="class0"))
tout1$pred <- revalue(tout1$pred, c("1"="class1", "0"="class0"))

#find the confusion matrix
table(tout1$pred)
table(tout1$obs)
nrow(filter(tout1,pred=="class1" & obs=="class1" ))

#or using the following package
library(caret)
confusionMatrix(tout1$pred, tout1$obs, positive = "class1", dnn = c("Prediction", "Reference"))

#more output
defaultSummary(tout1, lev = classes)
twoClassSummary(tout1, lev = classes)
install.packages("MLmetrics")
library(MLmetrics)
prSummary(tout1, lev = classes)
mnLogLoss(tout1, lev = classes)


###find the AUC, sensitivity adn LOSS function values on whole input dataset(Kobe1) for logistic regression. Cross-validaton (leave-one-out is used)
itself <- read.csv("C:\\Users\\chu001\\Documents\\Yongjun-Chu files\\SMU-data-science-application\\MSDS-6372\\Preject_2\\itself_test\\ITSELFOUT1.csv")
str(itself)
dim(itself)
head(itself)

itself1 <- data.frame(itself[,1], itself[,3], itself[,4], itself[,5])
colnames(itself1) <- c("obs","pred", "class1", "class0")
head(itself1, n=20)

#to get LOSS function
str(itself1)
loss = -1/length(itself1$obs) * ( sum (itself1$obs*log(itself1$class1)) + sum((1-itself1$obs)*log(itself1$class0)) )
#0.605

classes <- c("class1", "class0")
head(itself1)

#change "1" to class1 and "0" to class0 so that itself1 can be used in the following package
library(plyr)
itself1$obs <- as.factor(itself1$obs)
itself1$pred <- as.factor(itself1$pred)
itself1$obs <- revalue(itself1$obs, c("1"="class1", "0"="class0"))
itself1$pred <- revalue(itself1$pred, c("1"="class1", "0"="class0"))

#find the confusion matrix
table(itself1$pred)
table(itself1$obs)
nrow(filter(itself1,pred=="class1" & obs=="class1" ))

#or using the following package
library(caret)
confusionMatrix(itself1$pred, itself1$obs, positive = "class1", dnn = c("Prediction", "Reference"))

#more output
defaultSummary(itself1, lev = classes)
twoClassSummary(itself1, lev = classes)
install.packages("MLmetrics")
library(MLmetrics)
prSummary(itself1, lev = classes)



###An exmple on how to use caret package to get needed statistics (AUC, sensitivity, ...)
predicted <-  matrix(rnorm(50), ncol = 5)
observed <- rnorm(10)
apply(predicted, 2, postResample, obs = observed)

classes <- c("class1", "class2")
set.seed(1)
dat <- data.frame(obs =  factor(sample(classes, 50, replace = TRUE)),
                  pred = factor(sample(classes, 50, replace = TRUE)),
                  class1 = runif(50))
dat$class2 <- 1 - dat$class1            

defaultSummary(dat, lev = classes)
twoClassSummary(dat, lev = classes)
install.packages("MLmetrics")
library(MLmetrics)
prSummary(dat, lev = classes)
mnLogLoss(dat, lev = classes)


###modify the pred dataset (from Dr.T) accordingly
pred <- read.csv("C:\\Users\\chu001\\Documents\\Yongjun-Chu files\\SMU-data-science-application\\MSDS-6372\\Preject_2\\project2Pred.csv")
dim(pred)
pred1=pred
str(pred1)
pred1$shot_made_flag <- as.numeric()
levels(pred1$action_type)
table(pred1$action_type)
levels(pred1$combined_shot_type)

#compare if pred and kobe have the same number of levels for action_type
cpred<- data.frame(levels(pred1$action_type))
colnames(cpred)="action_type"

ckobe<- data.frame(levels(kobe$action_type))
colnames(ckobe)="action_type"
anti_join(cpred, ckobe)
#there are two levels in pred dataset that are not in the kobe dataset: Cutting Finger Roll Layup Shot and Turnaround Fadeaway Bank Jump Shot

#collapsing the "action_type" based on kobe1 data
collapse <- function (x) {
  if (grepl("^Driving Floating Bank Jump Shot$", x, ignore.case = T)) { x = "Slam Dunk Shot"}
  if (grepl("^Hook Bank Shot$", x, ignore.case = T)) { x = "Slam Dunk Shot"}
  if (grepl("^Reverse Slam Dunk Shot$", x, ignore.case = T))  { x = "Slam Dunk Shot"}   
  if (grepl("^Running Slam Dunk Shot$", x, ignore.case = T)) { x = "Slam Dunk Shot"}    
  if (grepl("^Running Tip Shot$", x, ignore.case = T)) { x = "Tip Shot"}
  if (grepl("^Turnaround Finger Roll Shot$", x, ignore.case = T)) { x = "Slam Dunk Shot"}
  return (x)
}

pred1$action_type <- as.factor(sapply(as.character(pred1$action_type), collapse))
table(pred1$action_type)
str(pred1)


#chnge the "matchup" into "Home" or "Away"
simple <- function (x) {
  if (grepl("@", x, ignore.case = T)) { x = "Away"}
  else { x = "Home"}
  return (x)
}

pred1$matchup <- as.character(pred1$matchup)
pred1$matchup <- as.factor(sapply(pred1$matchup, simple))
levels(pred1$matchup)
str(pred1)
dim(pred1)
#change game_data into a "Date" type
library(xts)
pred1$game_date = as.Date(pred1$game_date,format="%m/%d/%Y")

#change the shot_made_flag to numeric format (the same as the main input file)
pred1$shot_made_flag <- "."
pred1$shot_made_flag <- as.numeric(pred1$shot_made_flag)

write.csv(pred1, file="C:\\Users\\chu001\\Documents\\Yongjun-Chu files\\SMU-data-science-application\\MSDS-6372\\Preject_2\\project2Pred_new.csv", row.names = F)




