#PART 1-Installing and loading R packages------------------------------------------------------------------
install.packages("gmodels")
install.packages("lubridate")
install.packages("plyr")
install.packages("ggplot2")
install.packages("caTools")
install.packages("e1071")
install.packages("ROCR")
install.packages("caret")
install.packages("ROSE")
library(gmodels)
library(lubridate)
library(plyr)
library(ggplot2)
library(caTools)
library(e1071)
library(ROCR)
library(caret)
library(ROSE)
#END OF PART 1-------------------------------------------------------------------------------------------

#PART 2- Loading files into R data frame-----------------------------------------------------------------
#Loading data into R data frame
#It takes around 6 minutes to load (skip to code line 37 for faster option)
#raw.data <- NULL
#files<-list.files()
#
#system.time(for(f in files) {
#  temp<-read.csv(f, sep=",", dec=",", header = FALSE)
#  raw.data<-rbind(raw.data,temp)
#})

#END OF PART 2------------------------------------------------------------------------------------------
#Saving raw data on my HDD
#save(raw.data,file="rawdata.rData")

#Loading data from .rData file (uncomment next line and run it to load data frame)
#load("rawdata.rData")

#PART 3 DATA CLEANING----------------------------------------------------------------------------------
#Exploring raw data
dim(raw.data) #111 variables 1.321.880 observations
class(raw.data) #Data frame
names(raw.data) #Column names are not copied.
head(raw.data, n=20)
tail(raw.data, n=20)
str(raw.data) #All features are factors
#Assign headers based on existing row in data frame
colnames(raw.data) <- as.character(unlist(raw.data[2,]))

#Deleting trash rows
arg <- raw.data$id=="id"
raw.data <- subset(raw.data, arg==FALSE)

#Selecting relevant features for model
features <- c("loan_status", "grade", "sub_grade", "open_acc","pub_rec", "dti", "delinq_2yrs",
              "inq_last_6mths", "emp_length", "annual_inc", "home_ownership",  "purpose", "addr_state",
              "loan_amnt","int_rate", "installment", "issue_d", "revol_bal", "revol_util")

raw.data <- subset(raw.data, select = features) 

#Deleting empty rows
raw.data <- raw.data[!apply(raw.data == "", 1, all),]

#Missing values for emp_length
summary(raw.data$emp_length) #There are 73.049 missing values

#Will keep missing values. Categorical variable. Will make six bins <1, 1-3, 3-6, 6-9, 10+, missing
options(scipen = 50)
plot(raw.data$emp_length, col="red")
raw.data$emp_cat <- rep(NA, length(raw.data$emp_length))
raw.data$emp_cat[which(raw.data$emp_length == "< 1 year")] <- "0-1"
raw.data$emp_cat[which(raw.data$emp_length == "1 year" | raw.data$emp_length=="2 years" | raw.data$emp_length=="3 years")] <- "1-3"
raw.data$emp_cat[which(raw.data$emp_length == "4 years" | raw.data$emp_length=="5 years" | raw.data$emp_length=="6 years")] <- "4-6"
raw.data$emp_cat[which(raw.data$emp_length == "7 years" | raw.data$emp_length=="8 years" | raw.data$emp_length=="9 years")] <- "7-9"
raw.data$emp_cat[which(raw.data$emp_length == "10+ years")] <- "10+"
raw.data$emp_cat[which(raw.data$emp_length == "n/a")] <- "missing"
raw.data$emp_cat <- as.factor(raw.data$emp_cat)
plot(raw.data$emp_cat, col="red", main="Histogram of factorial variable emp_cat")
summary(raw.data$emp_cat)
raw.data$emp_length <- NULL

#Preparing data for analysis
#int_rate variable
class(raw.data$int_rate) #It is factor, should be numeric
raw.data$int_rate <- as.numeric(sub("%","",raw.data$int_rate)) #Taking out % sign and converting into numeric
raw.data$int_rate <- raw.data$int_rate / 100
is.numeric(raw.data$int_rate) # TRUE
anyNA(raw.data$int_rate) #No missing values

#revol_util variable
class(raw.data$revol_util) #It is factor, should be numeric
raw.data$revol_util <- as.numeric(sub("%","",raw.data$revol_util)) #Taking out % sign and converting into numeric
raw.data$revol_util <- raw.data$revol_util / 100
is.numeric(raw.data$revol_util) # TRUE
anyNA(raw.data$revol_util) #There are missing values

index.NA <- which(is.na(raw.data$revol_util)) #766 missing values
raw.data$revol_util[index.NA] <- median(raw.data$revol_util, na.rm = TRUE) #All missing values replaced by median 0.542
anyNA(raw.data$revol_util) #No missing values

#revol_bal variable
class(raw.data$revol_bal) #It is factor, should be numeric
raw.data$revol_bal <- as.character(raw.data$revol_bal) #Converting into character
raw.data$revol_bal <- as.numeric(raw.data$revol_bal) # Converting into numeric
anyNA(raw.data$revol_bal) #No missing values

#installment variable
class(raw.data$installment) #It is factor, should be numeric
raw.data$installment <- as.character(raw.data$installment) #Converting into character
raw.data$installment <- as.numeric(raw.data$installment) #Converting into numeric
is.numeric(raw.data$installment) # TRUE
anyNA(raw.data$installment) #No missing values

#loan_amnt
class(raw.data$loan_amnt) #It is factor, should be numeric
raw.data$loan_amnt <- as.character(raw.data$loan_amnt) #Converting into character
raw.data$loan_amnt <- as.numeric(raw.data$loan_amnt) #Converting into numeric
is.numeric(raw.data$loan_amnt) # TRUE
anyNA(raw.data$loan_amnt) #No missing values

#annual_inc
class(raw.data$annual_inc) #It is factor, should be numeric
raw.data$annual_inc <- as.character(raw.data$annual_inc) #Converting into character
raw.data$annual_inc <- as.numeric(raw.data$annual_inc) #Converting into numeric
is.numeric(raw.data$annual_inc) # TRUE
anyNA(raw.data$annual_inc) #4 missing values
index.NA <- which(is.na(raw.data$annual_inc))
raw.data$annual_inc[index.NA] <- median(raw.data$annual_inc, na.rm = TRUE)
anyNA(raw.data$annual_inc) #No missing values

#laon_status
class(raw.data$loan_status) #It is factor
raw.data$loan_status <- as.character(raw.data$loan_status)
is.character(raw.data$loan_status)
#Taking only rows where laon_status is fully paid or charged off
arg <- raw.data$loan_status=="Fully Paid" | raw.data$loan_status=="Charged Off"
raw.data <- subset(raw.data, arg==TRUE) #Number of observations reduced to 553403

#Encoding loan_status 0 - Charged Off, 1 - Fully paid
raw.data$loan_status <- ifelse(raw.data$loan_status=="Fully Paid",1,0)
raw.data$loan_status <- as.integer(raw.data$loan_status) #Converting to integer
is.integer(raw.data$loan_status)
anyNA(raw.data$loan_status)

#dti
class(raw.data$dti) #It is factor, should be numeric
raw.data$dti <- as.character(raw.data$dti) #Converting into character
raw.data$dti <- as.numeric(raw.data$dti) #Converting into numeric
is.numeric(raw.data$dti) # TRUE
anyNA(raw.data$dti) #No missing values

#open_acc
class(raw.data$open_acc) #It is factor, should be numeric
raw.data$open_acc <- as.character(raw.data$open_acc) #Converting into character
raw.data$open_acc <- as.numeric(raw.data$open_acc) #Converting into numeric
is.numeric(raw.data$open_acc) # TRUE
anyNA(raw.data$open_acc) #No missing values

#pub_rec
class(raw.data$pub_rec) #It is factor, should be numeric
raw.data$pub_rec <- as.character(raw.data$pub_rec) #Converting into character
raw.data$pub_rec <- as.numeric(raw.data$pub_rec) #Converting into numeric
is.numeric(raw.data$pub_rec) # TRUE
anyNA(raw.data$pub_rec) #No missing values

#delinq_2yrs
class(raw.data$delinq_2yrs) #It is factor, should be numeric
raw.data$delinq_2yrs <- as.character(raw.data$delinq_2yrs) #Converting into character
raw.data$delinq_2yrs <- as.numeric(raw.data$delinq_2yrs) #Converting into numeric
is.numeric(raw.data$delinq_2yrs) # TRUE
anyNA(raw.data$delinq_2yrs) #No missing values

#inq_last_6mths
class(raw.data$inq_last_6mths) #It is factor, should be numeric
raw.data$inq_last_6mths <- as.character(raw.data$inq_last_6mths) #Converting into character
raw.data$inq_last_6mths <- as.numeric(raw.data$inq_last_6mths) #Converting into numeric
is.numeric(raw.data$inq_last_6mths) # TRUE
anyNA(raw.data$inq_last_6mths) #No missing values

str(raw.data)
#END OF PART 3-----------------------------------------------------------------------------------------

#PART 4 EXPLORATORY DATA ANALYSIS----------------------------------------------------------------------
# Distribution of Interest rate
hist(raw.data$int_rate, col = "red", main = "Distribution of Intrest rate", xlab = "Interest rate")
summary(raw.data$int_rate)

#Turning loan_status to factor
raw.data$loan_status <- factor(raw.data$loan_status)

#Distribution of grade scores
#Histogram of grade score colored by loan_status in percentage
plot1 <- ggplot(raw.data,aes(x=grade, y=((..count..)/sum(..count..))*100))
plot1 <- plot1 + geom_histogram(aes(fill=loan_status), color="black", stat = "count", alpha=0.6)
plot1 <- plot1 + theme_light()
plot1 <- plot1 + scale_fill_manual("Loan Status",values = c("red", "green")) +
  labs(y="Percent", x="Loan Grades from A (best) to G (poor)")
plot1 <- plot1 + ggtitle("Distribution of Loans By Grading Scores and Loan Status")
plot1

#Making Contingency Table to check percentage of grading score in relation with unpaid loans 
CrossTable(raw.data$grade, raw.data$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,
           prop.chisq = FALSE )

#Taking the highest loan purposes
arg <- raw.data$purpose == "credit_card" | raw.data$purpose == "debt_consolidation" |
        raw.data$purpose == "home_improvement" | raw.data$purpose == "major_purchase" | raw.data$purpose == "other"
j <- subset(raw.data, arg==TRUE)

#Making distribution of loans by purpose
plot2 <- ggplot(j,aes(x=purpose, y=((..count..)/sum(..count..))*100))
plot2 <- plot2 + geom_bar(aes(fill=loan_status), position = "dodge", stat = "count")
plot2 <- plot2 + theme_bw()
plot2 <- plot2 + scale_fill_manual("Loan Status",values = c("red", "green")) +
  labs(y="Percent", x="Loan Purpose")
plot2 <- plot2 + ggtitle("Distribution of Loans By Purpose")
plot2

#Making Contingency Table to check percentage of grading score in relation with unpaid loans 
CrossTable(raw.data$purpose, raw.data$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,
           prop.chisq = FALSE )

#Making scatter diagram to control relation between interest rates and loans grades
plot3 <- ggplot(raw.data, aes(x=int_rate, y=sub_grade)) + geom_point(aes(color=loan_status, alpha=0.4))
plot3 <- plot3 + theme_bw() + scale_fill_manual("Loan Status", values = c("red", "green")) +
  labs(y="Sub Grades", x="Interest Rates")
plot3

#Deleting detected outliers
arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="G1"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="F5"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="E5"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="E4"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="E3"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="E2"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="E1"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="D5"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="D4"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="D3"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="D2"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="D1"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="C5"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="C4"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="C3"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="C2"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="C1"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="B5"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="B4"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="B3"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="B2"
raw.data <- subset(raw.data, arg==FALSE)

arg <- raw.data$int_rate==0.06 & raw.data$sub_grade=="B1"
raw.data <- subset(raw.data, arg==FALSE)

#5-number summary statistics for annual income
summary(raw.data$annual_inc) #There are potential outliers

#Plotting scatter diagram to detect outliers
plot(raw.data$annual_inc, ylab = "Annual Income")

#Removing outliers
index.outliers <- which(raw.data$annual_inc > 1000000) #91 outliers detected
raw.data <- raw.data[-index.outliers,] #Outliers deleted

#Histogram for Annual Income
hist(raw.data$annual_inc, col="red", xlab = "Annual Income", main = "Histogram of Annual Income")

#Removing outliers for dti
summary(raw.data$dti)
plot(raw.data$dti)

outliers_upperlimit <- quantile(raw.data$dti, 0.75) + 1.5 * IQR(raw.data$dti) # upper_limit = 40.8
index.outliers.dti <- which(raw.data$dti > outliers_upperlimit | raw.data$dti < 0 ) #470 outliers
raw.data <- raw.data[-index.outliers.dti,] #Removing observations

#Removing outliers for open_acc
summary(raw.data$open_acc)
plot(raw.data$open_acc)
index.outliers2 <- which(raw.data$open_acc > 50 | raw.data$open_acc <0 ) #41 outliers
raw.data <- raw.data[-index.outliers2,] #Removing observations

#Removing outliers for pub_rec
summary(raw.data$pub_rec)
plot(raw.data$pub_rec)
index.outliers3 <- which(raw.data$pub_rec > 20 | raw.data$pub_rec <0 ) #8 outliers
raw.data <- raw.data[-index.outliers3,] #Removing observations

#Removing outliers for delinq_2yrs
summary(raw.data$delinq_2yrs)
plot(raw.data$delinq_2yrs)
index.outliers4 <- which(raw.data$delinq_2yrs > 20 | raw.data$delinq_2yrs <0 ) #7 outliers
raw.data <- raw.data[-index.outliers4,] #Removing observations

#No detecetd outliers for inq_last_6mths
summary(raw.data$inq_last_6mths)
plot(raw.data$inq_last_6mths)

#No detecetd outliers for installment
summary(raw.data$installment)
plot(raw.data$installment)

#Removing outliers for revol_bal
summary(raw.data$revol_bal)
plot(raw.data$revol_bal)
index.outliers5 <- which(raw.data$revol_bal > 500000 | raw.data$revol_bal <0 ) #56 outliers
raw.data <- raw.data[-index.outliers5,] #Removing observations

#Removing outliers for revol_util
summary(raw.data$revol_util)
plot(raw.data$revol_util)
index.outliers6 <- which(raw.data$revol_util > 2 | raw.data$revol_util <0 ) #2 outliers
raw.data <- raw.data[-index.outliers6,] #Removing outliers

#No detecetd outliers for loan_amnt
summary(raw.data$loan_amnt)
plot(raw.data$loan_amnt)

#Multicollinearity
cor(raw.data[, sapply(raw.data, class) != "factor"]) #Checking multicollinearity
#END OF PART 4------------------------------------------------------------------------------------------

#PART 5 MODEL BUILDING AND MODEL EVALUATION--------------------------------------------------------------

loan.model <- subset(raw.data, select = c(1,2,4:11,13,14,17:19)) 
anyNA(loan.model) # No missing values
dim(loan.model) #14 features + 1 response, 552,625 observations

#Splitting data set into training and test set
set.seed(123) #making results reproduciable


sample <- sample.split(loan.model$loan_status, 0.7)
train.data <- subset(loan.model, sample==TRUE)
test.data <- subset(loan.model, sample==FALSE)

#LOGISTIC REGRESSION

logistic.regressor <- glm(loan_status ~ ., family = "binomial", data = train.data)
summary(logistic.regressor)

#Predicting outcomes on test data
prob_pred <- predict(logistic.regressor, newdata = test.data, type = "response")
summary(prob_pred)

#Cut-off value = 0.5
pred_cut_off <- ifelse(prob_pred > 0.5, 1,0) #Setting cut-off to be at 0.5
table(test.data$loan_status,pred_cut_off )
pred <- prediction(pred_cut_off,test.data$loan_status)
perf <- performance(pred, "tpr", "fpr")
#Printing AUC Value
perf1 <- performance(pred, "auc")
print(perf1@y.values[[1]])
#Plotting the ROC-curve
roc.curve(test.data$loan_status, pred_cut_off,col="red", main="The ROC-curve for Model with cut-off=0.5")
text(0.6,0.2,paste("AUC=0.52"))
confusionMatrix(test.data$loan_status,pred_cut_off )

#Cut-off value = 0.8
pred_cut_off <- ifelse(prob_pred > 0.8, 1,0) #Setting cut-off to be at 0.8
table(test.data$loan_status,pred_cut_off )
pred <- prediction(pred_cut_off,test.data$loan_status)
perf <- performance(pred, "tpr", "fpr")

#Printing AUC Value
perf1 <- performance(pred, "auc")
print(perf1@y.values[[1]])
#Plotting the ROC-curve
roc.curve(test.data$loan_status, pred_cut_off,col="red", main="The ROC-curve for Model with cut-off=0.8")
text(0.6,0.2,paste("AUC=0.65"))
confusionMatrix(test.data$loan_status,pred_cut_off )

#Plotting proportion of fully paid vs charged off loans
options(scipen=20)
barchart(train.data$loan_status, main="Proportion of Fully Paid and Charged Off Loans (Training Set)", xlab="Number of Loans")

#Assuming investor wants to finance top 20% of new loans in his portfolio
cutoff <- quantile(prob_pred, 0.8)
pred_cut_20 <- ifelse(prob_pred > cutoff, 1,0)
true.value <- as.character(test.data$loan_status)
true.value <- as.integer(true.value)
true_and_pred <- cbind(true.value, pred_cut_20)

accepted_loans <- true_and_pred[pred_cut_20==1,1]
bad_rate <- (sum(accepted_loans==0) / length(accepted_loans))*100 #6.69% of bad loans in his portfolio

#Building Strategy Table 
accept_rate <- sort(seq(0,0.99,by=0.05), decreasing = TRUE)
cutoff <- c()
bad_rate <- c()
for(i in 1:length(accept_rate)) {
  cutoff[i] <- quantile(prob_pred, accept_rate[i])
  pred_cut <- ifelse(prob_pred > cutoff[i], 1,0)
  true.value <- as.character(test.data$loan_status)
  true.value <- as.integer(true.value)
  true_and_pred <- cbind(true.value, pred_cut)
  accepted_loans <- true_and_pred[pred_cut==1,1]
  bad_rate[i] <- (sum(accepted_loans==0) / length(accepted_loans))
}

#Making Strategy Table
strategy <- cbind(1 - accept_rate, cutoff, bad_rate)
colnames(strategy) <- c("Accept Rate","Cut-off Value", "Bad Rate")
strategy <- as.data.frame(strategy)

#Plotting Strategy Curve
curve <- as.matrix(strategy[-2])
curve[,2] <- curve[,2]
plot(curve, type="l",col="dark red", lwd=3, main="Strategy Curve")

#IMPROVING MODEL BY BALANCED DATA
#Making balanced data using SDG method
balanced.data <- ROSE(loan_status ~ ., data = train.data, seed = 1)$data
table(balanced.data$loan_status) #Now we have almost 50% 50%

#Building new logistic regression model
rose.regressor <- glm(loan_status ~ ., family = "binomial", data = balanced.data)
summary(rose.regressor)

#Making predictions on test set
prob_pred_rose <- predict(rose.regressor, newdata = test.data, type="response")
hist(prob_pred_rose)

#Evaluating new model
roc.curve(test.data$loan_status, prob_pred_rose, col="dark red", main="The ROC-curve for Improved Model")
text(0.6,0.2,paste("AUC=0.704"))
#END OF PART 5------------------------------------------------------------------------------------------