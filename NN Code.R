rm(list=ls())
library(neuralnet)
#Eligibility of the Customer for Loan Sanction##########################################################################
loan.df <- read.csv("Training.csv")
#Data Cleaning -----------------------------------------------------------------------------------------------------
###Renaming the column to have similar style-----
loan.df <- loan.df[,-c(1)]
colnames(loan.df)[c(5,9,10,11,12)] <- c("SelfEmployed","LoanAmountTerm","CreditHistory","PropertyArea","LoanStatus") 

### Replace the missing values, factor the data, convert coloumns into numeric values. We use this for all models------------------------------------------------------------------
loan.df["Dependents"][is.na(loan.df["Dependents"])] <- 0
loan.df$Dependents <- as.integer(loan.df$Dependents)
loan.df["Dependents"][is.na(loan.df["Dependents"])] <- 3
loan.df["LoanAmountTerm"][is.na(loan.df["LoanAmountTerm"])] <- median(loan.df$LoanAmountTerm, na.rm = TRUE)
loan.df["ApplicantIncome"][is.na(loan.df["ApplicantIncome"])] <- mean(loan.df$ApplicantIncome, na.rm = TRUE)
loan.df["CoapplicantIncome"][is.na(loan.df["CoapplicantIncome"])] <- 0
loan.df["LoanAmount"][is.na(loan.df["LoanAmount"])] <- median(loan.df$LoanAmount, na.rm = TRUE)
loan.df["CreditHistory"][is.na(loan.df["CreditHistory"])] <- 0
loan.df["Gender"][is.na(loan.df["Gender"])] <- max(loan.df$Gender, na.rm = TRUE)
loan.df$Education<- factor(loan.df$Education, levels=c("Graduate","Not Graduate"))
loan.df["Education"][is.na(loan.df["Education"])] <- "Not Graduate"
loan.df$Married<- factor(loan.df$Married, levels=c("Yes","No"))
loan.df["Married"][is.na(loan.df["Married"])] <- 'No'
loan.df$SelfEmployed<- factor(loan.df$SelfEmployed, levels=c("Yes","No"))
loan.df["SelfEmployed"][is.na(loan.df["SelfEmployed"])] <- 'Yes'
loan.df[loan.df == "Y"] <- "1"
loan.df[loan.df == "N"] <- "0"
loan.df$LoanStatus <- as.integer(loan.df$LoanStatus)
summary(loan.df) 

loan.df1 <- loan.df[,-c(1,2,4,5,11)]
set.seed(1)  
train.index <- sample(c(1:dim(loan.df1)[1]), dim(loan.df1)[1]*0.7)  
train.df <- loan.df1[train.index, ]
valid.df <- loan.df1[-train.index, ]
summary(train.df)


nn <- neuralnet(LoanStatus ~ Dependents+ApplicantIncome+CoapplicantIncome+LoanAmount+LoanAmountTerm+CreditHistory, data = train.df, linear.output = F, hidden = 2)
plot(nn, rep="best")
nn.pred <- predict(nn, valid.df, type = "response")
nn.pred.classes <- ifelse(nn.pred > 0.5, 1, 0)
confusionMatrix(as.factor(nn.pred.classes), as.factor(valid.df$LoanStatus))

#Validating the area under the curve - Doesnt work
tree.results <- data.frame(actual = valid.df$LoanStatus, predicted = nn.pred.classes)
r <- roc(tree.results$actual, tree.results$predicted)
plot.roc(r)
auc(r)
