# PROJECT UAS STATISTIKA MULTIVARIAT TERAPAN
# AULIA MIRFAH SETYO AYU DAMAYANTI - 22/493337/PA/21178

library(readxl)
bank <- read.csv("D:/Kuliah/Semester 3/Statistika Multivariat Terapan/bankloan.csv")
View(bank)
str(bank)

# Change the data types of certain colums
bank$CreditCard <- as.factor(bank$CreditCard)
bank$Online <- as.factor(bank$Online)
bank$CD.Account <- as.factor(bank$CD.Account)
bank$Securities.Account <- as.factor(bank$Securities.Account)
bank$Personal.Loan <- as.factor(bank$Personal.Loan)
bank$Education <- as.factor(bank$Education)

# Check for missing values
sum(is.na(bank))
# No missing value found

# Check for duplicates
sum(duplicated(bank))
# No duplicate records found

## Exploratory Data Analysis
install.packages("vctrs")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
bank %>%
  group_by(Personal.Loan) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = Personal.Loan, y = n)) +
  geom_bar(stat = 'Identity', fill = 'salmon') +
  geom_label(aes(label = n)) +
  theme_minimal() +
  labs(title = 'Count of loans approved and not approved', x= 'Loan Status', y = 'Count') +
  theme(panel.grid = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

# To understand the impact of variables like Age and Income, it is better to put them in a range or category
bank$Age_range <- cut(bank$Age, breaks = c(22, 30, 40, 50, 60, 70),
                      labels = c("22-30", "31-40", "41-50", "51-60", "61-70"))
table(bank$Income)
bank$Income_class <- cut(bank$Income, breaks = c(7, 24, 100, 150, 200, 230),
                         labels = c('Poor', 'MiddleClass', 'UpperMiddleClass', 'Rich', 'SuperRich'))

# I don't know what does the column experience imply. So I am dropping the column along with ID column
bank1 <- bank[, c(2,4:16)]

## Boxplot
# Income and Family
bank %>%
  ggplot(aes(x = Family, y = Income, color = as.factor(Family))) + geom_boxplot()

# Income and Personal Loan
bank %>%
  ggplot(aes(x = Personal.Loan, y = Income, color = Personal.Loan)) + geom_boxplot()

# Income and Credit Card
bank %>%
  ggplot(aes(x = CreditCard, y = Income, color = CreditCard)) + geom_boxplot()

# Income Class and Mortgage
bank %>%
  ggplot(aes(x = Income_class, y = Mortgage, color = Income_class)) + geom_boxplot()

# Online and CC Avg
bank %>%
  ggplot(aes(x = Online, y = CCAvg, color = Online)) + geom_boxplot()

# CC Avg and Education
bank %>%
  ggplot(aes(x = Education, y = CCAvg, color = Education)) + geom_boxplot()

# CC Avg and Age Range
bank %>%
  ggplot(aes(x = Age_range, y = CCAvg, color = Age_range)) + geom_boxplot()


## Modeliing

# Using Logistic Regression
model <- glm(Personal.Loan ~., data = bank1, family = 'binomial')
summary(model)

# Try to remove insignificant variables
model1 <- step(model, direction = 'backward', trace = 0)
summary(model1)

# New data frame without 'Zipcode'
bank2 <- bank1[, c(1,2,4:14)]

# Check for multicollinearity
library(car)
vif(model1)

bank3 <- bank2[, c(1:11,13)]

model2 <- glm(Personal.Loan ~., data = bank3, family = 'binomial')
summary(model2)

# We will run the vif once again to check the values
vif(model2)

# Split the data into train and test
# install.packages("cli")
library(cli)
# install.packages("caret")
library(caret)
set.seed(1234)
index <- createDataPartition(bank3$Personal.Loan, p = 0.8, list = FALSE)
train <- bank3[index,]
test <- bank3[-index,]

# We will use train data to create the model and also use the step function
model3 <- glm(Personal.Loan ~., data = train, family = 'binomial')
summary(model3)

model4 <- step(model3, direction = 'backward', trace = 0)
summary(model4)
# Column 'Mortage' has been removed

# Predict the test data
predicted <- predict(model4, newdata = test, type = 'response')
predicted
test$predicted <- predicted
test$class <- ifelse(test$predicted >= 0.5,1,0)
str(test$class)

# It is numerical vector which needs to be converted to factor vector
test$class <- as.factor(test$class)
str(test$class)
# It has been converted to a factor vector

# Confusion Matrix
confusionMatrix(test$class, test$Personal.Loan, positive = "1")


## Balance Data
library(ROSE)
table(train$Personal.Loan)
# 3616 customers have not been approved for a loan while 384 has been approved
# Over data = 3616 + 3616 = 7232
# Under data = 384 + 384 = 768
# Both data = 3616 +384 = 4000

set.seed(1234)
over_data <- ovun.sample(Personal.Loan~., data = train, method = 'over', N = 7232)$data
table(over_data$Personal.Loan)

set.seed(1234)
under_data <- ovun.sample(Personal.Loan~., data = train, method = 'under', N = 768)$data
table(under_data$Personal.Loan)

set.seed(1234)
both_data <- ovun.sample(Personal.Loan~., data = train, method = 'both', p =0.5, N = 4000)$data
table(both_data$Personal.Loan)

# Over data - LR
set.seed(1234)
lr_over_model <- glm(Personal.Loan~., data = over_data, family = 'binomial')
predict_lr_over <- predict(lr_over_model, newdata = test, type = 'response')
predict_lr_over
test$predict_lr_over <- predict_lr_over
test$class_lr_over <- ifelse(test$predict_lr_over >= 0.5,1,0)
str(test$class_lr_over)
test$class_lr_over <- as.factor(test$class_lr_over)
confusionMatrix(test$class_lr_over, test$Personal.Loan, positive = "1")

# Under data - LR
set.seed(1234)
lr_under_model <- glm(Personal.Loan~., data = under_data, family = 'binomial')
predict_lr_under <- predict(lr_under_model, newdata = test, type = 'response')
predict_lr_under
test$predict_lr_under <- predict_lr_under
test$class_lr_under <- ifelse(test$predict_lr_under >= 0.5,1,0)
str(test$class_lr_under)
test$class_lr_under <- as.factor(test$class_lr_under)
confusionMatrix(test$class_lr_under, test$Personal.Loan, positive = "1")

# Both data - LR
set.seed(1234)
lr_both_model <- glm(Personal.Loan~., data = both_data, family = 'binomial')
predict_lr_both <- predict(lr_both_model, newdata = test, type = 'response')
predict_lr_both
test$predict_lr_both <- predict_lr_both
test$class_lr_both <- ifelse(test$predict_lr_both >= 0.5,1,0)
str(test$class_lr_both)
test$class_lr_both <- as.factor(test$class_lr_both)
confusionMatrix(test$class_lr_both, test$Personal.Loan, positive = "1")

library(rpart)
# Over data - DT
set.seed(1234)
dt_over_model <- rpart(Personal.Loan~., data= over_data)
predict_dt_over <- predict(dt_over_model, newdata = test, type = 'class')
predict_dt_over
test$predict_dt_over <- predict_dt_over
str(test$predict_dt_over)
confusionMatrix(test$predict_dt_over, test$Personal.Loan, positive = "1")

# Under data - DT
set.seed(1234)
dt_under_model <- rpart(Personal.Loan~., data= under_data)
predict_dt_under <- predict(dt_under_model, newdata = test, type = 'class')
predict_dt_under
test$predict_dt_under <- predict_dt_under
str(test$predict_dt_under)
confusionMatrix(test$predict_dt_under, test$Personal.Loan, positive = "1")

# Both data - DT
set.seed(1234)
dt_both_model <- rpart(Personal.Loan~., data= both_data)
predict_dt_both <- predict(dt_both_model, newdata = test, type = 'class')
predict_dt_both
test$predict_dt_both <- predict_dt_both
str(test$predict_dt_both)
confusionMatrix(test$predict_dt_both, test$Personal.Loan, positive = "1")

library(randomForest)
# Over data - RF
set.seed(1234)
rf_over_model <- randomForest(Personal.Loan~., data= over_data)
predict_rf_over <- predict(rf_over_model, newdata = test, type = 'class')
predict_rf_over
test$predict_rf_over <- predict_rf_over
str(test$predict_rf_over)
confusionMatrix(test$predict_rf_over, test$Personal.Loan, positive = "1")

# Under data - RF
set.seed(1234)
rf_under_model <- randomForest(Personal.Loan~., data= under_data)
predict_rf_under <- predict(rf_under_model, newdata = test, type = 'class')
predict_rf_under
test$predict_rf_under <- predict_rf_under
str(test$predict_rf_under)
confusionMatrix(test$predict_rf_under, test$Personal.Loan, positive = "1")

# Both data - RF
set.seed(1234)
rf_both_model <- randomForest(Personal.Loan~., data= both_data)
predict_rf_both <- predict(rf_both_model, newdata = test, type = 'class')
predict_rf_both
test$predict_rf_both <- predict_rf_both
str(test$predict_rf_both)
confusionMatrix(test$predict_rf_both, test$Personal.Loan, positive = "1")

# Over data - LR
library(pROC)
set.seed(1234)
pred_lr_over <- predict(lr_over_model, newdata = test, type = "response")
pred_lr_over
test$pred_lr_over <- pred_lr_over
par(pty = "s")
roc(test$Personal.Loan, test$pred_lr_over, plot = TRUE, percent = TRUE, legacy.axes = TRUE,
    xlab = "FALSE POSITIVE PERCENTAGE", ylab = "TRUE POSITIVE PERCENTAGE", col = 'steelblue',
    lwd = 1, print.auc = TRUE, main = "AUC FOR LOGISTIC REGRESSION FOR OVER BALANCED DATA")


# Under data - LR
library(pROC)
set.seed(1234)
pred_lr_under <- predict(lr_under_model, newdata = test, type = "response")
pred_lr_under
test$pred_lr_under <- pred_lr_under
par(pty = "s")
roc(test$Personal.Loan, test$pred_lr_under, plot = TRUE, percent = TRUE, legacy.axes = TRUE,
    xlab = "FALSE POSITIVE PERCENTAGE", ylab = "TRUE POSITIVE PERCENTAGE", col = 'purple4',
    lwd = 1, print.auc = TRUE, main = "AUC FOR LOGISTIC REGRESSION FOR UNDER BALANCED DATA")


# Both data - LR
library(pROC)
set.seed(1234)
pred_lr_both <- predict(lr_both_model, newdata = test, type = "response")
pred_lr_both
test$pred_lr_both <- pred_lr_both
par(pty = "s")
roc(test$Personal.Loan, test$pred_lr_both, plot = TRUE, percent = TRUE, legacy.axes = TRUE,
    xlab = "FALSE POSITIVE PERCENTAGE", ylab = "TRUE POSITIVE PERCENTAGE", col = 'turquoise4',
    lwd = 1, print.auc = TRUE, main = "AUC FOR LOGISTIC REGRESSION FOR BOTH BALANCED DATA")


# Over data - DT
set.seed(1234)
pred_dt_over <- predict(dt_over_model, newdata = test, type = "prob")
pred_dt_over
test$pred_dt_over <- pred_dt_over[,2]
par(pty = "s")
roc(test$Personal.Loan, test$pred_dt_over, plot = TRUE, percent = TRUE, legacy.axes = TRUE,
    xlab = "FALSE POSITIVE PERCENTAGE", ylab = "TRUE POSITIVE PERCENTAGE", col = 'grey',
    lwd = 1, print.auc = TRUE, main = "AUC FOR DECISION TREE FOR OVER BALANCED DATA")


# Under data - DT
set.seed(1234)
pred_dt_under <- predict(dt_under_model, newdata = test, type = "prob")
pred_dt_under
test$pred_dt_under <- pred_dt_under[,2]
par(pty = "s")
roc(test$Personal.Loan, test$pred_dt_under, plot = TRUE, percent = TRUE, legacy.axes = TRUE,
    xlab = "FALSE POSITIVE PERCENTAGE", ylab = "TRUE POSITIVE PERCENTAGE", col = 'pink3',
    lwd = 1, print.auc = TRUE, main = "AUC FOR DECISION TREE FOR UNDER BALANCED DATA")


# Both data - DT
set.seed(1234)
pred_dt_both <- predict(dt_both_model, newdata = test, type = "prob")
pred_dt_both
test$pred_dt_both <- pred_dt_both[,2]
par(pty = "s")
roc(test$Personal.Loan, test$pred_dt_both, plot = TRUE, percent = TRUE, legacy.axes = TRUE,
    xlab = "FALSE POSITIVE PERCENTAGE", ylab = "TRUE POSITIVE PERCENTAGE", col = 'blue4',
    lwd = 1, print.auc = TRUE, main = "AUC FOR DECISION TREE FOR BOTH BALANCED DATA")


# Over data - RF
set.seed(1234)
pred_rf_over <- predict(rf_over_model, newdata = test, type = "prob")
pred_rf_over
test$pred_rf_over <- pred_rf_over[,2]
par(pty = "s")
roc(test$Personal.Loan, test$pred_rf_over, plot = TRUE, percent = TRUE, legacy.axes = TRUE,
    xlab = "FALSE POSITIVE PERCENTAGE", ylab = "TRUE POSITIVE PERCENTAGE", col = 'maroon',
    lwd = 1, print.auc = TRUE, main = "AUC FOR RANDOM FOREST FOR OVER BALANCED DATA")


# Under data - RF
set.seed(1234)
pred_rf_under <- predict(rf_under_model, newdata = test, type = "prob")
pred_rf_under
test$pred_rf_under <- pred_rf_under[,2]
par(pty = "s")
roc(test$Personal.Loan, test$pred_rf_under, plot = TRUE, percent = TRUE, legacy.axes = TRUE,
    xlab = "FALSE POSITIVE PERCENTAGE", ylab = "TRUE POSITIVE PERCENTAGE", col = 'yellow3',
    lwd = 1, print.auc = TRUE, main = "AUC FOR RANDOM FOREST FOR UNDER BALANCED DATA")


# Both data - RF
set.seed(1234)
pred_rf_both <- predict(rf_both_model, newdata = test, type = "prob")
pred_rf_both
test$pred_rf_both <- pred_rf_both[,2]
par(pty = "s")
roc(test$Personal.Loan, test$pred_rf_both, plot = TRUE, percent = TRUE, legacy.axes = TRUE,
    xlab = "FALSE POSITIVE PERCENTAGE", ylab = "TRUE POSITIVE PERCENTAGE", col = 'orange2',
    lwd = 1, print.auc = TRUE, main = "AUC FOR RANDOM FOREST FOR BOTH BALANCED DATA")

