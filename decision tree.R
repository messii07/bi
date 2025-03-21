

# Load required libraries
library(rpart)
library(rpart.plot)
library(ggplot2)
library(caret)
library(e1071)
library(pROC)

# Load dataset
data <- read.csv("C:\\Users\\abc\\Downloads\\BANK LOAN.csv") #change path 

# Remove the 'SN' column
data <- data[, -1]

data$DEFAULTER <- as.factor(data$DEFAULTER)
str(data)
set.seed(42)
splitIndex <- caret::createDataPartition(data$DEFAULTER, p=0.8, list=FALSE)
traindata <- data[-splitIndex, ]
testdata <- data[-splitIndex, ]  

# Check data distribution
table(traindata$DEFAULTER)
table(testdata$DEFAULTER)

# Train Decision Tree model
dt_model <- rpart(DEFAULTER ~ ., data=traindata, method="class")
print(dt_model)

# plot images
png("Decision Tree.jpeg", width=1300, height=968)
rpart.plot(dt_model, 
           main="Decision Tree Chart", 
           cex=1)
dev.off()
dt_prob <- predict(dt_model, testdata, type="prob")
dt_pred <- ifelse(dt_prob[,2] > 0.5, 1, 0)
dt_pred <- as.factor(dt_pred)
dt_conf_matrix <- confusionMatrix(dt_pred, testdata$DEFAULTER)
print(dt_conf_matrix)

dt_roc_curve <- roc(testdata$DEFAULTER, dt_prob[,2])
plot(dt_roc_curve, main="ROC Curve - Decision Tree", 
     col="purple", 
     lwd=2)
dt_auc_roc <- auc(dt_roc_curve)

cat("Decision Tree AUC:",dt_auc_roc)
