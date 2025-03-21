library(caret)
library(pROC)
library(ggplot2)

df <-read.csv("C:\\Users\\abc\\Downloads\\BANK LOAN.csv")

head (df)
df <- df[,-1]
df$DEFAULTER <- as.factor(df$DEFAULTER)
str(df)

set.seed(123)

splitindex <- createDataPartition(df$DEFAULTER, p=0.8,list=FALSE)
traindata <- df[splitindex,]
testdata <- df[-splitindex,]

table(traindata$DEFAULTER)
table(testdata$DEFAULTER)
table(df$DEFAULTER)

log_model <- glm(DEFAULTER~., data=traindata, family = "binomial")
summary(log_model)

probabilities <- predict(log_model, testdata, type = "response")
predictions <- ifelse(probabilities > 0.6,1,0)

conf_matrix <- confusionMatrix(as.factor(predictions),as.factor(testdata$DEFAULTER))
print(conf_matrix)