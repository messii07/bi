
bankloan <- read.csv("C:\\Users\\abc\\Downloads\\BANK LOAN.csv")
library(caret)
index <- createDataPartition(bankloan$DEFAULTER, p=0.8, list=FALSE)
View(bankloan)
bankloan2 <- subset(bankloan, select = c(-DEFAULTER))
bankloan3 <- scale(bankloan2)
traindata <- bankloan3[index,]
testdata <- bankloan2[-index,]
Ytrain <- bankloan$DEFAULTER[index]
Ytest <- bankloan$DEFAULTER[-index]
library(class)
model_knn <- knn(traindata, testdata, k=23, cl= Ytrain)
table(Ytest, model_knn)
specificity(as.factor(Ytest),as.factor(model_knn),cutoff = 0.5)
sensitivity(as.factor(Ytest),as.factor(model_knn),cutoff = 0.5)