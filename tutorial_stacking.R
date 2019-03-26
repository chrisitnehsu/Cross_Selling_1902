set.seed(1)
#Adapted from the caret vignette
library("caret")
library("mlbench")
library("pROC")
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]
my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  summaryFunction=twoClassSummary
)

library("rpart")
library("caretEnsemble")
model_list <- caretList(
  Class~., data=training,
  trControl=my_control,
  methodList=c("glm", "rpart")
)



pred <- predict(model_list$glm, testing, type = "prob")
threshold <- 0.5
pred <- ifelse(pred$M >= threshold, "M", "R") %>% factor(levels = c("M", "R"),labels = c("M", "R"))
conf <- confusionMatrix(pred, testing$Class, mode = "everything",
                        positive = "M");conf

#AUC
AUC <- colAUC(pred, testing[["Class"]])
AUC




#
glm_ensemble <- caretStack(
  model_list,
  method="glm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
stack_result <- predict(glm_ensemble, newdata=testing, type="prob")

colAUC(stack_result, testing$Class)
