# SVM

library(caret)

library(MLmetrics)

# fitControl <- trainControl(method = "repeatedcv",
#                           number = 1, 
#                           # repeats = 10,
#                           classProbs =  TRUE,
#                           verboseIter = TRUE,
#                           summaryFunction = multiClassSummary)

# svm <- train(make.names(music_genre)~., data = train, 
#                 method = 'svmRadial', 
#                 trControl = fitControl, 
#                 verbose = TRUE, 
#                 tunelength = 5, 
#                 metric = "ROC")


# svm_pred <- predict(svm, newdata=test,type = 'prob')
# roc_svm_one <- roc(test$music_genre, as.vector(svm_pred[,1]))






library(e1071)

svm.model <- svm(music_genre ~ ., data = train, kernel='polynomial', cost = 1, scale = FALSE, prob = TRUE)
svm.pred = predict(svm.model, test, prob = TRUE)
svm.table = table(svm.pred, test$music_genre)
cm.svm <- svm.table


# Useful for metrics
n = sum(cm.svm) # number of instances
nc = nrow(cm.svm) # number of classes
diag = diag(cm.svm) # number of correctly classified instances per class 
rowsums = apply(cm.svm, 1, sum) # number of instances per class
colsums = apply(cm.svm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes


library(caret)
confusionMatrix(cm.svm)



# Accuracy
accuracy <- sum(diag) / n
accuracy

# Precision, recall and F1
precision <- diag / colsums
recall <- diag / rowsums
f1 <- 2 * precision * recall / (precision + recall)
data.frame(precision, recall, f1)

# Macro-averaged Metrics
macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
data.frame(macroPrecision, macroRecall, macroF1)



# Roc
library(ROCR)
library(pROC)

test.ele <- as.character(test$music_genre)
test.ele[test.ele != 'Rap'] <- 'oth'
test.ele <- as.factor(test.ele)


pred.prob <- attr(svm.pred, "probabilities")
pred.to.roc = pred.prob[, 6]
pred.rocr = prediction(pred.to.roc, test.ele)

perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr = performance(pred.rocr, "tpr", "fpr")
plot(perf.tpr.rocr, colorize=T, main=paste("AUC:",(perf.rocr@y.values)))
abline(a=0, b=1)




library(multiROC)

predictor_col.SVM <- attr(svm.pred, "probabilities")
colnames(predictor_col.SVM) <- levels(test$music_genre)
colnames(predictor_col.SVM) <- c(paste(levels(test$music_genre), "pred", "SVN", sep = "_"))

predictor_col.SVM <- as.data.frame(predictor_col.SVM)
predictor_col.SVM$true <- results$actual

library(fastDummies)
predictor_col.SVM <- fastDummies::dummy_cols(predictor_col.SVM, select_columns = "true", remove_selected_columns = TRUE)
colnames(predictor_col.SVM) <- c('Alternative_pred_SVM', 'Anime_pred_SVM', 'Blues_pred_SVM', 'Classical_pred_SVM', 'Country_pred_SVM', 'Electronic_pred_SVM', 'Hip-Hop_pred_SVM', 'Jazz_pred_SVM', 'Rap_pred_SVM', 'Rock_pred_SVM', 'Alternative_true', 'Anime_true', 'Blues_true', 'Classical_true', 'Country_true', 'Electronic_true', 'Hip-Hop_true', 'Jazz_true', 'Rap_true', 'Rock_true')

resRocSVM <- multi_roc(predictor_col.SVM, force_diag=TRUE)
unlist(resRocSVM$AUC)

