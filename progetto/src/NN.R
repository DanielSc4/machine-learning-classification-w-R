# NN

library(neuralnet)
set.seed(42)

# Speed: 18sec per step. 1 step = 1000 samples
# for early stopping you can raise threshold from 0.1 to 0.5 or something like that
# or
# you can set stepmax below 1e+05 (the default value), for example, at 60k steps the min threshold is about 0.53
nn <- neuralnet(music_genre ~ ., data = train, hidden = c(11, 11), threshold = 2.2, lifesign = 'full', stepmax = 2e+05)

plot(nn)
nn$result.matrix

net.predict <- compute(nn, test[-12]) # needs test without label to return probability distribution
net.predict$neurons

# getting probability for each class
results <- data.frame(actual = test$music_genre, prediction = net.predict$net.result)
colnames(results) <- c("actual", paste("prob", levels(test$music_genre), sep = "_"))
results

# getting argmax for every class
results$predicted <- sub('.....', '', colnames(results[-1])[apply(results[-1], 1, which.max)])
results

cm <- table(results$actual, results$predicted)
cm


# metricssssss


# Useful for metrics
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes


library(caret)
confusionMatrix(cm)



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




library(multiROC)

predictor_col.nn <- net.predict$net.result
colnames(predictor_col.nn) <- levels(test$music_genre)
colnames(predictor_col.nn) <- c(paste(levels(test$music_genre), "pred", "NN", sep = "_"))

predictor_col.nn <- as.data.frame(predictor_col.nn)
predictor_col.nn$true <- results$actual

library(fastDummies)
predictor_col.nn <- fastDummies::dummy_cols(predictor_col.nn, select_columns = "true", remove_selected_columns = TRUE)
colnames(predictor_col.nn) <- c('Alternative_pred_NN', 'Anime_pred_NN', 'Blues_pred_NN', 'Classical_pred_NN', 'Country_pred_NN', 'Electronic_pred_NN', 'Hip-Hop_pred_NN', 'Jazz_pred_NN', 'Rap_pred_NN', 'Rock_pred_NN', 'Alternative_true', 'Anime_true', 'Blues_true', 'Classical_true', 'Country_true', 'Electronic_true', 'Hip-Hop_true', 'Jazz_true', 'Rap_true', 'Rock_true')

resRocNN <- multi_roc(predictor_col.nn, force_diag=TRUE)
unlist(resRocNN$AUC)



# 10 fold cross validation during train of NN (and SVM (check method parameter on train function))


library(tidyverse) 
library(caret)

# manually executed 10 fold cross validation, this code now does't work after Re-NN implementation
# (first implementation: data <- train and target is music_genre)
set.seed(100)
trctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)
nn_fit <- train(factor(target) ~., data = data, method = "brnn", trControl=trctrl, tuneLength = 0)
nn_fit

# unfolding
pred <- nn_fit$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)

eachfold <- pred %>%                                        
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
eachfold

# boxplot per l'accuratezza media dei kfold
ggplot(data=eachfold, aes(x=Resample, y=Accuracy, group=1)) +
  geom_boxplot(color="maroon") +
  geom_point() +
  theme_minimal()


# PER METRICHE: seguire il codice sopra per generare CM, accuracy, f1-score, roc ecc...







