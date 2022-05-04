# Re-training neural net


library(neuralnet)
set.seed(42)


df_all <- read.csv(file = './text_recognition/df_all.csv')

# dropping track_name, corpus
df_all_t <- subset(df_all, select = -c(track_name, corpus, X))


##############################
##############################

# TODO inserire qui statistiche relative alla frequenza di termini per ogni classe (barplot)

##############################
##############################

library(caTools)

set.seed(42)
split = sample.split(df_all_t, SplitRatio = 0.75)
train2 = subset(df_all_t, split == TRUE)
test2 = subset(df_all_t, split == FALSE)




# training
nn2 <- neuralnet(music_genre ~ ., data = train2, hidden = c(27, 18), threshold = 3.8, lifesign = 'full', stepmax = 2e+06)
nn2$result.matrix

plot(nn2)


net.predict2 <- compute(nn, subset(test2, select = -c(music_genre))) # needs test without label to return probability distribution
net.predict2$neurons

test2$music_genre <- as.factor(test2$music_genre)

results2 <- data.frame(actual = test2$music_genre, prediction = net.predict2$net.result)
colnames(results2) <- c("actual", paste("prob", levels(test2$music_genre), sep = "_"))
results2

# getting argmax for every class
results2$predicted <- sub('.....', '', colnames(results2[-1])[apply(results2[-1], 1, which.max)])
results2



cm.nn2 <- table(results2$actual, results2$predicted)
cm.nn2


# Useful for metrics
n = sum(cm.nn2) # number of instances
nc = nrow(cm.nn2) # number of classes
diag = diag(cm.nn2) # number of correctly classified instances per class 
rowsums = apply(cm.nn2, 1, sum) # number of instances per class
colsums = apply(cm.nn2, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes


library(caret)
confusionMatrix(cm.nn2)


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

predictor_col.nn2 <- net.predict2$net.result
colnames(predictor_col.nn2) <- levels(test$music_genre)
colnames(predictor_col.nn2) <- c(paste(levels(test$music_genre), "pred", "NN2", sep = "_"))

predictor_col.nn2 <- as.data.frame(predictor_col.nn2)
predictor_col.nn2$true <- results$actual

library(fastDummies)
predictor_col.nn2 <- fastDummies::dummy_cols(predictor_col.nn2, select_columns = "true", remove_selected_columns = TRUE)
colnames(predictor_col.nn2) <- c('Alternative_pred_NN', 'Anime_pred_NN', 'Blues_pred_NN', 'Classical_pred_NN', 'Country_pred_NN', 'Electronic_pred_NN', 'Hip-Hop_pred_NN', 'Jazz_pred_NN', 'Rap_pred_NN', 'Rock_pred_NN', 'Alternative_true', 'Anime_true', 'Blues_true', 'Classical_true', 'Country_true', 'Electronic_true', 'Hip-Hop_true', 'Jazz_true', 'Rap_true', 'Rock_true')

resRocNN2 <- multi_roc(predictor_col.nn2, force_diag=TRUE)
unlist(resRocNN2$AUC)












