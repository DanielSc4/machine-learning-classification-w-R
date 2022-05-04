# Metrics
# Palette: #ef476f, #ffd166, #06d6a0, #118ab2, #073b4c

set.seed(42)

accs <- c(0.5444, 0.553, 0.7263)

mf <- data.frame(name = factor(c("SVM", "Neural network", "Neural Network + FE")),
                 value = accs,
                 sd = c(0.0088, 0.0088, 0.0088),
                 L = c(0.5356, 0.5442, 0.7351),
                 U = c(0.5531, 0.5617, 0.7175))

mf$name <- factor(mf$name, levels = mf$name)

require(ggplot2)
ggplot(mf, aes(x = name, y = value)) +
  geom_errorbar(aes(ymax = U, ymin = L), size = 1, colour = "#118ab2") +
  geom_point(size = 4, colour = "#ef476f") +
  ggtitle("Accuracy per model") +
  scale_y_continuous(limits = c(0.5, 0.8))
  


# Horizontal barplot, for each label record balanced accuracy score

b_acc <- read.csv("./metrics/b_accuracy models2.csv")

b_acc$Class <- factor(b_acc$Class, levels = sample(levels(as.factor(b_acc$Class))))
b_acc$Model <- factor(b_acc$Model, levels = sample(levels(as.factor(b_acc$Model))))

ggplot(data=b_acc, aes(x=Class, y= b_acc, fill = Model)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8)+
  geom_text(aes(label = round(b_acc, digits = 2)), hjust = 1.2, vjust=0.5, color="white",
            position = position_dodge(0.8), size=3.5)+
  scale_fill_brewer(palette="Paired") +
  scale_fill_manual(values=c('#06d6a0','#118ab2', '#073b4c')) +
  coord_flip() +
  ggtitle("Balanced accuracy per class") +
  theme_minimal()


# Horizontal barplot, for each label record AUC score

m_auc <- read.csv("./metrics/auc models.csv")

m_auc$Class <- factor(m_auc$Class, levels = sample(levels(as.factor(m_auc$Class))))
m_auc$Model <- factor(m_auc$Model, levels = sample(levels(as.factor(m_auc$Model))))

ggplot(data=m_auc, aes(x=Class, y= AUROC, fill = Model)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8)+
  geom_text(aes(label = round(AUROC, digits = 2)), hjust = 1.2, vjust=0.5, color="white",
            position = position_dodge(0.8), size=3.5)+
  scale_fill_brewer(palette="Paired") +
  scale_fill_manual(values=c('#06d6a0','#118ab2', '#073b4c')) +
  coord_flip() +
  ggtitle("AUC per class") +
  theme_minimal()








#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm = TRUE), sd = sd(x[[col]], na.rm = TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func, varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

b_acc2 <- data_summary(b_acc, varname="b_acc", groupnames=c("Class", "Model"))



########################################
########################################
# CM_NN2
########################################
########################################

cm_nn2 <- read.csv("./metrics/cm_nn2.csv")
cm_nn2 <- data.frame(cm_nn2[,-1], row.names = cm_nn2[,1])
cm_nn2 <- t(cm_nn2)


# Useful for metrics
n = sum(cm_nn2) # number of instances
nc = nrow(cm_nn2) # number of classes
diag = diag(as.matrix(cm_nn2)) # number of correctly classified instances per class 
rowsums = apply(cm_nn2, 1, sum) # number of instances per class
colsums = apply(cm_nn2, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

confusionMatrix(cm_nn2)



# Accuracy
accuracy <- sum(diag) / n
accuracy

# Precision, recall and F1
precision <- diag / colsums
recall <- diag / rowsums
f1 <- 2 * precision * recall / (precision + recall)
data.frame(precision, recall, f1)





val_acc2 <- read.csv("./metrics/AccVal2.csv")

ggplot(data=val_acc, aes(x=Resample, y=Model, group=1)) +
  geom_line()+
  geom_point()+
  scale_y_continuous(limits = c(0, 0.8))

ggplot(val_acc2, aes(x=Resample, y= Acc, colour = Model)) + 
  geom_line()

ggplot(val_acc2, aes(x = Resample, y = Acc, group = 1)) + 
  geom_line(aes(color = Model)) + 
  scale_color_manual(values = c("#06d6a0", "#118ab2", "#073b4c"))






