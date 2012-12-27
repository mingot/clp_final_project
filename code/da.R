#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Discriminant analysis (Mingot)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(MASS) #lda, qda
library(ROCR) #performance
library(klaR) # partimat
library(sda) #sda(lda + shrinkage + feat selection), sda.ranking, catscores (both feat.selection)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# LDA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
names(train.df)= feat_names_short
# lda scatter plot 
partimat(label~x+y+z+size_feature_1,data=train.df,method="lda")

# feature selection
sda_ranking = sda.ranking(as.matrix(train.df[,-c(corFeat8,1)]), train.df$label, 
                          diagonal=FALSE, fdr=TRUE, plot.fdr=FALSE, verbose=TRUE)
plot(sda_ranking, top=40)

lda.mod = lda(label ~ ., data = train.df[,-(corFeat8)])
sda.mod = sda(as.matrix(train.df[,-c(1,corFeat8)]), train.df$label)
summary(lda.mod)

#### Evaluation
pred.sda = predict(sda.mod, as.matrix(test.df[,-c(1,corFeat8)]))
pred.lda = predict(lda.mod, test.df[,-(corFeat8)])
cat("LDA Test error:", sum(pred.lda$class!=test.df$label) / nrow(test.df))
cat("SDA Test error:", sum(pred.sda$class!=test.df$label) / nrow(test.df))

# ROC curve
pred = prediction(pred.lda$x, test.df$label)
perf = performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col=rainbow(10))

# AUC
performance(pred, measure="auc")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# QDA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qda.mod = qda(label ~ ., data = train.df[,-(corFeat8)])
qda.mod$terms


#### Evaluation
pred.qda = predict(qda.mod, test.df[,-(corFeat8)])
cat("QDA Test error:",sum(pred.qda$class!=test.df$label) / nrow(test.df))

# ROC curve
pred = prediction(pred.qda$class, test.df$label)
perf = performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col=rainbow(10))

# AUC
performance(pred, measure="auc")