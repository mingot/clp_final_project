#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Discriminant analysis (Mingot)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(MASS) #lda, qda
library(ROCR) #performance
library(klaR) # partimat
library(sda) #sda(lda + shrinkage + feat selection), sda.ranking, catscores (both feat.selection)

# Dataframe to store resutls
error.da.df = data.frame(error_train=rep(0,3), error_test=rep(0,3))
rownames(error.da.df)=c("DDA","LDA","QDA")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DDA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dda.mod =  sda(as.matrix(train.df[,-c(1,corFeat8)]), train.df$label, 
               lambda=0, lambda.var=0, diagonal=TRUE) # no shrinkage

pred.dda.train = predict(dda.mod, as.matrix(train.df[,-c(1,corFeat8)]))
pred.dda.test = predict(dda.mod, as.matrix(test.df[,-c(1,corFeat8)]))

error.da.df[1,]=c(sum(pred.dda.train$class!=train.df$label) / nrow(train.df),
               sum(pred.dda.test$class!=test.df$label) / nrow(test.df))
cat("DDA Train error:", error.da.df[1,"error_train"])
cat("DDA Test error:", error.da.df[1,"error_test"])

# ROC curve
pred.dda = prediction(pred.dda.test$posterior[,2], test.df$label)
perf.dda = performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf.dda, colorize=TRUE)
performance(pred.dda, measure="auc") #AUC

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# LDA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lda.mod = lda(label ~ ., data = train.df[,-(corFeat8)])

pred.lda.train = predict(lda.mod, train.df[,-(corFeat8)])
pred.lda.test = predict(lda.mod, test.df[,-(corFeat8)])


error.da.df[2,] = c(sum(pred.lda.train$class!=train.df$label) / nrow(train.df),
                 sum(pred.lda.test$class!=test.df$label) / nrow(test.df))
cat("LDA Train error:", error.da.df[2,"error_train"])
cat("LDA Test error:", error.da.df[2,"error_test"])

#%%%%%%%%%%%%%%%%%% EXTRA

error.lda.df = data.frame(error_test=rep(0,4))
rownames(error.lda.df)=c("LDA","LDA_PCA","LDA_ICA", "LDA_selec")

# lda scatter plot 
names(train.df)= feat_names_short
partimat(label~x+y+z+size_feature_1,data=train.df,method="lda")

# feature selection
sda_ranking = sda.ranking(as.matrix(train.df[,-c(corFeat8,1)]), train.df$label, 
                          diagonal=FALSE, fdr=TRUE, plot.fdr=FALSE, verbose=TRUE)
plot(sda_ranking, top=20)

err=numeric(0)
for(top in 3:80){
  # top=20
  sda_feat_select = sda_ranking[1:top,1]
  lda_selec.mod = sda(as.matrix(train.df[,c(sda_feat_select)]), train.df$label)
  lda_selec.pred = predict(lda_selec.mod, as.matrix(test.df[,c(sda_feat_select)]))
  err = c(err, sum(lda_selec.pred$class!=test.df$label) / nrow(test.df))
}
plot(err, type="l", xlab="Nombre variables", ylab="error")

sda_feat_select = sda_ranking[1:43,1]

lda_shrink.mod = sda(as.matrix(train.df[,-c(1,corFeat8)]), train.df$label)
lda_selec.mod = sda(as.matrix(train.df[,c(sda_feat_select)]), train.df$label)
lda_selec_shrink.mod
summary(lda.mod)

lda_shrink.pred = predict(lda_shrink.mod, as.matrix(test.df[,-c(1,corFeat8)]))
lda_selec.pred = predict(lda_selec.mod, as.matrix(test.df[,c(sda_feat_select)]))
error.lda.df[1,] = error.da.df[2,"error_test"]#lda total 
error.lda.df[2,] = 0 #lda PCA
error.lda.df[3,] = 0 #lda ICA
error.lda.df[4,] = sum(lda_selec.pred$class!=test.df$label) / nrow(test.df) #lda selec

cat("LDA_shrink Test error:", sum(lda_shrink.pred$class!=test.df$label) / nrow(test.df))
cat("LDA_selec Test error:", sum(lda_selec.pred$class!=test.df$label) / nrow(test.df))


# ROC curve
pred.lda = prediction(pred.lda$x, test.df$label)
perf.lda = performance(pred.lda, measure = "tpr", x.measure = "fpr") 
plot(perf.lda, colorize=TRUE) # PLOT
performance(pred.lda, measure="auc") #AUC


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# QDA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qda.mod = qda(label ~ ., data = train.df[,-(corFeat8)])

pred.qda.train = predict(qda.mod, train.df[,-(corFeat8)])
pred.qda.test = predict(qda.mod, test.df[,-(corFeat8)])


error.da.df[3,] = c(sum(pred.qda.train$class!=train.df$label) / nrow(train.df),
                 sum(pred.qda.test$class!=test.df$label) / nrow(test.df))
cat("QDA Train error:",error.da.df[3,"error_train"])
cat("QDA Test error:",error.da.df[3,"error_test"])

# matrius de covariàncies per estudiar-ne el condicionament
kappa(cov(as.matrix(train.df[,-c(1,corFeat8)])))