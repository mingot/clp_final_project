#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Discriminant analysis (Mingot)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(MASS) #lda, qda
library(ROCR) #performance
library(klaR) # partimat
library(sda) #sda(lda + shrinkage + feat selection), sda.ranking, catscores (both feat.selection)

# Dataframe to store results
error.da.df = data.frame(error_train=rep(0,3), error_test=rep(0,3))
rownames(error.da.df)=c("DDA","LDA","QDA")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DDA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dda.mod =  sda(as.matrix(train.df[,-1]), train.df$label, 
               lambda=0, lambda.var=0, diagonal=TRUE) # no shrinkage

pred.dda.train = predict(dda.mod, as.matrix(train.df[,-1]))
pred.dda.test = predict(dda.mod, as.matrix(test.df[,-1]))

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

lda.mod = lda(label ~ ., data = train.df)

pred.lda.train = predict(lda.mod, train.df)
pred.lda.test = predict(lda.mod, test.df)


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
sda_ranking = sda.ranking(as.matrix(train.df[,-1)]), train.df$label, 
                          diagonal=FALSE, fdr=TRUE, plot.fdr=FALSE, verbose=TRUE)
plot(sda_ranking, top=20)

err.var_da=numeric(0)
for(top in 3:80){
  # top=20
  sda_feat_select = sda_ranking[1:top,1]
  lda_selec.mod = sda(as.matrix(train.df[,c(sda_feat_select)]), train.df$label)
  lda_selec.pred = predict(lda_selec.mod, as.matrix(test.df[,c(sda_feat_select)]))
  err.var_da = c(err.var_da, sum(lda_selec.pred$class!=test.df$label) / nrow(test.df))
}
plot(err.var_da, type="l", xlab="Nombre variables", ylab="error")

sda_feat_select = sda_ranking[1:43,1]

lda_shrink.mod = sda(as.matrix(train.df[,-1]), train.df$label)
lda_selec.mod = sda(as.matrix(train.df[,c(sda_feat_select)]), train.df$label)
lda_selec_shrink.mod
summary(lda.mod)

lda_shrink.pred = predict(lda_shrink.mod, as.matrix(test.df[,-1]))
lda_selec.pred = predict(lda_selec.mod, as.matrix(test.df[,c(sda_feat_select)]))
error.lda.df[1,] = error.da.df[2,"error_test"]#lda total 
error.lda.df[2,] = 0 #lda PCA
error.lda.df[3,] = 0 #lda ICA
error.lda.df[4,] = sum(lda_selec.pred$class!=test.df$label) / nrow(test.df) #lda selec

cat("LDA_shrink Test error:", sum(lda_shrink.pred$class!=test.df$label) / nrow(test.df))
cat("LDA_selec Test error:", sum(lda_selec.pred$class!=test.df$label) / nrow(test.df))


# ROC curve (For LDA + DDA)
pred.lda = prediction(pred.lda.test$x[,1], test.df$label)
perf.lda = performance(pred.lda, measure = "tpr", x.measure = "fpr") 
plot(perf.lda, col="red") # PLOT
plot(perf.dda, add=TRUE, col="blue")
performance(pred.lda, measure="auc") #AUC


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# QDA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qda.mod = qda(label ~ ., data = train.df)

pred.qda.train = predict(qda.mod, train.df)
pred.qda.test = predict(qda.mod, test.df)


error.da.df[3,] = c(sum(pred.qda.train$class!=train.df$label) / nrow(train.df),
                 sum(pred.qda.test$class!=test.df$label) / nrow(test.df))
cat("QDA Train error:",error.da.df[3,"error_train"])
cat("QDA Test error:",error.da.df[3,"error_test"])

# matrius de covariàncies per estudiar-ne el condicionament
kappa(cov(as.matrix(train.df[,-1])))