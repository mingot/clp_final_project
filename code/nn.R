library(nnet)
library(ROCR)


# Initial NN
nn.mod = nnet(label ~ . , data = train.df, size=5, decay=1)
pred = predict(nn.mod, test.df)
cat("Error:", sum(abs(pred-(as.numeric(test.df$label)-1)))/nrow(test.df))

# Roc curve
pred = predict(nn.mod, test.df)
pred.roc = prediction(pred, test.df$label)
perf = performance(pred.roc, measure="tpr", x.measure="fpr" )
plot(perf, col=rainbow(10))

# AUC
performance(pred.roc, measure="auc")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Nombre optim de neurones capa intermitja
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Trobar nombre optim de neurones capa intermitja a travaes del parametre de regularització
err.reg=numeric(0)
x.lab = numeric(0)
for(i in 1:10){
  nn.mod = nnet(label ~ . , data = train.df, size=8, decay=10^(i-8))
  pred = predict(nn.mod, test.df)
  err.reg = c(err.reg,sum(abs(pred-(as.numeric(test.df$label)-1)))/nrow(test.df))
  x.lab = c(x.lab, 10^(i-5))
}
plot(log10(x.lab), err.reg, type="l", xlab="log(param_reg)", ylab="error")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CV
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#cross-validation per l'error i AUC
pred.cv=numeric(0)
real.cv=numeric(0)
for(i in 1:5){
  nn.mod = nnet(label~., data = f_pe[index_cv[[i]]$train,], size=8, decay=1)
  pred = head(predict(nn.mod, f_pe[index_cv[[i]]$test,]),607)
  pred.cv = cbind(pred.cv, pred)
  real.cv = cbind(real.cv, as.numeric(head(f_pe[index_cv[[i]]$test,"label"], 607))-1)
}
pred.nn.roc = prediction(pred.cv, real.cv)#AUC
perf = performance(pred.nn.roc, measure="tpr", x.measure="fpr")
plot(perf, colorize=TRUE)
performance(pred.nn.roc, measure="auc")

