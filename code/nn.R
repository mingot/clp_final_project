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
# CV
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#cross-validation per l'error i AUC
pred.cv = numeric(0)
real.cv = numeric(0)
err_tot = numeric(0)
for(i in 1:5){
  nn.mod = nnet(label~., data = data.df[index_cv[[i]]$train,], size=8, decay=0.01)
  pred = head(predict(nn.mod, data.df[index_cv[[i]]$test,]),607)
  pred.cv = cbind(pred.cv, pred)
  real.cv = cbind(real.cv, as.numeric(head(data.df[index_cv[[i]]$test,"label"], 607))-1)
  err_tot = c(err_tot, sum(abs(pred.cv[,ncol(pred.cv)] - real.cv[,ncol(real.cv)]))/607)
}
pred.nn.roc = prediction(pred.cv, real.cv)#AUC
perf = performance(pred.nn.roc, measure="tpr", x.measure="fpr")
plot(perf, colorize=TRUE)
auc.vec = as.numeric(performance(pred.nn.roc, measure="auc")@y.values)
cat("auc:", mean(auc.vec))
cat("error:", mean(err_tot))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Nombre optim de neurones capa intermitja
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Trobar nombre optim de neurones capa intermitja a travaes del parametre de regularització
err.reg = numeric(0)
dec.vec = 10^(1:10-8)
for(dec in dec.vec){ # decay parameter sweep
  err.reg.v = numeric(0)
  for(i in 1:5){ # CV
    nn.mod = nnet(label~., data = data.df[index_cv[[i]]$train,], size=8, decay=dec)
    pred = predict(nn.mod, data.df[index_cv[[i]]$test,])
    err.reg.v = c(err.reg.v, sum(abs(pred-(as.numeric(data.df[index_cv[[i]]$test,"label"])-1)))/nrow(pred))
  }
  err.reg = c(err.reg, mean(err.reg.v))
}
plot(log10(dec.vec), err.reg, xlab="log(param_reg)", ylab="error", type="l")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PCA comp
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Estudi de l'error comés en funcio del nombre de components pricipals usades 
err.comp = numeric(0)
ncomp.vec = 1:10
for(ncomp in ncomp.vec){
  err.comp.v = numeric(0)
  for(i in 1:5){ # CV
    nn.mod = nnet(label~., data = data_pca.df[index_cv[[i]]$train,1:(4+ncomp)], size=8, decay=0.01)
    pred = predict(nn.mod, data_pca.df[index_cv[[i]]$test,1:(4+ncomp)])
    err.comp.v = c(err.comp.v, sum(abs(pred-(as.numeric(data_pca.df[index_cv[[i]]$test,"label"])-1)))/nrow(pred))
  }
  err.comp = c(err.comp, mean(err.comp.v))
}

plot(ncomp.vec, err.comp, type="l", xlab="nombre de components principals", ylab="error")


