

library(randomForest)
library(ROCR)

# Initial RF
rf.mod = randomForest(label~., data=train.df, mtry=8)
rf.pred = predict(rf.mod, test.df, type="prob")[,2]
rf.err = sum(abs(rf.pred - as.numeric(test.df$label)+1))/nrow(test.df)

# Roc curve
pred.roc = prediction(rf.pred, test.df$label)
perf = performance(pred.roc, measure="tpr", x.measure="fpr" )
plot(perf, col=rainbow(10))

# Perf
cat("AUC:", as.numeric(performance(pred.roc, measure="auc")@y.values))
cat("Error rate:", rf.err)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CV
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#cross-validation per l'error i AUC
pred.cv = numeric(0)
real.cv = numeric(0)
err_tot = numeric(0)
for(i in 1:5){
  rf.mod = randomForest(label~., data = data.df[index_cv[[i]]$train,], mtry=8)
  pred = head(predict(rf.mod, data.df[index_cv[[i]]$test,], type="prob")[,2],607)
  pred.cv = cbind(pred.cv, pred)
  real.cv = cbind(real.cv, as.numeric(head(data.df[index_cv[[i]]$test,"label"], 607))-1)
  err_tot = c(err_tot, sum(abs(pred.cv[,ncol(pred.cv)] - real.cv[,ncol(real.cv)]))/607)
}
pred.rf.roc = prediction(pred.cv, real.cv)#AUC
perf.rf = performance(pred.rf.roc, measure="tpr", x.measure="fpr")
plot(perf.rf, colorize=TRUE)
auc.vec = as.numeric(performance(pred.rf.roc, measure="auc")@y.values)
cat("auc:", mean(auc.vec))
cat("error:", mean(err_tot))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Nombre optim de variables a seleccionar per arbre
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Trobar nombre optim de neurones capa intermitja a travaes del parametre de regularització
err.var = numeric(0)
nvar.vec = 10:20
for(nvar in nvar.vec){ # decay parameter sweep
  err.var.v = numeric(0)
  for(i in 1:4){ # CV
    rf.mod = randomForest(label~., data = data.df[index_cv[[i]]$train,], mtry=nvar, ntree=30)
    pred = predict(rf.mod, data.df[index_cv[[i]]$test,], type="prob")[,2]
    err.var.v = c(err.var.v, sum(abs(pred-(as.numeric(data.df[index_cv[[i]]$test,"label"])-1)))/length(pred))
  }
  err.var = c(err.var, mean(err.var.v))
}
plot(nvar.vec, err.var, xlab="Nombre variables", ylab="error", type="l")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Nombre optim d'arbres
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

err.arb = numeric(0)
narb.vec = 50+1:10*2
for(narb in narb.vec){ # decay parameter sweep
  err.arb.v = numeric(0)
  for(i in 1:3){ # CV
    rf.mod = randomForest(label~., data = data.df[index_cv[[i]]$train,], ntree=narb, mtry=17)
    pred = predict(rf.mod, data.df[index_cv[[i]]$test,], type="prob")[,2]
    err.arb.v = c(err.arb.v, sum(abs(pred-(as.numeric(data.df[index_cv[[i]]$test,"label"])-1)))/length(pred))
  }
  err.arb = c(err.arb, mean(err.arb.v))
}
plot(narb.vec, err.arb, xlab="Nombre d'arbres", ylab="error", type="l")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Top var
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

topvar.df = data.frame(name = rownames(rf.mod$importance), gini=rf.mod$importance)
topvar.df = topvar.df[with(topvar.df, order(-MeanDecreaseGini)), ]
names(topvar.df) = c("variable", "Guany Gini")
topvar.df = topvar.df[1:10,]
rownames(topvar.df) = NULL