library(nnet)
library(ROCR)

nn.mod = nnet(label ~ . -patientid, data = train.df, size=5)
summary(nn.mod)
sum(nn.mod$residuals)/nrow(train.df)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Estudi del error
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Roc curve
pred = predict(nn.mod, test.df)
pred.roc = prediction(pred, test.df$label)
perf = performance(pred.roc, measure="tpr", x.measure="fpr" )
plot(perf, col=rainbow(10))

# AUC
performance(pred.roc, measure="auc")

# Classification Error
error = abs(pred - test.df$label)
sum(error)/nrow(test.df)
