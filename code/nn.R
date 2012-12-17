nn.mod = nnet(label ~ . -PatientID, data = train.df, size=5)
summary(nn.mod)
sum(nn.mod$residuals)/nrow(train.df)
pred = predict(nn.mod, test.df)
error = abs(pred - test.df$label)
sum(error)/nrow(test.df)

colisio!!