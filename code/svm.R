library(ROCR)


svm.mod = ksvm(label ~ . - PatientID, data = train.df, 
               type = "C-bsvc", 
               kernel = "rbfdot", 
               kpar = list(sigma = 1), 
               C = 10, prob.model = TRUE)
svm.mod

pred = predict(svm.mod, test.df)
error = test.df$label - pred