library(kernlab)
library(nnet)
library(rpart)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f_pe = read.table("data/KDDPETrain.txt",  header=FALSE, strip.white=TRUE)
feat_names = read.table("data/KDDPEfeatureNames.txt")
names(f_pe) = feat_names$V1

f_pe[f_pe$label!=0,"label"] = 1
rows = sample(1:nrow(f_pe),round(0.7*nrow(f_pe)))
train.df = f_pe[rows,]
test.df = f_pe[-rows,]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data cleaning
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

summary(f_pe$spatialshapefeature1)



svm.mod = ksvm(label ~ . - PatientID, data = train.df, 
               type = "C-bsvc", 
               kernel = "rbfdot", 
               kpar = list(sigma = 1), 
               C = 10, prob.model = TRUE)
svm.mod

pred = predict(svm.mod, test.df)
error = test.df$label - pred

nn.mod = nnet(label ~ . -PatientID, data = train.df, size=5)
summary(nn.mod)
sum(nn.mod$residuals)/nrow(train.df)
pred = predict(nn.mod, test.df)
error = abs(pred - test.df$label)
sum(error)/nrow(test.df)



# grow tree
tree.mod = rpart(label ~ . - PatientID,
             method="class", data=train.df)

printcp(tree.mod) # display the results
plotcp(fit) # visualize cross-validation results
summary(tree.mod) # detailed summary of splits

# plot tree
plot(tree.mod, uniform=TRUE,
     main="Classification Tree for Kyphosis")
text(tree.mod, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree
post(fit, file = "c:/tree.ps",
     title = "Classification Tree for Kyphosis")
