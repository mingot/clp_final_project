library(rpart) # rpart
library(ROCR)


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