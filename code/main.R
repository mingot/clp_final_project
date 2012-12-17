library(kernlab)
library(nnet)
library(rpart)
library(ggplot2)
library(car)
library(GGally)
library(ellipse)
library(sm)
library(lattice)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f_pe = read.table("data/KDDPETrain.txt",  header=FALSE, strip.white=TRUE)
feat_names = read.table("data/KDDPEfeatureNames.txt")
names(f_pe) = gsub(' ', '_', tolower(feat_names$V1)) # Substituim els espais per underscores en els noms de les variables
names(f_pe)[2]="label"

f_pe[f_pe$label!=0,"label"] = 1 #Posem labels a 1 de cada candidat
f_pe$label = factor(f_pe$label)
set.seed(1) #fixem seed per poder reproduir les simulacions amb la mateixa particio.
rows = sample(1:nrow(f_pe),round(0.7*nrow(f_pe))) # separacio aleatoria entre train i test: 70% i 30%
train.df = f_pe[rows,]
test.df = f_pe[-rows,]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Visualization
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Analisi exploratoria. Objectiu: veure relacio entre variables i la seva relacio
# amb la variable objectiu


# Varies representacions de la matriu de covariances

# Scatter plot de parelles de variables senzill
pairs(train.df[,c(2,5:10)])

# Mesura de l'autocorrelacio entre variables mitjancant elipses
plotcorr(as.matrix(cor(train.df[,10:30])))


scatterplotMatrix(~spatial_shape_feature_1 + spatial_shape_feature_2 + spatial_shape_feature_3|factor(label), 
                  data=train.df)


# eina definitiva per l'analisi exploratoria de parelles de variables
ggpairs(train.df[,c(2,8:11)],color='label', alpha=0.4)

# # Diamonds example of the use of ggpairs
# pm <- ggpairs(
#   diamonds[,1:3],
#   upper = list(continuous = "density", combo = "box"),
#   lower = list(continuous = "points", combo = "dot"),
#   color = "cut",
#   title = "Diamonds"
# )
# pm


# Comparacio de funcions de dencsitat
histogram(~V23|label, data=train.df)
qplot(V23, fill=label, data=train.df) + facet_wrap(~label, ncol=2)
ggplot(train.df, aes(x=V23, fill=label)) + geom_density(alpha=.3) 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Model Training
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Pretenen donar una visio de la influencia de cada variable


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
