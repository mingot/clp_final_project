library(kernlab)
library(nnet)
library(rpart)
library(ggplot2)
library(car)
library(GGally)
library(ellipse)
library(lattice)
rm(list=ls())

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Loading
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f_pe = read.table("data/KDDPETrain.txt",  header=FALSE, strip.white=TRUE)

# Canviar el nom de les variables del data frame
feat_names = read.table("data/KDDPEfeatureNames.txt")
feat_names = feat_names$V1
feat_names = gsub(' ', '_', tolower(feat_names))# Substituim els espais per underscores en els noms de les variables
feat_names_short = names(f_pe) # guardem les velles pq son curtes i ens poden servir
feat_names_short[1:5]=c("PatientID","label","x","y","z")
names(f_pe) = feat_names

f_pe[f_pe$label!=0,"label"] = 1 # posem labels a 1 de cada candidat
f_pe$label = factor(f_pe$label) # fiquem com a factor


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Visualization
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Analisi exploratoria. Objectiu: veure relacio entre variables i la seva relacio
# amb la variable objectiu

mostra.df = f_pe[sample(1:nrow(f_pe),1000),]
names(mostra.df) = feat_names_short

# Scatter plot de parelles de variables senzill
pairs(mostra.df[,c(2,5:10)])

# Mesura de l'autocorrelacio entre variables mitjancant elipses
plotcorr(as.matrix(cor(mostra.df[,10:30])))

names(mostra.df) = feat_names
scatterplotMatrix(~spatial_shape_feature_1 + spatial_shape_feature_2 + spatial_shape_feature_3|factor(label), 
                  data=mostra.df)


# eina definitiva per l'analisi exploratoria de parelles de variables
ggpairs(mostra.df[,c(2,8:11)],color='label', alpha=0.4)

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
names(mostra.df)=feat_names_short
qplot(V23, fill=label, data=mostra.df) + facet_wrap(~label, ncol=2)
ggplot(mostra.df, aes(x=V23, fill=label)) + geom_density(alpha=.3) 



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# K-fold cross-validation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Funció per construir els index del k-fold cross-validation

# Primera partició entre train-test
set.seed(1) #fixem seed per poder reproduir les simulacions amb la mateixa particio.
rows = sample(1:nrow(f_pe),round(0.7*nrow(f_pe))) # separacio aleatoria entre train i test: 70% i 30%
train.df = f_pe[rows,]
test.df = f_pe[-rows,]


# Particio en 5 subconjunts de k-fold
f_K_fold <- function(Nobs,K=5){
  rs <- runif(Nobs)
  id <- seq(Nobs)[order(rs)]
  k <- as.integer(Nobs*seq(1,K-1)/K)
  k <- matrix(c(0,rep(k,each=2),Nobs),ncol=2,byrow=TRUE)
  k[,1] <- k[,1]+1
  l <- lapply(seq.int(K),function(x,k,d) 
    list(train=d[!(seq(d) %in% seq(k[x,1],k[x,2]))],
         test=d[seq(k[x,1],k[x,2])]),k=k,d=id)
  return(l)
}

index_cv = f_K_fold(nrow(f_pe),5)






