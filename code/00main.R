library(ggplot2) # ggplot2, qplot
library(GGally) # ggpairs
library(ellipse) # plotcorr
library(sna) # plot.sociomatrix
library(caret) # findCorrelation
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
# K-fold cross-validation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Funció per construir els index del k-fold cross-validation

# Primera partició entre train-test
set.seed(1) #fixem seed per poder reproduir les simulacions amb la mateixa particio.
rows = sample(1:nrow(f_pe),round(0.7*nrow(f_pe))) # separacio aleatoria entre train i test: 70% i 30%
train.df = f_pe[rows,]
test.df = f_pe[-rows,]


# Particio en 5 subconjunts de k-fold
f_K_fold = function(Nobs,K=5){
  rs = runif(Nobs)
  id = seq(Nobs)[order(rs)]
  k = as.integer(Nobs*seq(1,K-1)/K)
  k = matrix(c(0,rep(k,each=2),Nobs),ncol=2,byrow=TRUE)
  k[,1] = k[,1]+1
  l = lapply(seq.int(K),function(x,k,d) 
    list(train=d[!(seq(d) %in% seq(k[x,1],k[x,2]))],
         test=d[seq(k[x,1],k[x,2])]),k=k,d=id)
  return(l)
}

index_cv = f_K_fold(nrow(f_pe),5)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Visualization
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Analisi exploratoria. Objectiu: veure relacio entre variables i la seva relacio
# amb la variable objectiu

mostra.df = f_pe[sample(1:nrow(f_pe),1000),]
names(mostra.df) = feat_names_short

# Scatter plot de parelles de variables senzill
pairs(mostra.df[,c(2,18:21,23,29)])

# Mesura de l'autocorrelacio entre variables mitjancant elipses
plotcorr(as.matrix(cor(mostra.df[,3:10])))

# Plot de la matriu d'autocorrelació segons intensitats
plot.sociomatrix(cor(mostra.df[,4:ncol(mostra.df)]), drawlab=FALSE, diaglab=FALSE)

# Borrem variables autocorrelacionades
dat = mostra.df[-c(1:2)]
corFeat = findCorrelation(cor(dat), cutoff=0.8, verbose=TRUE)
mostra.df = mostra.df[,-(corFeat+2)]

ggpairs(mostra.df, columns=10:15)

# Comparacio de funcions de dencsitat per una variable donada
names(mostra.df)=feat_names_short
qplot(V23, fill=label, data=mostra.df) + facet_wrap(~label, ncol=2)
ggplot(mostra.df, aes(x=V23, fill=label)) + geom_density(alpha=.3) 
