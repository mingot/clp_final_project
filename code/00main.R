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
names(f_pe) = feat_names

# Eliminem patientid com a variable
f_pe = f_pe[2:ncol(f_pe)]
feat_names_short[1:4]=c("label","x","y","z")
feat_names_short = feat_names_short[1:(length(feat_names_short)-1)]

f_pe[f_pe$label!=0,"label"] = 1 # posem labels a 1 de cada candidat
f_pe$label = factor(f_pe$label) # fiquem com a factor



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Noms de grups de variables
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
v_pos = 2:4 
v_spat_shape = 6:8
v_simp_intens = 13:16
v_neigh = 17:20
v_neigh_intens = 21:38
v_shape = 39:64
v_anatonical = 65:68
v_neigh_feat_tres = 69:70
v_intens_contrast = 71:75
v_shape_neigh = 76:109
v_dgfr = 111:117 
v_top = c(14,51,12,17,114,9,8) # variables més significatives segons CAT score de LDA

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
# Correlation clean
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Objectiu: limpiar les variables que estan autocorrelacionades
# localitzar quines ho estan.

# Borrem variables autocorrelacionades (segons cutoff d'autocorrelacio)
dat = f_pe[-1]
corFeat9 = findCorrelation(cor(dat), cutoff=0.9, verbose=FALSE) + 1
corFeat8 = findCorrelation(cor(dat), cutoff=0.85, verbose=FALSE) + 1
train.df = train.df[,-corFeat8]
test.df = test.df[,-corFeat8]


# Visualització de la matriu d'autocorrelacio
plot.sociomatrix(cor(f_pe[,-c(1,corFeat9)]), drawlab=FALSE, diaglab=FALSE)

# Debug de les variables eliminades amb quines es trobaven correlacionades
var_num = 61
corMat = cor(dat)
corMat[which(abs(corMat[,var_num-1])>0.9),var_num-1]
