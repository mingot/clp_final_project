

# PCA sobre totes les variables menys les de la posició (no normalitzades)
pe.pca = princomp(f_pe[,-c(1:4)], scores=TRUE)
summary(pe.pca)

# Plot de cada component i la seva desviacio estandard 
plot(apply(pe.pca$scores[,1:20],2,sd), xlab="Component", ylab="Standard Deviation")

# Plot del ggpairs de les n components principals 
n = 4
data_pca.df = f_pe
data_pca.df[,-c(1:4)] = pe.pca$scores 
names(data_pca.df)[5:ncol(data_pca.df)] = paste("comp", 1:(ncol(data_pca.df)-4), sep="")
ggpairs(data_pca.df[,c(1,2:4,5:(5+n))], colour="label")