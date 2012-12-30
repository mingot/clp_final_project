#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Visualization
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Analisi exploratoria. Objectiu: veure relacio entre variables i la seva relacio
# amb la variable objectiu. Procediment: visualitzacio de variables 2 a 2.

library(ggplot2) # ggplot2, qplot
library(GGally) # ggpairs
library(ellipse) # plotcorr


mostra.df = f_pe[sample(1:nrow(f_pe),1000),]
names(mostra.df) = feat_names_short

# Scatter plot de parelles de variables senzill
pairs(mostra.df[,c(2,18:21,23,29)])

# Mesura de l'autocorrelacio entre variables mitjancant elipses
plotcorr(as.matrix(cor(mostra.df[,2:50])))

# Plot de la matriu d'autocorrelació segons intensitats
plot.sociomatrix(cor(mostra.df[,2:ncol(mostra.df)]), drawlab=FALSE, diaglab=FALSE)

ggpairs(mostra.df[,c(1,v_top)], colour='label')


# Comparacio de funcions de dencsitat per una variable donada
names(mostra.df) = feat_names_short
qplot(V14, fill=label, data=mostra.df) + facet_wrap(~label, ncol=2)
ggplot(mostra.df, aes(x=V14, fill=label)) + geom_density(alpha=.3) 
