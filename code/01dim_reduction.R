#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Reducció de dimensionalitat
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Objectiu: donar un subconjunt de variables significatives per 
# entrenar els classificadors. Aquest subconjunt pot ser donar
# o bé mitjançant PROJECCIONS en subespais (PCA, ICA, MDA) o bé 
# SELECCIONS de quines són les variables més significatives del conjunt 
# total donat.

library(MASS) #setpAIC


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PROECCIONS (Biel)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# PCA (Biel)

# ICA (Biel)

# MDA (Biel)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SELECCIONS (Mingot)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Segons regressió logistica (tipo lineal)
glm.mod = glm(label ~ .,data = train.df, family=binomial)
summary(glm.mod)

step = stepAIC(glm.mod, direction="both") #stepwise selection
step$anova # display results

