cdir = getwd()
setwd("./tex")
Sweave("memoria.Rnw")
setwd(cdir)