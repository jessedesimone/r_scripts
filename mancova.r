#module to run mancova analysis in R

library(jmv)
manc1<-mancova(data=df, deps=vars(<var1>, <var2> , <var n>) + 
               factors = <grouping variable>, covs = vars(<cov1>, <cov2>, <cov n>),  boxM = TRUE)
manc1