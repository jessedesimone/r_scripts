#non-parametric permutation testing
#anova model with pairwise comparisons

library(lmPerm)
library(multcomp)

#read data
df<-read.csv('/path/to/data.csv')

#fit anova model
mod<- aovp(<response var> ~ <grouping var> + <cov1> + <cov2> + <cov3> , data = df, perm="Exact", seqs=FALSE, center=TRUE, projections = FALSE)
summary(mod, type= "III")

#pairwise comparisons with FDR correction
PT = pairwisePermutationTest(<response var> ~ <grouping var> + <cov1> + <cov2> + <cov3> ,data = df, method="fdr"); PT
