#non-parametric permutation ANCOVA (aovp)
#use Tukey or another method for post-hoc comparisons

#import packages
library(lmPerm)
library(multcomp)

#read data
df<-read.csv('/path/to/data.csv')

#fit anova model
mod<- aovp(<response var> ~ <grouping var> + <cov1> + <cov2> + <cov3> , data = df, perm="Exact", seqs=FALSE, center=TRUE, projections = FALSE)
summary(mod, type= "III")

#tukey post hoc
TukeyHSD(mod, "<grouping variable>",  conf.level=.95)
