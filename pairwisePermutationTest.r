#non-parametric pairwise permutation test
#The pairwisePermutationTest function in the rcompanion package conducts 
#permutation tests across groups in a pairwise manner

#import packages 
library(rcompanion)

#read data
df<-read.csv('/path/to/data.csv')

#pairwise comparisons with FDR correction
PT = pairwisePermutationTest(<response var> ~ <grouping var> + <cov1> + <cov2> + <cov3> ,data = df, method="fdr"); PT