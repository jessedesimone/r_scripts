#run partial spearman's correlation while adjusting for covariates

install.packages("PResiduals")
library(PResiduals)

df=read.csv('/Users/jessedesimone/Desktop/correlations.csv')
head(df)
colnames(df)

#set factors
df$sex=as.factor(df$sex)
df$apoe=as.factor(df$apoe)
attach(df)

#set seed
set.seed(123)

#spearman correlation
#To compute the partial Spearman's rank correlation between X and Y adjusting for Z, formula is specified as X | Y ~ Z. This indicates that models of X ~ Z and Y ~ Z will be fit.

#run single test
partial_Spearman(x_var|y_var ~ age + sex + educ + apoe, data=df)

#run tests in loop for multiple x variables
y_var<-composite_wm_fat     #define y response variable
x_var_list<-list(ab42_40, tau, ptau, nfl, gfap, centiloid)  #specify list of x predictor variables
for (i in x_var_list){
    ps<-partial_Spearman(i|y_var ~ age + sex + educ + apoe, data=df)
    print(ps)
}
