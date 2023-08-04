#non-parametric permutation ANCOVA (aovp)
#use Tukey or another method for post-hoc comparisons

#import packages
library(lmPerm)
library(multcomp)

#set working directory
setwd('/Users/jessedesimone/Desktop/test/')

set.seed(42)
#read data
df<-read.csv('test_ancova.csv')
df$grp_id<-as.factor(df$grp_id)
df$sex<-as.factor(df$sex)
df$apoe<-as.factor(df$apoe)
attach(df)

#check factors
is.factor(df$grp_id)
is.factor(df$sex)
is.factor(df$apoe)
is.factor(df$educ)
is.factor(df$age)

#---------single test option----------
#deps <- c("Amygdala_L_FW")
#for (i in df[deps]) {
#    mod<-aovp(i ~ grp_id + age + sex + apoe + educ , data = df, perm="Exact", seqs=FALSE,center=TRUE, projections = FALSE)
#    print(summary(mod, type="III"))
#    print(summary(mod)[[1]][1,5])
#    ph<-glht(mod, linfct = mcp(grp_id = "Tukey"))
#    print(summary(ph))
#    sum_test = unlist(summary(ph))
#    print(sum_test$test.pvalues1)
#    print(sum_test$test.pvalues2)
#    print(sum_test$test.pvalues3)
#}

#----------loop for multiple tests---------
library(car)
6:ncol(df)
pvals<-rep(NA,96)  #create empty table with pvals for number of tests to be run
sink ('/Users/jessedesimone/Desktop/test/test.doc')
variable_list=list()
aovp_list=list()
tukey_list_p1=list()
tukey_list_p2=list()
tukey_list_p3=list()
for (i in 6:ncol(df)){
    column<-names(df[i])
    print(column)
    variable_list<-append(variable_list, column)        #get variable name

    mod<-aovp(df[,i] ~ grp_id + age + sex + apoe + educ , data = df, perm="Exact", seqs=FALSE,center=TRUE, projections = FALSE)
    pvals<-summary(mod)
    print(pvals)
    aovp_pval<-summary(mod)[[1]][1,5]       #get pvalue for group effect only
    aovp_list<-append(aovp_list, aovp_pval)
    
    tk<-summary(glht(mod, linfct = mcp(grp_id = "Tukey")))
    print(tk)
    sum_test = unlist(summary(tk))
    p1<-sum_test$test.pvalues1
    p2<-sum_test$test.pvalues2
    p3<-sum_test$test.pvalues3
    tukey_list_p1<-append(tukey_list_p1, p1)
    tukey_list_p2<-append(tukey_list_p2, p2)
    tukey_list_p3<-append(tukey_list_p3, p3)
}
sink()

#combine lists into final dataframe
my_nested_list<-list(roi=variable_list, 
pval=aovp_list, 
a_vs_b=tukey_list_p1,
a_vs_c=tukey_list_p2,
b_vs_c=tukey_list_p3
)
df_final <-  as.data.frame(do.call(cbind, my_nested_list))
print(df_final)

#export dataframe
df_final<-apply(df_final,2,as.character)
write.csv(df_final, file='df_final.csv', row.names=FALSE)

