#non-parametric permutation ANCOVA (aovp)
#use Tukey or another method for post-hoc comparisons
#aovp p values should be fdr corrected after running script

#import packages
library(lmPerm)
library(multcomp)

#set working directory
setwd('<path/to/working/directory/>')

set.seed(42)
#read data
df<-read.csv('aovp_input.csv')
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

# ---------single test option with repeat----------
# setwd('<path/to/working/directory/>')
# df<-read.csv('</path/to/infile>.csv')
# deps <- c("Amygdala_L_FW")
# x<-1
# repeat {
# for (i in df[deps]) {
#    mod<-aovp(i ~ grp_id + age + sex + apoe + educ , data = df, perm="Exact", seqs=FALSE,center=TRUE, projections = FALSE, maxIter=10000)
#    print(summary(mod, type="III"))
#    print(summary(mod, type="III")[[1]][1,5])
#    ph<-glht(mod, linfct = mcp(grp_id = "Tukey"))
#    print(summary(ph))
#    sum_test = unlist(summary(ph))
#    print(sum_test$test.pvalues1)
#    print(sum_test$test.pvalues2)
#    print(sum_test$test.pvalues3)
# }
# x<-x+1
# if (x==4){
#    break
# }
# }

#----------loop for multiple tests no repeat---------
#this option will output summary word and excel documents
#example below for 96 different regions starting at column 6 (columns 1-5 of dataframe are predictor variables)

6:ncol(df)         #check number of columns/tests to run, n specifies first column of variables to be tested; convariate factors should be placed before the first variable 
pvals<-rep(NA,96)  #create empty table with pvals for number of tests to be run; should match number of variables
sink ('aovp_output.doc')        #specify output doc file
#create empty lists
variable_list=list()
aovp_list=list()
tukey_list_p1=list()
tukey_list_p2=list()
tukey_list_p3=list()
for (i in 6:ncol(df)){
    column<-names(df[i])
    print(column)
    variable_list<-append(variable_list, column)        #append variable name to list

    mod<-aovp(df[,i] ~ grp_id + age + sex + apoe + educ , data = df, perm="Exact", seqs=FALSE,center=TRUE, projections = FALSE)
    pvals<-summary(mod)
    print(pvals)      #print the aovp summary to output doc
    aovp_pval<-summary(mod, type="III")[[1]][1,5]     #get pvalue for group effect only
    aovp_list<-append(aovp_list, aovp_pval)       #append pvalue to list
    
    tk<-summary(glht(mod, linfct = mcp(grp_id = "Tukey")))
    print(tk)
    sum_test = unlist(summary(tk))
    p1<-sum_test$test.pvalues1
    p2<-sum_test$test.pvalues2
    p3<-sum_test$test.pvalues3
    #append tukey pvalues to lists
    tukey_list_p1<-append(tukey_list_p1, p1)
    tukey_list_p2<-append(tukey_list_p2, p2)
    tukey_list_p3<-append(tukey_list_p3, p3)
}
sink()

#combine lists into final dataframe
my_nested_list<-list(roi=variable_list, 
p_unc=aovp_list, 
a_vs_b=tukey_list_p1,
a_vs_c=tukey_list_p2,
b_vs_c=tukey_list_p3
)
df_final <-  as.data.frame(do.call(cbind, my_nested_list))
print(df_final)


#----------loop for multiple tests---------
#repeat test for each variable 10 times
#use the average main effect as the output uncorrected p value
#preferred option because aovp will give a different p-value each time
#taking the average provides better confidence the effect is real
#example below for 96 different regions starting at column 6 (columns 1-5 of dataframe are predictor variables)

6:ncol(df)         #check number of columns/tests to run, n specified first column of variables to be tested 
pvals<-rep(NA,96)  #create empty table with pvals for number of tests to be run
#create empty lists
variable_list=list()
aovp_list=list()
tukey_list_p1=list()
tukey_list_p2=list()
tukey_list_p3=list()
for (i in 6:ncol(df)){
    column<-names(df[i])
    print("******************")
    print(column)
    variable_list<-append(variable_list, column)        #append variable name to list

    #repeat the aovp model n=10 times for each variable
    #take the average main effect as the final p-value
    t=0         #set counter
    aovp_tmp=list()
    #run model repeat times
    repeat {
        mod<-aovp(df[,i] ~ grp_id + age + sex + apoe + educ , data = df, perm="Exact", seqs=FALSE,center=TRUE, projections = FALSE)
        print(summary(mod, type="III"))
        p_tmp<-summary(mod, type="III")[[1]][1,5]       #extract main effect from aovp summary
        aovp_tmp<-append(aovp_tmp, p_tmp)       #append temp pvals to list
        t<-t+1      #update counter
        if (t==10){
            break
        }
    }
    print("*** temp p-vals ***")
    df_tmp<-do.call(rbind.data.frame, aovp_tmp)     #convert tmp pval list to dataframe
    colnames(df_tmp)<-c("tmp_pvals")        #change column name
    print(df_tmp)
    aovp_pval<-mean(df_tmp$tmp_pvals)      #compute mean of tmp_pvals
    print("***mean p-value****")
    print(aovp_pval)
    aovp_list<-append(aovp_list, aovp_pval)     #append mean pval to aovp_list

    #perform Tukey contrasts
    #note this is only performing tukey contrasts for the last model run
    #Tukey less likely to have major changes from model to model
    tk<-summary(glht(mod, linfct = mcp(grp_id = "Tukey")))
    sum_test = unlist(summary(tk))
    p1<-sum_test$test.pvalues1
    p2<-sum_test$test.pvalues2
    p3<-sum_test$test.pvalues3
    tukey_list_p1<-append(tukey_list_p1, p1)
    tukey_list_p2<-append(tukey_list_p2, p2)
    tukey_list_p3<-append(tukey_list_p3, p3)
}

#combine lists into final dataframe
my_nested_list<-list(roi=variable_list, 
p_unc=aovp_list, 
a_vs_b=tukey_list_p1,
a_vs_c=tukey_list_p2,
b_vs_c=tukey_list_p3
)
df_final <-  as.data.frame(do.call(cbind, my_nested_list))
print(df_final)

#export dataframe
df_final<-apply(df_final,2,as.character)
write.csv(df_final, file='aovp_output.csv', row.names=FALSE)