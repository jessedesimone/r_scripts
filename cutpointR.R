#install packages
#install.packages("cutpointr")

#load required packages
library("readxl")
library("cutpointr")

#input data
data<-read.csv("/path/to/infile.csv")
head(data)

#determine cutpoint
cp <- cutpointr(data, ab_ratio, pet_outcome, 
                direction ="<=", 
                method = maximize_metric, 
                metric = sum_sens_spec)
summary(cp)
plot(cp)

#optimize
opt_cut <- cutpointr(data, plasma, pet, 
                     direction = "<=", 
                     pos_class = "positive",
                     neg_class = "negative", 
                     method = maximize_metric, 
                     metric = youden,
                     boot_runs = 1000)
summary(opt_cut)
plot_metric(opt_cut)

#calculate bootstrap confidence interval
boot_ci(opt_cut, optimal_cutpoint, in_bag = FALSE, alpha = 0.05)        #cutpoint
boot_ci(opt_cut, AUC, in_bag = TRUE, alpha = 0.05)      #AUC
