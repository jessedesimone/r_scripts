#install packages
#install.packages("cutpointr")

#load required packages
library("readxl")
library("cutpointr")

#input data
data<-read_excel("/path/to/infile.xlsx")
head(data)

#determine cutpoint
cp <- cutpointr(data, ab_ratio, pet_outcome, 
                direction ="<=", 
                method = maximize_metric, 
                metric = sum_sens_spec)
summary(cp)
plot(cp)

#optimize
opt_cut <- cutpointr(data, ab_ratio, pet_outcome, 
                     direction = "<=", 
                     pos_class = "positive",
                     neg_class = "negative", 
                     method = maximize_metric, 
                     metric = youden)
plot_metric(opt_cut)
