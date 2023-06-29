#import packages
library(car)
library(dplyr)


#set seed
sed.seed(123)

#import data
df = read.csv('/path/to/data.csv')

#summarize data
summary(df)
data %>%
    group_by(<group>) %>%  
    summarise(mean_<variable> = mean(<variable 1>),
    sd_<variable 1> = sd(<variable 1>),
    mean_<variable 2> = mean(variable 2),
    sd_<variable 2> = sd(variable 2))

#boxplot
boxplot(<response variable> ~ <group>,
data = df,
main = "Response by Group",
xlab = "Group",ylab = "Score",
col = "red",border = "black")

#test assumptions
#independence (between covariate and treatment)
#independence satisfied if p > .05
model <- aov(<covariate> ~ <group>, data = df)
summary(model)

#homogeneity of variance
#variance assumption satisfied if p > .05
leveneTest(<response variable> ~ <group>, data = data)

#fit ancova model
mod1<- aov(<response varibale> ~ <group> + <covariate>, data = df); summary(mod1, type= "III")

#Tukey post-hoc contrasts
ph1 <- glht(mod1, linfct = mcp(<group> = "Tukey")); summary(ph1)





