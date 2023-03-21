#module to run stepwise regression model

library(MASS)
df = read.csv(file = '/path/to/csv')
full.model <- lm(<response var> ~ <pred1> + <pred2> + <pred n>, data = df)
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
                     