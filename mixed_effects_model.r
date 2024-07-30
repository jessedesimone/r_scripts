# Load necessary libraries
library(lme4)
library(dplyr)
library(ggplot2)

# Example data (replace with your actual data)
subject <- factor(rep(1:10, each=2))  # 10 subjects with 2 time points each
time <- factor(rep(c("baseline", "month24"), 10))
group <- factor(rep(c("Control", "Treatment"), each=10))  # 2 groups
brain_volume <- c(1500, 1490, 1480, 1470, 1505, 1495, 1488, 1478, 1510, 1500, 
                  1550, 1540, 1560, 1550, 1545, 1535, 1555, 1545, 1550, 1540)

# Create a data frame
data <- data.frame(subject, time, group, brain_volume)

# Fit the mixed-effects model with time and interaction
model <- lmer(brain_volume ~ time * group + (1|subject), data=data)
summary(model)

ggplot(data, aes(x=time, y=brain_volume, color=group, group=subject)) +
  geom_line() +
  geom_point() +
  facet_wrap(~group) +
  labs(title="Change in Brain Volume Over Time by Group",
       x="Time Point",
       y="Brain Volume")

# Diagnostic plots
par(mfrow=c(2,2))
plot(model)