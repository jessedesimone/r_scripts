# ---------------------------------------
# Calculate power one sample
library(pwr)

# Proportions
p0 <- 0.8              # Null hypothesis proportion
p1 <- 0.9              # Observed or expected proportion
n <- 126                # Sample size
alpha <- 0.025

# Effect size (Cohen's h for proportions)
h <- ES.h(p1, p0)

# Calculate power
pwr.p.test(n = n, h = h, sig.level = alpha,
           alternative = "greater")


# ---------------------------------------
# Calculate required sample size given power one sample
# Load pwr package
library(pwr)

# Define proportions
p0 <- 0.8              # Null hypothesis proportion
p1 <- 0.9             # Alternative proportion
alpha <- 0.025
power <- 0.8

# Calculate effect size for proportions
h <- ES.h(p1, p0)

# Power calculation for one-sample proportion test
pwr.p.test(h = h, sig.level = alpha, power = power,
           alternative = "greater")


# ---------------------------------------
# Calculate power two sample
# Parameters
n_test <- 126        # number of relevant test samples (e.g., positives for sensitivity)
p_null <- 0.80      # null hypothesis value (e.g., current expected sensitivity)
p_alt  <- 0.90      # alternative hypothesis value (what you hope to detect)
alpha  <- 0.025     # one-sided significance level

# Run power analysis
result <- power.prop.test(n = n_test,
                          p1 = p_null,
                          p2 = p_alt,
                          sig.level = alpha,
                          alternative = "one.sided")

# Print result
print(result)
