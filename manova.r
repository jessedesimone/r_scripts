#module to run manova in R

dep_vars <- cbind(<pred1> + <pred2> + <pred n>)
man1 <- manova(dep_vars ~ <response variable factor> + data = df); summary(man1)