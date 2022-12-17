'''
Module for checking and installing missing packages

option 1 source: https://thiyanga.netlify.app/post/multiplepkg/
option 2 source: https://gist.github.com/stevenworthington/3178163
'''
# Option 1
list.of.packages <- c("gganimate", "tidyverse", "gapminder")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Option 2
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("ggplot2", "plyr", "reshape2", "RColorBrewer", "scales", "grid")
ipak(packages)