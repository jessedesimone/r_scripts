#install packages
install.packages(c('tibble', 'dplyr'))
library(tibble)
library(dplyr)

#vector of unadjusted p-values
p.raw=c(0.005, 0.001, 0.02, 0.91, 0.005, 0.0013)

#variable names
reg<-c("a", "b", "c", "d", "e", "f")  

#create unadjusted dataframe; sorted lowest to highest
df<-data.frame(reg,p.raw)
names(df)<-c("Region", "rawP")
df = df[order(df$rawP),]
df

#BH adjustment; append to df; sorted lowest to highest
df$BH =
  p.adjust(df$rawP,
           method = "BH")
df = df[order(df$BH),]

df2 = df %>% mutate(Sig = case_when(BH <= 0.05 ~ "*"))
df2

#Print output to text file
sink('/path/to/outfile.txt', append=TRUE)
cat("# FDR correction\n\n")
df
sink()