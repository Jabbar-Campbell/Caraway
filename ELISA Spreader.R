
#This will take a table where where rows 1 and 2 are rep 1 and 2 respectively
#and spreads them out side by side


library(tidyr)
spread<-read.csv(file = 'clipboard', sep = "\t")


spreader<-pivot_wider(data= spread, id_cols = NULL, names_from =c(treatment) ,values_from = c("Ctl"))


write.table(spreader,file = 'clipboard', sep = "\t", row.names = FALSE)
