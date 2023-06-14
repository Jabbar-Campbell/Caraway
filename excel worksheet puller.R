library(readxl)
library(tidyverse)


x<-read_xlsx(path = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/temp/20210401_24s Hydroxycholesterol ELISA.xlsx", sheet = 1)
y<-read_xlsx(path = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/temp/20210401_24s Hydroxycholesterol ELISA.xlsx", sheet = 2)
z<-read_xlsx(path = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/temp/20210401_24s Hydroxycholesterol ELISA.xlsx", sheet = 3)



my_list<-c(1:3)


ALL<-lapply(my_list, function(i){
  df<-read_xlsx(path = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/temp/20210401_24s Hydroxycholesterol ELISA.xlsx", sheet = i)
  df<-df[c(83:155),c(1:5)]
  df$attempt<-paste0("Attempt","_",i)
  df
})




ALL[3]


#ALL<-unlist(ALL, recursive = FALSE)

ALL2<-do.call("rbind",ALL) 


colnames(ALL2)<-c("Row","Column","Sample","Value","Dilution", "Attempt")

ALL2<-as.data.frame(ALL2)
ALL2<-filter(ALL2, Sample != "blank" ) 
ALL2<-separate(ALL2, col = Dilution, into = c("Dilution", "times"), sep = "x")
  

ALL2$times<-NULL



  mutate(ALL2, times_DF = as.numeric(Dilution) * as.numeric(Value))

  
  unique(ALL2$Sample)
  
  
  ggplot(ALL2, mapping = aes(x=interaction(Dilution, Attempt),y=as.numeric(Value),
                             color = Sample,
                             size = .5,
                             alpha= .5))+
  geom_point()+
  geom_label(data = filter(ALL2, Sample %in% c("p- 1","p- 2","p- 3","p- 4","p- 5")), aes(label = Sample))+#label to use
  geom_jitter(width = .3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ylab(label = "Value")+
  xlab(label = "Dilutions")+
  labs(title = "Samples 1 thru 25,  at Different Dilutions")
  

  
  