install.packages("imager")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("EBImage")

library(reticulate)
library(stringr)
library(dplyr)
library(imager)
library(tidyr)
library(ggplot2)
library(EBImage)
library(tidyverse)
library(openxlsx)


Folders<-list.files(path ="C:/Caraway Project/INFORMATICS/spheroid/Stack 4", full.names = TRUE)
FILES<-list.files(path = Folders[1], pattern = "- DAPI", full.names = TRUE)

###take file names across folders
slice_meta<-lapply(Folders, function(i){
  slice_meta<-list.files(i, pattern = "DAPI", full.names = TRUE)
              as.data.frame(slice_meta)
  slice_meta
})

###takes on meta data to each filename
slice_meta<-lapply(slice_meta, function(i){
  meta<-i
  extra<-as.data.frame(meta) %>% 
    separate(., col = 1, into =c("Company","path","Experiment/indication","Object",
                                 "Magnification/date/level/row",
                                 "seperator",
                                 "well",
                                 "field",
                                 "wv",
                                 "UV",
                                 "seperator2","channel") , sep = " ") %>% 
    select(.,c(1:5,7,8,12)) %>% 
    separate(.,col= 'Magnification/date/level/row',into = c("Mag","Month","day","level", "Row"),sep= "_") %>% 
    separate(.,col= 'Experiment/indication',into = c("Experiment","Indication"),sep= "/") %>% 
    separate(.,col= well ,into = c("Well","x"),sep= "\\(") %>% 
    separate(.,col= channel ,into = c("Channel","y"),sep= "\\).") %>% 
    separate(.,col= Row ,into = c("Exp","Row"),sep= "/") %>%
    unite(., col='Path', c('Company', 'path'), sep='/') %>% 
    unite(., col='Path', c('Path', 'Experiment'), sep=' ') 

  slice_meta<-cbind(meta,extra)
  slice_meta
  
})






 

for (i in 1:dim(slice_meta[[1]])[1]) {


w<-slice_meta[[1]][i,1] #2000 slice
x<-slice_meta[[2]][i,1] #2500 slice
y<-slice_meta[[3]][i,1] #3000 slice
z<-slice_meta[[4]][i,1] #3500 slice


#()
Stack<-readImage(files = c(w,x,y,z),names = c("w","x","y","z"))
Stack<-Stack*5
#EBImage::display(Stack)#, method = "raster" ,all = TRUE)
 
 
#EBImage::display(Stack)
#regexpr(slice_meta[[1]][1,1], pattern = "4/")[1]
#substr(w, 1, regexpr(slice_meta[[1]][1,1], pattern = "4/")[1])
#slice_meta[[1]][1,10]
#slice_meta[[1]][1,11]


target<- paste0(substr(w, 1, regexpr(slice_meta[[1]][i,1], pattern = "4/")[1]),
      " output/",
      slice_meta[[1]][i,10],
      slice_meta[[1]][i,11],".tiff" )
 
 
EBImage::writeImage(Stack,target )

} 




#B-07 H-07 J-07 L-05 N-07 had to be removed since no objects where observed
 



##############################################################################################################
#Lets read the output data back in although we could have sused broom to nest the data before its written out#


FILES2<-list.files( path = substring(PATH, 1, gregexpr(pattern = "ut/",PATH)[[1]][1]+1),
                    pattern = "xlsx",
                    full.names = TRUE)
 
my_objects2<-lapply(FILES2, function(i){
                           #file_name <- i
                           df<-read.xlsx(xlsxFile =  i ,sheet = 1)
                           well<-strsplit(i, split = "_")[[1]][2] %>% strsplit(.,split = " ")
                           df$Well<-well[[1]] [2]
                           colnames(df)[1]<-"index"
                           df
  
})

my_objects2<-bind_rows(my_objects2)

###read in KEYs and merge
Treat_codes<-read.table(file = 'clipboard', header = TRUE)
colnames(Treat_codes)<-c("Row","04","05","06","07")
Treat_code<-gather(Treat_codes,key= "Well", value = "code",2:5)

Treatments<-read.table(file = 'clipboard', sep = "\t", header = TRUE)

Key<-merge(Treat_code, Treatments)
Key<-unite(Key, col = Well, sep = "", 'Row','Well')



#all objects in the water shed.....a new column in order to order the conentration later on in ggplot
my_objects3<-merge(my_objects2,Key)
my_objects3$Treatment2 = factor(my_objects3$Treatment, levels= str_sort(unique(my_objects3$Treatment), numeric = TRUE))
colnames(my_objects3)

AVG_DMSO_area<-my_objects3 %>% 
  #group_by(.,Well) %>% 
  filter(.,Treatment== "(DMSO only)") %>% 
  summarise(., Avg_DMSO_area = mean(my_objects3$s.area))

AVG_DMSO_peri<-my_objects3 %>% 
  #group_by(.,Well) %>% 
  filter(.,Treatment== "(DMSO only)") %>% 
  summarise(., Avg_DMSO_peri = mean(my_objects3$s.perimeter))

my_objects3<-my_objects3 %>% 
  merge(.,AVG_DMSO_area) %>% 
  merge(.,AVG_DMSO_peri)



str(my_objects3)

my_objects3<-my_objects3 %>% 
            mutate(., Area_Percent_DMSO= (my_object3$s.area/my_object3$Avg_DMSO_area)*100 ) %>% 
            mutate(., Peri_Percent_DMSO= (my_object3$s.perimeter/my_object3$Avg_DMSO_peri)*100 ) 
#ggplot(AVG_DMSO_peri,aes(x= Well,y= s.area, colour = Treatment),group = Treatment)+
#  geom_jitter(aes(size = 1.5, alpha = .3))+
#  theme_bw()+
#  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+labs(title ="Raw DMSO- Lableled objects only")


#Filter of just the obects that have been labeled
my_objects4 <- my_objects3 %>% 
  filter(.,my_objects3$s.area > 600 & my_objects3$s.area < 10000)


#RAW Total objects
ggplot(my_objects3,aes(x= Treatment2,y= s.area, colour = Treatment),group = Treatment)+
  geom_jitter(aes(size = 1.5, alpha = .3))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+labs(title ="Raw Area- Total objects ")


ggplot(my_objects3,aes(x= Treatment2,y= s.perimeter, colour = Treatment),group = Treatment)+
  geom_jitter(aes(size = 1.5, alpha = .3))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+labs(title ="Raw Perimeter- Total objects")





#RAW labeled objects
ggplot(my_objects4,aes(x= Treatment2,y= s.area, colour = Treatment),group = Treatment)+
  geom_jitter(aes(size = 1.5, alpha = .3))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+labs(title ="Raw Area- Lableled objects only")
  

ggplot(my_objects4,aes(x= Treatment2,y= s.perimeter, colour = Treatment),group = Treatment)+
  geom_jitter(aes(size = 1.5, alpha = .3))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+labs(title ="Raw Perimeter- Labeled objects only")



##PERCENT OF CONTROL Total objects
ggplot(my_objects3,aes(x= Treatment2,y= Area_Percent_DMSO, colour = Treatment),group = Treatment)+
  geom_jitter(aes(size = 1.5, alpha = .3))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+labs(title ="% DMSO Area- Lableled objects only")


ggplot(my_objects3,aes(x= Treatment2,y= Peri_Percent_DMSO, colour = Treatment),group = Treatment)+
  geom_jitter(aes(size = 1.5, alpha = .3))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+labs(title ="% DMSO Perimeter- Labeled objects only")







##PERCENT OF CONTROL labeled objects
ggplot(my_objects4,aes(x= Treatment2,y= Area_Percent_DMSO, colour = Treatment),group = Treatment)+
  geom_jitter(aes(size = 1.5, alpha = .3))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+labs(title ="% DMSO Area- Lableled objects only")


ggplot(my_objects4,aes(x= Treatment2,y= Peri_Percent_DMSO, colour = Treatment),group = Treatment)+
  geom_jitter(aes(size = 1.5, alpha = .3))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+labs(title ="% DMSO Perimeter- Labeled objects only")








# This package allows us to work with pacakges from python
#install.packages("reticulate")
library(reticulate)
use_condaenv(condaenv = "C:/Users/Jabbar/anaconda3/pkgs/python-3.9.13-h6244533_2/python.exe")
use_condaenv(condaenv = "C:/Users/Jabbar/anaconda3/python.exe")
use_condaenv(condaenv = "C:/Users/Jabbar/anaconda3/python.exe")
 
seaborn<-import('seaborn')
matplotlib <- import('matplotlib.pyplot')

seaborn$pairplot(r_to_py(my_objects3))
matplotlib$show()
