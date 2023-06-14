##this reads all files in as a batch and  processes :)
##each having there own independent plate map  :)
##the plate map is part of the DATA file.. :)
##better less span :/
##nested IC50s extraction  :/
#STARTING 7/21/22 Organelles.Intensity.Bckg..wv2 BECAME Organelles.Intensity.Bckg.wv2!!!###


#remove.packages('rlang')
install.packages('rlang')
install.packages('ggplot2', dependencies = TRUE)
install.packages("ggplot2")
install.packages('updateR')
#remove.packages(tidyverse)
install.packages('tidyverse')
install.packages("installr")
library('installr')
install.packages("stringr")

updateR()
install.packages("directlabels")
install.packages("ggrepel")
install.packages("plotrix")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("directlabels")
install.packages("drc")
install.packages("devtools")
install.packages("Rtools")
devtools::install_github("r-lib/xml2")
devtools::install_github("Rtools")
install.packages("xml2", dependencies = TRUE, lib = "C:/Program Files/R/R-4.1.0/library")
install.packages("plotly")



#.libPaths()

library(broom)
library(stringr)
library("xml2")
library('rlang')
library("plotrix")  
library("tidyverse")
library(gridExtra)
library(ggplot2)
library(drc)
library(dplyr)
library(directlabels)
library(plotly)
#library(stringr)
#library(ggrepel)


FILES<- list.files(path = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/temp", full.names = TRUE)


All <- lapply(FILES,function(i){
  df<-read.csv(file = i)
  DATA<-df[,1:23]
  PLATE_MAPS<-df[1:43,26:50]
  DRUGS<-PLATE_MAPS[28:43,1:25]
  colnames(DRUGS)<-c("Row",c(1:24))
  DRUGS<-gather(DRUGS,key = "Column", value = "treatment", 2:length(DRUGS))
  CONC<-PLATE_MAPS[1:24,3:4]
  colnames(CONC)<-c("Column","Conc")
  REPS<-PLATE_MAPS[1:16,1:2]
  colnames(REPS)<-c("Row","Rep")
   jg_data<-merge(DATA,DRUGS)
   jg_data<-merge(jg_data,CONC)
   jg_data<-merge(jg_data,REPS)
   jg_data<-separate(jg_data,col = Plate.ID, into = c("Date","Cell","Plate_no"), sep = "_", remove = FALSE)
   jg_data$Day<- substring(FILES[1], 102,106)
   jg_data
})



jg_data<-bind_rows(All)









#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Quadrant_1<-rows A:H columns  1:8
Q1<-filter(jg_data, Row %in% LETTERS[1:8] & Column %in% c(1:8)) 
Q1 %>% 
  filter(., treatment == "dmso_ctl") %>% 
  summarise(meand_dmso = mean(Q1$Organelles.Intensity.Bckg.wv2))
jg_data$quadrant <- ifelse(jg_data$Row %in% LETTERS[1:8] & jg_data$Column %in% c(1:8), "Q1", NA)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Quadrant_2<-rows A:H  columns 9:16
Q2<-filter(jg_data, Row %in% LETTERS[1:8] & Column %in% c(9:16))
Q2 %>% 
  filter(., treatment == "dmso_ctl") %>% 
  summarise(meand_dmso = mean(Q1$Organelles.Intensity.Bckg.wv2))
jg_data$quadrant <- ifelse(jg_data$Row %in% LETTERS[1:8] & jg_data$Column %in% c(9:16), "Q2",jg_data$quadrant)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Quadrant_3<- rows A:H   columns  17:24
Q3<-filter(jg_data, Row %in% LETTERS[1:8] & Column %in% c(17:24))
Q3 %>% 
  filter(., treatment == "dmso_ctl") %>% 
  summarise(meand_dmso = mean(Q1$Organelles.Intensity.Bckg.wv2))
jg_data$quadrant <- ifelse(jg_data$Row %in% LETTERS[1:8] & jg_data$Column %in% c(17:24), "Q3",jg_data$quadrant)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Quadrant_4<- rows I:P   columns  1:8
Q4<-filter(jg_data, Row %in% LETTERS[9:16] & Column %in% c(1:8))
Q4 %>% 
  filter(., treatment == "dmso_ctl") %>% 
  summarise(meand_dmso = mean(Q1$Organelles.Intensity.Bckg.wv2))
jg_data$quadrant <- ifelse(jg_data$Row %in% LETTERS[9:16] & jg_data$Column %in% c(1:8), "Q4",jg_data$quadrant)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Quadrant_5<-rows I:P   columns 9:16
Q5<-filter(jg_data, Row %in% LETTERS[9:16] & Column %in% c(9:16))
Q5 %>% 
  filter(., treatment == "dmso_ctl") %>% 
  summarise(meand_dmso = mean(Q1$Organelles.Intensity.Bckg.wv2))
jg_data$quadrant <- ifelse(jg_data$Row %in% LETTERS[9:16] & jg_data$Column %in% c(9:16), "Q5",jg_data$quadrant)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Quadrant_6<-rows I:p  columns  17:24
Q6<-filter(jg_data, Row %in% LETTERS[9:16] & Column %in% c(17:24))
Q6 %>% 
  filter(., treatment == "dmso_ctl") %>% 
  summarise(meand_dmso = mean(Q1$Organelles.Intensity.Bckg.wv2))
jg_data$quadrant <- ifelse(jg_data$Row %in% LETTERS[9:16] & jg_data$Column %in% c(17:24), "Q6",jg_data$quadrant)



colnames(jg_data)


unique(jg_data$treatment)

###################################     SUMMARIES FOR MERGING ################################################
#summarize Background besure to take just "empty" values 
#and merge in
#background<-jg_data %>% 
  #group_by(.,plate) %>% 
#  filter(.,treatment == "empty")%>% 
#  summarise(., background = mean(Organelles.Intensity.Bckg.wv2))

#jg_data<-merge(jg_data,background)           


#deduct this from all values
#jg_data<-mutate(jg_data, minus_bkg = Organelles.Intensity.Bckg.wv2 -background)
colnames(jg_data)


#Now that we have lABELED EACH QUADRANT LETS MAKE A TABLE THAT GROUPS BY THIS AND CALCULATES AN AVERAGE 
#DMSO VALUE along with a standard error Value.......MERGE them both BACK IN FOR FURTHER MUTATION
dmso_ctl<-jg_data %>% 
  group_by(.,Date,Plate_no,quadrant) %>% 
  filter(.,treatment == "dmso_ctl") %>% 
  summarise(.,avg_dmso_ctl = mean(Organelles.Intensity.Bckg.wv2))
jg_data<-merge(jg_data, dmso_ctl)

##plate_DMSO
plate_dmso<- jg_data %>% 
  group_by(., Date, Plate_no) %>% 
  filter(.,treatment == "dmso_ctl") %>% 
  summarise(plate_dmso =  mean(Organelles.Intensity.Bckg.wv2) )
jg_data<-merge(jg_data, plate_dmso)


# 20uM 807
um_807<-jg_data %>% 
  group_by(., Date, Plate_no) %>% 
  filter(.,treatment == "20 uM 807") %>% 
  summarise(pos_ctl =  mean(Organelles.Intensity.Bckg.wv2) )


# 20uM 901
um_901<-jg_data %>% 
  group_by(., Date, Plate_no) %>% 
  filter(.,treatment == "20 uM 901") %>% 
  summarise(pos_ctl =  mean(Organelles.Intensity.Bckg.wv2) )



pos_ctl<-rbind(um_807,um_901)

jg_data<-merge(jg_data, pos_ctl)



##cbe controls
cbe_ctl<-jg_data %>% 
  group_by(.,Date,Plate_no,quadrant) %>% 
  filter(.,treatment == "cbe_ctl") %>% 
  summarise(.,avg_cbe_ctl = mean(Organelles.Intensity.Bckg.wv2))
#jg_data<-merge(jg_data, cbe_ctl)


jg_data<-mutate(jg_data, percent_to_dmso_ctl = (Organelles.Intensity.Bckg.wv2/avg_dmso_ctl)*100)

jg_data<-mutate(jg_data, norm_high_ctl = (pos_ctl/avg_dmso_ctl)*100)




#summarize averages reps for each column 
replicate_average<-jg_data %>% 
  group_by(.,Date,Plate_no,treatment, Column) %>% 
  summarise(., replicate_average = mean(percent_to_dmso_ctl))

jg_data<-merge(jg_data,replicate_average) 


#summarize standard error
standar_error<-jg_data %>% 
  #filter(., !quadrant %in% c("Q5","Q4","Q6") | !Row %in% c("O","N", "P" )) %>% 
  group_by(.,Date,Plate_no,quadrant ,Column) %>% 
  summarise(.,std_error = std.error(percent_to_dmso_ctl) )
jg_data<-merge(jg_data,standar_error) 


####you'll need this new column in order to facet by plate properly  later on in ggplot...
jg_data$Plate_no2 = factor(jg_data$Plate_no, levels= str_sort(unique(jg_data$Plate_no), numeric = TRUE))


#calculate z prime
#Z_prime<-jg_data  %>%
#  group_by(.,quadrant)%>% 
#  filter(., treatment == "dmso_ctl" | treatment == "cbe_ctl") 

#std_dev_pos<-Z_prime%>% 
#  group_by(.,quadrant) %>% 
#  filter(., treatment == "dmso_ctl") %>% 
#   summarise(., std_dev_pos = sd(percent_to_dmso_ctl))

#std_dev_neg<-Z_prime%>% 
#  group_by(.,quadrant) %>% 
#  filter(., treatment == "cbe_ctl") %>% 
#  summarise(., std_dev_neg = sd(percent_to_dmso_ctl))



##Z_prime no longer has quadrants 3 and 6 since those dont have pos controls
#Z_prime<-merge(std_dev_pos,Z_prime)
#Z_prime<-merge(std_dev_neg,Z_prime)


#mutate(Z_prime, "Z_prime" =  )


omitted_controls<-c("20 uM 807","dmso_ctl", "cbe_ctl", "blank", "20 uM 901")

`%notin%` <- Negate(`%in%`)


x<-filter(jg_data,  treatment %notin% omitted_controls)




#varied_map<-read.table(file = "clipboard", sep = "\t", header = TRUE)

#x<-merge(x,varied_map)

#if you need to filter out certain reps in certain quadrants
#x<-filter(x, !quadrant %in% c("Q5","Q4","Q6") | !Row %in% c("O","N", "P" ))






### NORMALIZED CBE CONTROLS####
p1<-ggplot(data = filter(jg_data, treatment == "cbe_ctl"), aes(y=percent_to_dmso_ctl, x = treatment,colour= Plate_no))+
  geom_boxplot()+
  geom_point()+
  geom_jitter(width=.2)+
  #facet_grid( rows= vars(Plate_no),cols = vars(quadrant), drop = TRUE)+
  facet_wrap(~ Plate_no2 + quadrant,ncol = 4)+
  #geom_hline(yintercept = 50, color= "red")+
  theme_bw()
# theme(axis.text.x = element_text(angle = 90))
p1<-p1 +labs(title ="Normalized CBE Controls per plate")+ylim(NA,110)



# RAW CBE CONTROLS####
p1.1<-ggplot(data = filter(jg_data, treatment == "cbe_ctl"), aes(y=Organelles.Intensity.Bckg.wv2, x = treatment,colour= Plate_no))+
  geom_boxplot()+
  geom_point()+
  #geom_jitter(width=.2)+
  #facet_grid( rows= vars(Plate_no),cols = vars(quadrant), drop = TRUE)+
  facet_wrap(~ Plate_no2 + quadrant, ncol = 4)+
  #geom_hline(yintercept = mean(cbe_ctl$avg_cbe_ctl), color= "red")+
  theme_bw()
# theme(axis.text.x = element_text(angle = 90))
p1.1<-p1.1 +labs(title ="Raw CBE Controls per plate")

unique(jg_data$treatment)


# RAW 20uM 807 CONTROLS####
p1.3<-ggplot(data = filter(jg_data, treatment == "20 uM 807"), aes(y=Organelles.Intensity.Bckg.wv2, x = treatment, colour = Plate_no2))+
  geom_boxplot()+
  geom_point()+
  #geom_jitter(width=.2)+
  #facet_grid( rows= vars(Plate_no),cols = vars(quadrant), drop = TRUE)+
  facet_wrap(~ Plate_no2 + quadrant, ncol = 4)+
  #geom_hline(yintercept = mean(um_807$uM_807), color= "red")+
  theme_bw()
# theme(axis.text.x = element_text(angle = 90))
p1.3<-p1.3 +labs(title ="Raw 20uM 807 Controls per plate vs Avg")








#p2<-ggplot(data = filter(jg_data, treatment == "empty"), aes(x=treatment,y=percent_to_dmso_ctl))+
# geom_boxplot()+
#geom_point()
#facet_grid( cols = vars(quadrant), scales = "free", drop = TRUE)+
#theme(axis.text.x = element_text(angle = 90))
#p2 +labs(title ="Background")  


# NORMALIZED DMSO CONTROL BY QUADRANT####
p3<-ggplot(data = filter(jg_data, treatment == "dmso_ctl"), aes(x=treatment,y=percent_to_dmso_ctl))+
  geom_boxplot()+
  geom_point()+ 
  #geom_jitter(width=.2)+
  facet_grid( rows= vars(Plate_no2),cols = vars(quadrant), drop = TRUE)+
  theme_bw()
  #theme(axis.text.x = element_text(angle = 90))
p3<-p3 +labs(title ="Normalized DMSO Controls") + expand_limits(x=0,y=0)


#THIS PLOT LOOKS AT RAW DMSO CONTROLS BY QUADRANT####
p3.1<-ggplot(data = filter(jg_data, treatment == "dmso_ctl"), aes(x=treatment,y=Organelles.Intensity.Bckg.wv2))+
  geom_boxplot()+
  geom_point()+ 
  #geom_jitter(width=.2)+
  facet_grid( rows= vars(Plate_no2),cols = vars(quadrant), drop = TRUE)+
  theme_bw()
  #geom_hline(yintercept = plate_dmso, color = "red")
#theme(axis.text.x = element_text(angle = 90))
p3.1<-p3.1 +labs(title ="Raw DMSO Controls") + expand_limits(x=0,y=0)
 





# RAW DMSO CONTROLS OVER TIME####
p2<-ggplot(data = filter(jg_data, treatment == "dmso_ctl"|treatment == "cbe_ctl" ), aes(x= interaction(Column,quadrant,Row) ,y=Organelles.Intensity.Bckg.wv2))+
  geom_point(aes(color = treatment), size = 3)+ 
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid( rows= vars(Plate_no2), scales = "free", drop = TRUE)+
  theme_bw()+
  #geom_hline(yintercept = mean(dmso_ctl$avg_dmso_ctl), color = "lightseagreen", linetype = "dashed")+
  #geom_hline(yintercept = mean(cbe_ctl$avg_cbe_ctl), color = "indianred1", linetype = "dashed")+ 
  theme(axis.text.x = element_text(angle = 90), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p2<-p2+labs(title ="Raw DMSO Controls over time")+expand_limits(x=0,y=0)


# NORMALIZED CONTROLS OVER TIME####
p2.1<-ggplot(data = filter(jg_data, treatment == "dmso_ctl" |treatment == "cbe_ctl" ), aes(x= interaction(Column,quadrant,Row) ,y=percent_to_dmso_ctl))+
  geom_point(aes(color = treatment),size = 3)+ 
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid( rows= vars(Plate_no2), drop = TRUE)+
  theme_bw()+
  #geom_hline(yintercept = 70, color = "grey", linetype = "dashed")+
  theme(axis.text.x = element_text(angle = 90), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p2.1<-p2.1+labs(title ="Normalized DMSO Controls over time")+ylim(NA,130)




# RAW HIGH CONTROLS OVER TIME####
p2.2<-ggplot(data = filter(jg_data, treatment == "20 uM 807" ), aes(x= interaction(Column,quadrant,Row) ,y=Organelles.Intensity.Bckg.wv2))+
  geom_point(aes(color = treatment),size = 3)+ 
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid( rows= vars(Plate_no2), drop = TRUE)+
  theme_bw()+
  #geom_hline(yintercept = 70, color = "grey", linetype = "dashed")+
  theme(axis.text.x = element_text(angle = 90), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p2.2<-p2.2+labs(title ="Normalized DMSO Controls over time")

unique(jg_data$treatment)







#THIS PLOT LOOKS AT STANDARD ERROR
p4<-ggplot(data = standar_error, aes(x=quadrant,y=std_error))+
  geom_col()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  facet_wrap(as.numeric(Column) ~ quadrant, scales = "free")
p4<-p4 + labs(title = "Standard Error")+theme(strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")))

#All %>% filter(cpd %in% c("dmso_ctl","cbe_ctl" )) 


# SIGNAL WINDOW OF DMSO TO CBE PER QUADRANT####
p5<-ggplot(data = filter(jg_data, treatment == "dmso_ctl" | treatment == "cbe_ctl" | treatment == "20 uM 807" ), aes(x=treatment,y=percent_to_dmso_ctl, colour = treatment))+
  geom_boxplot()+
  geom_point()+
  #geom_jitter(width=.2)+
  facet_grid( rows = vars(Plate_no2), cols = vars(quadrant),  drop = TRUE)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p5<-p5 +labs(title ="DMSO Controls vs CBE Controls vs 807 Controls") + expand_limits(x=0,y=0)


#p6<-ggplot(data = zprime, aes(x=treatment,y=percent_to_dmso_ctl))+
#  geom_boxplot()+
# geom_point()
#facet_grid( rows =  vars(plate), scales = "free", drop = TRUE)  
#facet_wrap(  ~ cell+incubation+wash, scales = "free")
#p6 +labs(title ="Z-Prime")


#DOSE RESPONSE CURVES####

#p6<-ggplot(data = filter(x, treatment == "806" | treatment == "807"), aes(x=log10(as.numeric(Conc)),y=percent_to_dmso_ctl, colour = interaction(Plate_no,treatment)))+
 # geom_point()+
  #stat_smooth(method = "loess", se = FALSE)+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  #geom_hline(yintercept=130, linetype="dotted", size = 1)+
  #facet_grid(rows = vars(Date), cols = vars(treatment), scales = "free", drop = FALSE)+
  #geom_errorbar(aes(ymin = replicate_average - std_error, ymax = replicate_average + std_error ),colour = "black")+
  #facet_wrap( ~   treatment, scales = "free")
#p6<- p6 + labs(title = "806 and 807" )



#this looks at the treametent column and makes a count table for number of times things repeat
#divide by conc steps * repeat number for number of times it was repeated
repeats<-as.data.frame(table(x$treatment)) %>% 
  mutate(., repeats =  Freq / (8*7)) %>% 
  filter(.,repeats >1) %>% 
  droplevels()
repeats<-levels(unlist( repeats[1]))
 
  

## REPEATS####
p6.1<-ggplot(data = filter(x, treatment %in% repeats & 
                             treatment != "1266"&
                             treatment != "1365" &
                             treatment != "1365 +CBE"&
                             treatment != "1266 + CBE"), aes(x=log10(as.numeric(Conc)),y=percent_to_dmso_ctl, colour = quadrant))+
  geom_point()+
  stat_smooth(method = "loess", se = FALSE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_hline(yintercept=130, linetype="dotted", size = 1)+
  #facet_grid(rows = vars(Date), cols = vars(treatment), scales = "free", drop = FALSE)+
  #geom_errorbar(aes(ymin = replicate_average - std_error, ymax = replicate_average + std_error ),colour = "black")+
   # facet_wrap( ~treatment   , scales = "free")+
    facet_wrap( ~treatment)+
  xlab(label = "Log10 Conc")+
  theme_bw()
p6.1<- p6.1 + labs(title = "Repeats" ) 


ggplotly(p6.1)





#####################                                  WRITE OUT PRISM STYLE#######################################################33
###                                             DMSO and DATA need to be written out separately
  `%notin%` <- Negate(`%in%`)

 TMEM<-filter(x , Plate_no != "plate1" | Plate_no != "plate7")
 tmem<-unique(TMEM$treatment)#[c(2, 12,20, 31)]
  distill<- unique(x$treatment)#[c(1:12, 17:47)]
 
colnames(x)
Magdeline<-filter(x, treatment %in% repeats)
Magdeline_dmso<-filter(jg_data,   treatment  %in% c(repeats,omitted_controls)) 

colnames(Magdeline)
colnames(Magdeline_dmso)

Magdeline<- Magdeline[,c(2,4,5,23,30,29)]
Magdeline_dmso<- Magdeline_dmso[,c(2,4,5,23,29,30)] %>% filter(.,treatment == "dmso_ctl")


Magdeline<-pivot_wider(Magdeline,id_cols = c(Organelles.Intensity.Bckg.wv2,Conc,treatment,quadrant), 
                       names_from = c(Rep,treatment,Plate_no,quadrant), 
                       values_from = Organelles.Intensity.Bckg.wv2)

Magdeline_dmso<-pivot_wider(Magdeline_dmso,id_cols = c(Organelles.Intensity.Bckg.wv2,Conc,treatment,quadrant), 
                       names_from = c(Rep,treatment,Plate_no), 
                       values_from = Organelles.Intensity.Bckg.wv2)

Magdeline<-as.data.frame(Magdeline)
Magdeline_dmso<-as.data.frame(Magdeline_dmso)

Magdeline<-Magdeline %>% group_by(.,treatment) 
Magdeline_dmso<-Magdeline_dmso %>% group_by(.,treatment)  


Magdeline<-Magdeline[order(Magdeline$Conc),]
Magdeline_dmso<-Magdeline_dmso[order(Magdeline_dmso$Conc),]

write.table(Magdeline,file = 'clipboard', sep = "\t")
write.table(Magdeline_dmso,file = 'clipboard', sep = "\t")
write.csv(Magdeline,file = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/Prism Gateway/spread_for_prism.csv")
write.table(as.data.frame(plate_dmso),file = 'clipboard', sep = "\t")
#########################################################################################################################







#COMPOUNDS ACROSS PLATES WITH A HIGH CONTROL####
p7<-ggplot(data = x , aes(x=log10(as.numeric(Conc)),y=percent_to_dmso_ctl, colour = treatment))+
  geom_point()+
  stat_smooth(method = "loess", se =FALSE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_point(data = jg_data, color = "black" ,aes(x=0, y=norm_high_ctl, shape = 8, colour = Plate_no))+
  scale_shape_identity()+
  #facet_wrap(cols = vars(Cell), rows= vars(treatment), drop = FALSE)+
  #geom_errorbar(aes(ymin = replicate_average - std_error, ymax = replicate_average + std_error ),colour = "black")+
  geom_dl(aes(label=treatment), method = list(dl.combine("last.points")), cex = 0.8)+
  facet_wrap( ~Plate_no2)+#new column allows plate to be in order
  #geom_point(aes(x=log10(as.numeric(Conc)),y=pos_ctl, colour = treatment))+
  theme_bw()
p7<- p7 + labs(title = "Compound across plates with high control " )+xlim(-2,4)
ggplotly(p7)

#COMPOUNDS ACROSS NPC1####
p7.1<-ggplot(data =  x, aes(x=log10(as.numeric(Conc)),y=percent_to_dmso_ctl, colour = treatment))+
  geom_point()+
  stat_smooth(method = "loess")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  #facet_grid(rows = vars(Cell), cols = vars(treatment), scales = "free", drop = FALSE)+
  #geom_errorbar(aes(ymin = replicate_average - std_error, ymax = replicate_average + std_error ),colour = "black")+
  facet_wrap( vars(treatment))+
  theme_bw()
p7.1<- p7.1 + labs(title = "Compound across NPC1" )


#ALL COMPOUNDS TOGETHER####
p8<-ggplot(data =  x, aes(x=log10(as.numeric(Conc)),y=replicate_average, colour = treatment))+
  geom_point()+
  stat_smooth(method = "loess")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_hline(yintercept=100, linetype="dashed", size =1)+
  geom_hline(yintercept=130, linetype="dotted", size = 1)+
  geom_dl(aes(label=treatment), method = list(dl.combine("last.points")), cex = 0.8)+
  theme_bw()
#facet_grid( cols = vars(plate),rows = vars(wash), scales = "free")+
#facet_wrap(treatment~)
p8<-p8 + labs(title = "Rank and File" )+xlim(-2,2.2)




#ggplotly(p8)




unique(x$Plate_no)
choosen = c("plate11" , "plate12" , "plate13")
filter(x, between(Conc, 11,30) & Plate_no %notin% choosen)
unique(x$Conc)


#COMPOUNDS GROUPED BY PLATE####
p8.1<-ggplot(data = x, aes(x=log10(as.numeric(Conc)),y=replicate_average, colour = treatment))+
  geom_point()+
  stat_smooth(method = "loess")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_hline(yintercept=100, linetype="dashed", size =1)+
  geom_hline(yintercept=130, linetype="dotted", size = 1)+
#facet_grid( cols = vars(plate),rows = vars(wash), scales = "free")+
  facet_wrap(~Plate_no2)+
  geom_dl(aes(label=treatment), method = list(dl.combine("last.points")), cex = 0.8)+
  theme_bw()
p8.1<-p8.1 + labs(title = "Rank and File by plate" )+xlim(-2,5)

ggplotly(p8.1)

           
           

####

drew<-filter(jg_data, replicate_average >= 200  & Conc  >= 10) 
 # unique(drew$treatment)


x$
top_hitters = unique(x$treatment)
 high_controls =  c("807", "1364")
 ignore_plates= c("807", "1503")
p8.2<-ggplot(data =  filter(x, Plate_no == "plate1") , aes(x=log10(as.numeric(Conc)),y=replicate_average, colour = treatment))+
  geom_point()+
  stat_smooth(method = "loess", se = FALSE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_hline(yintercept=100, linetype="dashed", size =1)+
  geom_hline(yintercept=130, linetype="dotted", size = 1)+
  geom_errorbar(aes(ymin = replicate_average - std_error, ymax = replicate_average + std_error ),colour = "grey", size = .1, width = .1)+
  theme_bw()+
  geom_dl(aes(label=treatment), method = list(dl.combine("last.points")), cex = 0.8)+
  xlab("log10")+
  ylab("Replicate Average")
   
#facet_grid( cols = vars(plate),rows = vars(wash), scales = "free")+
#facet_wrap(treatment~)
p8.2<-p8.2 + labs(title = "TMEM Compounds" )+xlim(-2,2.2)


###RANK BY EFFICACY####
#this takes the top dose of 30 and sorts by replicate average
shaba<-filter(x, Conc == 10 )
shaba<-shaba[order(shaba$replicate_average, decreasing = TRUE),]
#unique(shaba$treatment)
write.table(unique(shaba$treatment),file = "clipboard", sep = "\t")



#tmem<-c("plate3", "plate4")
#######################################    EC30 REGRESSION    ###########################################
###since theres no top to our curve it may be more useful to find the concentration at 130% and extrapolate concentration
###group the data and create a models for each in a tibble
##the span argument affect the smoothing and the accuracy to the data
library(broom)
my_models <- x %>% 
  group_by(.,Plate_no,treatment) %>% 
  do(fit = loess(log10(as.numeric(Conc)) ~ replicate_average, data = ., span = .25))
  #do(fit = stat_smooth(method= "loess" , formula = log10(as.numeric(Conc)) ~ replicate_average, data = .))

###create a new dataframe with made up values
x_sim<-x %>% 
  mutate(.,replicate_average = 130)


#using the library(broom) we must join our model with the simulated data frame 
#and use augment which is like a prediction
#plugging in 200 returns 1 so this log  must be converted to 
#standard conc
ec30<- x_sim %>% group_by(.,Plate_no,treatment) %>% nest() %>% 
  full_join(my_models) %>% 
  group_by(.,treatment,Plate_no) %>% 
  do(augment(.$fit[[1]], newdata = .$data[[1]]))

colnames(ec30)
ec30<-as.data.frame(distinct(ec30[,c(1,2,40)]))
ec30<-mutate(ec30, ec30_Conc = 10^.fitted)

write.table(unique(drew$treatment),file = 'clipboard', sep = "\t")


##max at certain concentration
x %>% 
filter(., Column == "10") %>% 
  select(.,c(Plate_no, treatment, replicate_average))
 

colnames(x)
 




###Youll need loop thru this by PLATE DAY an CELL like the lipidomics data
#######write out a file that is ready for prism
output<-x %>% 
  #group_by(plate) %>% 
 # dplyr::select(.,treatment,Conc,percent_to_dmso_ctl, Row, quadrant ,Rep, Column) %>% 
  #filter(., plate == "once_npc2_.5hr_plate#1.txt") %>% 
 # pivot_wider(.,id_cols = c("Conc","treatment","Column","quadrant"),
            #  names_from = c("Rep"),
             # values_from =  percent_to_dmso_ctl
  #)

#output2<-output[
 # with( output,order(quadrant) ), 
  ]
#write.csv(output2,file = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/Prism Gateway/20200916_npc1_plate6.csv", sep = "\t")









#######################################    EC50 ###########################################
#convert to factor 
str(x)
x$quadrant<-as.factor(x$quadrant)
x$treatment<-as.factor(x$treatment)
x$replicate_average
x$Conc<-as.numeric(x$Conc)

#group and nest the data by 
 by_treatment <- x %>% 
  group_by(.,Plate_no) %>%  
  nest()
 
 
 ##collapse the data 
 Y<-unique(x %>% dplyr::select(., treatment, replicate_average,Conc))
 

 #prism fixes slope to 1 for its 3 point logistic
 my_curved_fit <- drm(
   formula = replicate_average ~ Conc,
   curveid = treatment,
   data = Y,
   logDose = 10,
   fct = LL.4(fixed = c(NA,NA,NA,NA), names = c("hill", "min_value","max_value" ,"log10[ec_50]"))
   #fct = LL.3(fixed = c(1,NA,NA), names = c("hill","max_value" ,"log10[ec_50]"))
   #fct = LL2.3(fixed = c(NA,NA,NA), names = c("hill", "min_value","max_value" ,"log10[ec_50]"))
 )
  


summary(my_curved_fit)

##table of just IC50s
ec50s<-summary(my_curved_fit)
ec50s<-as.data.frame(ec50s$coefficients)

ec50s<-ec50s[13:18,][1]
ec50s<-ec50s[19:24,][1]
ec50s<-ec50s[c(7,8),][1]

##call the function on the nested data and preserve the mapping
##perhaps mutate again and call summary data on the mod column which contains a list of dose reponse models

 

#calling the 6th column give us a list of nested ec50s as long as our list.
#unnest(models, cols = c(Date,Plate_no, ec50_sum) )????????????????


ec50<-as.list(models$ec50_sum[1])
 

choosen<-c("806","807")
##replot 
p9<-ggplot(data = filter(x, treatment %in% choosen, quadrant != "Q6"), aes(x=log10(as.numeric(Conc)),y=percent_to_dmso_ctl, colour = treatment))+
  geom_point(aes(shape = treatment), size =2, alpha = .6)+
  #stat_smooth(method = "loess")+
  geom_smooth(se=F)+
  geom_hline(yintercept=100, linetype="dashed", size =1)+
  geom_hline(yintercept=130, linetype="dotted", size = 1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  annotation_custom(tableGrob(ec50s), xmin=1, xmax=-4, ymin=Inf, ymax=175)+
  theme_bw()
p9  
  #facet_grid( cols = vars(plate),rows = vars(wash), scales = "free")+
  #facet_wrap(day~plate)

###write a File of normalized values 
#write.csv(ec50s,file = "C:/Users/Jabbar/Desktop/Prism Gateway/donato_ec50s.csv" , sep = "\t")


colnames(x)




unique(x$treatment)



update.packages(xml2)


###################################################### POWER POINT################################
library(officer) 
# Create a PowerPoint 
doc <- read_pptx()
layout_summary(doc)
doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p1 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p1.1 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p1.3 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p2 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p2.1 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p2.2 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p3 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p3.1 ,location = ph_location_fullsize() )


doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p5 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = repeats ,location = ph_location_fullsize() )


doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p6 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p6.1 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p7 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p7.1 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p8 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p8.1 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p8.2 ,location = ph_location_fullsize() )


#doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
#doc <- ph_with(x = doc, value = p9 ,location = ph_location_fullsize() )
print(doc, target = paste0( "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/Prism Gateway/",substring(FILES[1],70,80),"_summary",".pptx" )) 


###playing around
ggplot(filter(jg_data,  treatment == "dmso_ctl"), aes(x=Organelles.Intensity.Bckg.wv2))+
  geom_histogram( binwidth = 30, colour = "black", fill = "white")+
  geom_density( alpha = .2, fill = "#FF6666") +
  facet_grid(~Plate_no)
  
  
ggplot(filter(jg_data,  treatment == "dmso_ctl"), aes(x=Organelles.Intensity.Bckg.wv2, fill = Plate_no))+
  #geom_histogram( binwidth = 30, colour = "black", fill = "white")+
  geom_density( alpha = .3 )+
  facet_grid(~Date)

                  
  ggplot(dat, aes(x=rating, fill=cond)) + geom_density(alpha=.3)