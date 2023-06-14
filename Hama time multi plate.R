library(tidyverse)
library("plotrix") 
library(ggplot2)
library(mgcv)
library(drc)
library(directlabels)
#Read in Files
FILES<- list.files(path = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/Sandbox", full.names = TRUE)



All<-readxl::read_xlsx(FILES[1], sheet = 1)

All<-All[8:104,]

All<-All[-1,]


colnames(All)<-c("Row", c(1:24),"Plate_no")


All<-gather(All,key = "Column", value = "treatment", 2:25)







#read in map
plate_maps<-readxl::read_xlsx(path = "C:/Caraway Project/HIGH CONTENT/plate map_hama.xlsx", sheet = 1)
plate_maps<-as.data.frame(plate_maps) 

drugs<-plate_maps[c(28:43),c(1:25)]
colnames(drugs)<-c("Row",c(1:24))

drugs<-gather(drugs,key = "Column", value = "cpd", 2:25)



conc<-plate_maps[c(1:24),c(3:4)]
colnames(conc)<-c("Column","conc")



reps<-plate_maps[c(1:16),c(1:2,5)]
colnames(reps)<-c("Row","Rep","hemisphere")





#merge maps and plate data
All<-merge(All,drugs)
All<-merge(All,conc)
All<-merge(All,reps)





#quadrant definitions
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Quadrant_1<-rows A:H columns  1:8
Q1<-filter(All, Row %in% LETTERS[1:8] & Column %in% c(1:8)) 
Q1 %>% 
  filter(., treatment == "dmso_ctl") %>% 
  summarise(mean_dmso = mean(Q1$treatment))
All$quadrant <- ifelse(All$Row %in% LETTERS[1:8] & All$Column %in% c(1:8), "Q1", NA)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Quadrant_2<-rows A:H  columns 9:16
Q2<-filter(All, Row %in% LETTERS[1:8] & Column %in% c(9:16))
Q2 %>% 
  filter(., treatment == "dmso_ctl") %>% 
  summarise(mean_dmso = mean(Q1$treatment))
All$quadrant <- ifelse(All$Row %in% LETTERS[1:8] & All$Column %in% c(9:16), "Q2",All$quadrant)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Quadrant_3<- rows A:H   columns  17:24
Q3<-filter(All, Row %in% LETTERS[1:8] & Column %in% c(17:24))
Q3 %>% 
  filter(., treatment == "dmso_ctl") %>% 
  summarise(mean_dmso = mean(Q1$treatment))
All$quadrant <- ifelse(All$Row %in% LETTERS[1:8] & All$Column %in% c(17:24), "Q3",All$quadrant)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Quadrant_4<- rows I:P   columns  1:8
Q4<-filter(All, Row %in% LETTERS[9:16] & Column %in% c(1:8))
Q4 %>% 
  filter(., treatment == "dmso_ctl") %>% 
  summarise(mean_dmso = mean(Q1$treatment))
All$quadrant <- ifelse(All$Row %in% LETTERS[9:16] & All$Column %in% c(1:8), "Q4",All$quadrant)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Quadrant_5<-rows I:P   columns 9:16
Q5<-filter(All, Row %in% LETTERS[9:16] & Column %in% c(9:16))
Q5 %>% 
  filter(., treatment == "dmso_ctl") %>% 
  summarise(mean_dmso = mean(Q1$treatment))
All$quadrant <- ifelse(All$Row %in% LETTERS[9:16] & All$Column %in% c(9:16), "Q5",All$quadrant)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Quadrant_6<-rows I:p  columns  17:24
Q6<-filter(All, Row %in% LETTERS[9:16] & Column %in% c(17:24))
Q6 %>% 
  filter(., treatment == "dmso_ctl") %>% 
  summarise(mean_dmso = mean(Q1$treatment))
All$quadrant <- ifelse(All$Row %in% LETTERS[9:16] & All$Column %in% c(17:24), "Q6",All$quadrant)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


jg_data<-All

colnames(jg_data)[4]<-"Organelles.Intensity.Bckg..wv2"
colnames(jg_data)[5]<-"treatment"

jg_data$Organelles.Intensity.Bckg..wv2<-as.numeric(jg_data$Organelles.Intensity.Bckg..wv2)

###################################     SUMMARIES FOR MERGING ################################################
#summarize Background besure to take just "empty" values 
#and merge in
#background<-jg_data %>% 
#group_by(.,plate) %>% 
#  filter(.,treatment == "empty")%>% 
#  summarise(., background = mean(Organelles.Intensity.Bckg..wv2))

#jg_data<-merge(jg_data,background)           


#deduct this from all values
#jg_data<-mutate(jg_data, minus_bkg = Organelles.Intensity.Bckg..wv2 -background)



#Now that we have lABELED EACH QUADRANT LETS MAKE A TABLE THAT GROUPS BY THIS AND CALCULATES AN AVERAGE 
#DMSO VALUE along with a standard error Value.......MERGE them both BACK IN FOR FURTHER MUTATION
dmso_ctl<-jg_data %>% 
  group_by(.,Plate_no,quadrant) %>% 
  filter(.,treatment == "dmso_ctl") %>% 
  summarise(.,avg_dmso_ctl = mean(Organelles.Intensity.Bckg..wv2))
jg_data<-merge(jg_data, dmso_ctl)

##plate_DMSO
plate_dmso<- jg_data %>% 
  group_by(., Plate_no) %>% 
  filter(.,treatment == "dmso_ctl") %>% 
  summarise(plate_dmso =  mean(Organelles.Intensity.Bckg..wv2) )
jg_data<-merge(jg_data, plate_dmso)


# 20uM 807
um_807<-jg_data %>% 
  group_by(., Plate_no) %>% 
  filter(.,treatment == "20 uM 807") %>% 
  summarise(pos_ctl =  mean(Organelles.Intensity.Bckg..wv2) )


# 20uM 901
um_901<-jg_data %>% 
  group_by(., Plate_no) %>% 
  filter(.,treatment == "20 uM 901") %>% 
  summarise(pos_ctl =  mean(Organelles.Intensity.Bckg..wv2) )



pos_ctl<-rbind(um_807,um_901)

jg_data<-merge(jg_data, pos_ctl)



##cbe controls
cbe_ctl<-jg_data %>% 
  group_by(.,Plate_no,quadrant) %>% 
  filter(.,treatment == "cbe_ctl") %>% 
  summarise(.,avg_cbe_ctl = mean(Organelles.Intensity.Bckg..wv2))
#jg_data<-merge(jg_data, cbe_ctl)


jg_data<-mutate(jg_data, percent_to_dmso_ctl = (Organelles.Intensity.Bckg..wv2/avg_dmso_ctl)*100)

jg_data<-mutate(jg_data, norm_high_ctl = (pos_ctl/avg_dmso_ctl)*100)




#summarize averages reps for each column 
replicate_average<-jg_data %>% 
  group_by(.,treatment, Column) %>% 
  summarise(., replicate_average = mean(percent_to_dmso_ctl))

jg_data<-merge(jg_data,replicate_average) 


#summarize standard error
standar_error<-jg_data %>% 
  #filter(., !quadrant %in% c("Q5","Q4","Q6") | !Row %in% c("O","N", "P" )) %>% 
  group_by(.,quadrant ,Column) %>% 
  summarise(.,std_error = std.error(percent_to_dmso_ctl) )
jg_data<-merge(jg_data,standar_error) 


####you'll need this new column in order to facet by plate properly  later on in ggplot...
#jg_data$Plate_no2 = factor(jg_data$Plate_no, levels= str_sort(unique(jg_data$Plate_no), numeric = TRUE))


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


omitted_controls<-c("20 uM 807", "dmso_ctl", "cbe_ctl", "blank", "20 uM 901")

`%notin%` <- Negate(`%in%`)


x<-filter(jg_data,  treatment %notin% omitted_controls)

unique(x$treatment)


#varied_map<-read.table(file = "clipboard", sep = "\t", header = TRUE)

#x<-merge(x,varied_map)

#if you need to filter out certain reps in certain quadrants
#x<-filter(x, !quadrant %in% c("Q5","Q4","Q6") | !Row %in% c("O","N", "P" ))





###QC####################################################################
#THIS PLOT LOOKS AT Normalized CBE CONTROLS
p1<-ggplot(data = filter(jg_data, treatment == "cbe_ctl"), aes(y=percent_to_dmso_ctl, x = treatment,colour= quadrant))+
  geom_boxplot()+
  geom_point()+
  geom_jitter(width=.2)+
  facet_grid( rows= vars(Plate_no),cols = vars(quadrant), drop = TRUE)+
  #facet_wrap(~   quadrant,ncol = 4)+
  #geom_hline(yintercept = 50, color= "red")+
  theme_bw()
# theme(axis.text.x = element_text(angle = 90))
p1<-p1 +labs(title ="Normalized CBE Controls per plate")+ylim(NA,110)



#THIS PLOT LOOKS AT Raw CBE CONTROLS
p1.1<-ggplot(data = filter(jg_data, treatment == "cbe_ctl"), aes(y=Organelles.Intensity.Bckg..wv2, x = treatment,colour= quadrant))+
  geom_boxplot()+
  geom_point()+
  #geom_jitter(width=.2)+
  facet_grid( rows= vars(Plate_no),cols = vars(quadrant), drop = TRUE)+
  #facet_wrap(~ quadrant, ncol = 4)+
  #geom_hline(yintercept = mean(cbe_ctl$avg_cbe_ctl), color= "red")+
  theme_bw()
# theme(axis.text.x = element_text(angle = 90))
p1.1<-p1.1 +labs(title ="Raw CBE Controls per plate")

unique(jg_data$treatment)


#THIS PLOT LOOKS AT Raw 20uM 807 CONTROLS
p1.3<-ggplot(data = filter(jg_data, treatment == "20 uM 807"), aes(y=Organelles.Intensity.Bckg..wv2, x = treatment, colour = quadrant))+
  geom_boxplot()+
  geom_point()+
  #geom_jitter(width=.2)+
  facet_grid( rows= vars(Plate_no),cols = vars(quadrant), drop = TRUE)+
  #facet_wrap(~  quadrant, ncol = 4)+
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


#THIS PLOT LOOKS AT Normalized DMSO CONTROL BY QUADRANT
p3<-ggplot(data = filter(jg_data, treatment == "dmso_ctl"), aes(x=treatment,y=percent_to_dmso_ctl))+
  geom_boxplot()+
  geom_point()+ 
  #geom_jitter(width=.2)+
  #facet_wrap(~  quadrant, ncol = 4)+
   facet_grid( rows= vars(Plate_no),cols = vars(quadrant), drop = TRUE)+
  theme_bw()
#theme(axis.text.x = element_text(angle = 90))
p3<-p3 +labs(title ="Normalized DMSO Controls") + expand_limits(x=0,y=0)


#THIS PLOT LOOKS AT Raw DMSO CONTROL BY QUADRANT
p3.1<-ggplot(data = filter(jg_data, treatment == "dmso_ctl"), aes(x=treatment,y=Organelles.Intensity.Bckg..wv2))+
  geom_boxplot()+
  geom_point()+ 
  #geom_jitter(width=.2)+
  #facet_wrap(~  quadrant, ncol = 4)+
  facet_grid( rows= vars(Plate_no),cols = vars(quadrant), drop = TRUE)+
  theme_bw()
#geom_hline(yintercept = plate_dmso, color = "red")
#theme(axis.text.x = element_text(angle = 90))
p3.1<-p3.1 +labs(title ="Raw DMSO Controls") + expand_limits(x=0,y=0)






#THIS PLOT LOOKS AT RAW DMSO CONTROLS OVER TIME
p2<-ggplot(data = filter(jg_data, treatment == "dmso_ctl"|treatment == "cbe_ctl" ), aes(x= interaction(Column,quadrant,Row) ,y=Organelles.Intensity.Bckg..wv2))+
  geom_point(aes(color = treatment), size = 3)+ 
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid( rows= vars(Plate_no), scales = "free", drop = TRUE)+
  theme_bw()+
  #geom_hline(yintercept = mean(dmso_ctl$avg_dmso_ctl), color = "lightseagreen", linetype = "dashed")+
  #geom_hline(yintercept = mean(cbe_ctl$avg_cbe_ctl), color = "indianred1", linetype = "dashed")+ 
  theme(axis.text.x = element_text(angle = 90), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p2<-p2+labs(title ="Raw DMSO Controls over time")+expand_limits(x=0,y=0)


#THIS PLOT LOOKAT AT NORMALIZED CONTROLS OVER TIME
p2.1<-ggplot(data = filter(jg_data, treatment == "dmso_ctl" |treatment == "cbe_ctl" ), aes(x= interaction(Column,quadrant,Row) ,y=percent_to_dmso_ctl))+
  geom_point(aes(color = treatment),size = 3)+ 
  theme(axis.text.x = element_text(angle = 90))+
 # facet_grid( rows= vars(Plate_no), drop = TRUE)+
  theme_bw()+
  #geom_hline(yintercept = 70, color = "grey", linetype = "dashed")+
  theme(axis.text.x = element_text(angle = 90), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p2.1<-p2.1+labs(title ="Normalized DMSO Controls over time")+ylim(NA,130)




#THIS PLOT LOOKAT AT Raw High CONTROLS OVER TIME
p2.2<-ggplot(data = filter(jg_data, treatment == "20 uM 807" ), aes(x= interaction(Column,quadrant,Row) ,y=Organelles.Intensity.Bckg..wv2))+
  geom_point(aes(color = treatment),size = 3)+ 
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid( rows= vars(Plate_no), drop = TRUE)+
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


#THIS PLOT LOOKS AT SIGNAL WINDOW OF DMSO TO CBE PER QUADRANT
p5<-ggplot(data = filter(jg_data, treatment == "dmso_ctl" | treatment == "cbe_ctl" | treatment == "20 uM 807" ), aes(x=treatment,y=percent_to_dmso_ctl, colour = treatment))+
  geom_boxplot()+
  geom_point()+
  #geom_jitter(width=.2)+
  #facet_grid( rows = vars(Plate_no), cols = vars(quadrant),  drop = TRUE)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p5<-p5 +labs(title ="DMSO Controls vs CBE Controls vs 807 Controls") + expand_limits(x=0,y=0)


#p6<-ggplot(data = zprime, aes(x=treatment,y=percent_to_dmso_ctl))+
#  geom_boxplot()+
# geom_point()
#facet_grid( rows =  vars(plate), scales = "free", drop = TRUE)  
#facet_wrap(  ~ cell+incubation+wash, scales = "free")
#p6 +labs(title ="Z-Prime")


###########################################

p6<-ggplot(data = filter(x, treatment == "806" | treatment == "807"), aes(x=log10(as.numeric(Conc)),y=percent_to_dmso_ctl, colour = interaction(Plate_no,treatment)))+
  geom_point()+
  stat_smooth(method = "loess", se = FALSE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_hline(yintercept=130, linetype="dotted", size = 1)+
  #facet_grid(rows = vars(Date), cols = vars(treatment), scales = "free", drop = FALSE)+
  #geom_errorbar(aes(ymin = replicate_average - std_error, ymax = replicate_average + std_error ),colour = "black")+
  #facet_wrap( ~   treatment, scales = "free")
  p6<- p6 + labs(title = "806 and 807" )



#this looks at the treametent column and makes a count table for number of times things repeat
#divide by conc steps * repeat number for number of times it was repeated
repeats<-as.data.frame(table(x$treatment)) %>% 
  mutate(., repeats =  Freq / (8*7)) %>% 
  filter(.,repeats >1) %>% 
  droplevels()
repeats<-levels(unlist( repeats[1]))



##plot things that are in the repeat list
p6.1<-ggplot(data = filter(x, treatment %in% repeats), aes(x=log10(as.numeric(Conc)),y=percent_to_dmso_ctl, colour = interaction(Plate_no,treatment)))+
  geom_point()+
  stat_smooth(method = "loess", se = FALSE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_hline(yintercept=130, linetype="dotted", size = 1)+
  #facet_grid(rows = vars(Date), cols = vars(treatment), scales = "free", drop = FALSE)+
  #geom_errorbar(aes(ymin = replicate_average - std_error, ymax = replicate_average + std_error ),colour = "black")+
  facet_wrap( ~   treatment, scales = "free")+
  xlab(label = "Log10 Conc")+
  theme_bw()
p6.1<- p6.1 + labs(title = "Repeats" ) 



`%notin%` <- Negate(`%in%`)

#tmem<-unique(x$treatment)[c(2, 12,20, 31)]
#distill<- unique(x$treatment)[c(1:12, 17:47)]
#plot the average point and facet by plate 

#Magdeline<-filter(x, treatment == "CTM-138") %>% 
#Magdeline<- Magdeline[,c(29,30,35)]
#Magdeline<-pivot_wider(Magdeline,id_cols = c(percent_to_dmso_ctl,Conc), names_from = Rep, values_from = percent_to_dmso_ctl)
#write.table(Magdeline,file = 'clipboard', sep = ",")

colnames(x)
p7<-ggplot(data = x , aes(x=log10(as.numeric(conc)),y=percent_to_dmso_ctl, colour = treatment))+
  geom_point()+
  stat_smooth(method = "loess", se =FALSE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_point(data = jg_data, color = "black" ,aes(x=0, y=norm_high_ctl, shape = 8, colour = Plate_no))+
  scale_shape_identity()+
  #facet_wrap(cols = vars(Cell), rows= vars(treatment), drop = FALSE)+
  #geom_errorbar(aes(ymin = replicate_average - std_error, ymax = replicate_average + std_error ),colour = "black")+
  geom_dl(aes(label=treatment), method = list(dl.combine("last.points")), cex = 0.8)+
  facet_wrap( ~Plate_no)+#new column allows plate to be in order
  #geom_point(aes(x=log10(as.numeric(Conc)),y=pos_ctl, colour = treatment))+
  theme_bw()
p7<- p7 + labs(title = "Compound across plates with high control " )+xlim(-2,2.0)



p7.1<-ggplot(data =  x, aes(x=log10(as.numeric(conc)),y=percent_to_dmso_ctl, colour = treatment))+
  geom_point()+
  stat_smooth(method = "loess")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  #facet_grid(rows = vars(Cell), cols = vars(treatment), scales = "free", drop = FALSE)+
  #geom_errorbar(aes(ymin = replicate_average - std_error, ymax = replicate_average + std_error ),colour = "black")+
  facet_wrap( vars(treatment))+
  theme_bw()
p7.1<- p7.1 + labs(title = "Compound across NPC1" )



p8<-ggplot(data =  x, aes(x=log10(as.numeric(conc)),y=replicate_average, colour = treatment))+
  geom_point()+
  stat_smooth(method = "loess")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_hline(yintercept=100, linetype="dashed", size =1)+
  geom_hline(yintercept=130, linetype="dotted", size = 1)+
  theme_bw()
#facet_grid( cols = vars(plate),rows = vars(wash), scales = "free")+
#facet_wrap(treatment~)
p8<-p8 + labs(title = "Rank and File" )





unique(x$Plate_no)
choosen = c("plate3" , "plate4" , "plate9" , "plate6")

p8.1<-ggplot(data = x, aes(x=log10(as.numeric(conc)),y=replicate_average, colour = treatment))+
  geom_point()+
  stat_smooth(method = "loess")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_hline(yintercept=100, linetype="dashed", size =1)+
  geom_hline(yintercept=130, linetype="dotted", size = 1)+
  #facet_grid( cols = vars(plate),rows = vars(wash), scales = "free")+
  #facet_wrap(~Plate_no2)+
  geom_dl(aes(label=treatment), method = list(dl.combine("last.points")), cex = 0.8)+
  theme_bw()
p8.1<-p8.1 + labs(title = "Rank and File by plate" )+xlim(-2,1.5)


####


###################################################### power point################################
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
doc <- ph_with(x = doc, value = p6 ,location = ph_location_fullsize() )

#doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
#doc <- ph_with(x = doc, value = p6.1 ,location = ph_location_fullsize() )

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




