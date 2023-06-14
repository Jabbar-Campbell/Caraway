install.packages("pzfx")
library(pzfx)

library(dplyr)
library(tidyverse)


path<-"C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/temp/20210401_alpha_synuclein_cw0006_media_from_nat_and_sarah.pzfx"



#load full file names from a folder of interest
FILES<-list.files(path = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/temp", full.names = TRUE)



#my_tables<-pzfx_tables(path = FILES[1])


##read in each file using the above filename  attaching a date, anaylate and sheetname to each....
ALL<-lapply(FILES, function(i){
       my_tables<-pzfx_tables(path = i)
         lapply(my_tables, function(j){
           
          df<-read_pzfx(path = i, table = j)
          df$date<-substring(i,57,64 )
          df$ELISA<-strsplit(i,"_")[[1]][2]
          df$sheetname<-j
          #df$rep<- c(1:length(df))
          df
         })
  
})


#flattens out the list is lists in a single list of DFS
ALL2<-unlist(ALL,recursive = FALSE)



#removes the dataframes that contain  standards curve data based on content fo any condition being true
 for (i in 1:length(ALL2)) {
      ALL2[[i]][is.na(ALL2[[i]])] = 0#I had to remove all NA values
      if(any(ALL2[[i]]  == "std7" |
             ALL2[[i]]  == "std 7" | 
             ALL2[[i]]  == "p- 1"  |
             ALL2[[i]]  == "Standard curve jg"|
             ALL2[[i]]== "plasma 1" |
             ALL2[[i]]== "raw standard"|
             ALL2[[i]]== "raw samples with standard" |
             ALL2[[i]]== "s2 test" |
             ALL2[[i]]== "test 50" |
             ALL2[[i]]  == "Standard curve"|
             ALL2[[i]]  == "Original standard"|
             ALL2[[i]]  == "Standard Curve"|
             ALL2[[i]]  == "Back calculated standards log anti log"|
             ALL2[[i]]  == "s1")) {
      ALL2[[i]]<-1
      ALL2[[i]]$rep<-c(1:length(ALL2[[i]]))
      ALL2
      } 
  
}




#adds a replicatecolumn for each of the  dataframes
for (i in 1:length(ALL2)) {
ALL2[[i]]$rep<-c(1:length(ALL2[[i]]$ELISA))
}







##another loop to test if its a data frame starting from the end. if you aint a dataframe you gotsta go!
for (i in length(ALL2):1) {
  if(is.data.frame(ALL2[[i]])){
    ALL2[[i]]<-ALL2[[i]]
  }else{ALL2[[i]]<-NULL}
  
}




#find the date column 
#which(colnames(ALL2[[5]]) == "date",arr.ind = TRUE  )-1


##This finds the column called "date" and gathers everything before it ie 1:date
ALL3<-ALL2
for (i in 1:length(ALL2)) {
ALL3[[i]]<-gather(ALL2[[i]], key="treatment", value = value, 1:which(colnames(ALL2[[i]]) == "date",arr.ind = TRUE  )-1)
#ALL3[[i]]<-as.data.frame(ALL3[[i]])
}



#binds all the data frames into one dataframe and removes the rows (which was once a column before gathering)labeled Row Title
ALL4<-do.call("rbind",ALL3) %>% 
 filter(., treatment != "ROWTITLE" & value != "0" ) 


library(plotrix)
#calculate some metrics for ggplot to use and merge back in
Average_rep<-ALL4 %>% 
  group_by(.,date,ELISA,sheetname,treatment) %>% 
  summarise(average = mean(as.numeric(value)), SEM = std.error(as.numeric(value)))

ALL4<-merge(ALL4,Average_rep)




unique(ALL4$sheetname)



#graph a subset of the data
p1<-ggplot(filter(ALL4,ELISA == "24(s)Hydroxycholesterol" &   
                   sheetname == "Lrrk2_HOMO Males" & 
                   date == "20210113"), mapping = aes(x=treatment,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", color = "blue", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(shape=treatment))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_grid(ELISA~sheetname )+
 # geom_text(aes(label = ELISA))
 #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Reprodution of Prism Graph")
  







#graph all worksheets associated with a certain day and anylate 
#because at this time I was placeing columns on separate
#worksheets and then dragging them onto a graph vehecle is absent
p2<-ggplot(filter(ALL4,ELISA == "24(s)Hydroxycholesterol"  & 
                date == "20210113"), mapping = aes(x=treatment,y=as.numeric(value), fill = sheetname ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color= sheetname))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(~sheetname , scales = 'free')+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "24sHydroxycholesterol Data from a single Prism file colored by worksheet")


#graph all worksheets associated with a certain day and anylate
#if the nameing convention is preserved/conserved  ie "Vehicle" vs "DMSO" or "WT Vehicle"
#or "30mg/kg" vs "30 mg/kg" this could be useful
p3<-ggplot(filter(ALL4,ELISA == "24(s)Hydroxycholesterol"  & 
                date == "20210113"), mapping = aes(x=treatment,y=as.numeric(value), fill = sheetname ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color=sheetname))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(ELISA~date )+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "24sHydroxycholesterol Data from a single Prism file Collapsing all worksheets")

##another way is to have the sheets on the x Axis and color by treatment
p4<-ggplot(filter(ALL4,ELISA == "24(s)Hydroxycholesterol"  & 
                date == "20210113"), mapping = aes(x=sheetname,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color=treatment))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(ELISA~date )+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "24sHydroxycholesterol Data from a single Prism file Worksheets along the X axis colored by treatment")





###normalized data only by sheet grepl is powerful and find any cell containing 'normalized
p5<-ggplot(filter(ALL4,  grepl('normalized',sheetname) &
                date == "20210113" ), mapping = aes(x=treatment,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color=treatment))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(ELISA~sheetname, scales = "free")+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "24sHydroxycholesterol Normalized Data from a single Prism file colored by Treatment")

###normalized data on the same graph
p6<-ggplot(filter(ALL4,  grepl('normalized',sheetname) &
                date == "20210113" ), mapping = aes(x=treatment,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color=treatment))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(ELISA~date)+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "24sHydroxycholesterol Normalized Data from a single Prism file all worksheets Collapsed")


############################using more than one FILE the worksheets increase#######################
#multiplefiles across worksheets
p7<-ggplot(filter(ALL4,  grepl('normalized',sheetname) &
                  ELISA=="24(s)Hydroxycholesterol" ), mapping = aes(x=treatment,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color=treatment))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(~sheetname)+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  #scale_x_discrete(label = abbreviate)
  labs(title = "ALL 24sHydroxycholesterol Normalized Data Colored by Treatment faceted by sheetname")


#multiple files worksheets collapsed 24SHYDROXYCHOLESEROL
p8<-ggplot(filter(ALL4,  grepl('normalized',sheetname) &
                ELISA=="24(s)Hydroxycholesterol"), mapping = aes(x=treatment,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color=treatment))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(~(ELISA))+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "ALL 24sHydroxycholesterol Normalized Data Colored by Treatment Collapsed")



p8.1<-ggplot(filter(ALL4, !grepl("mg/kg",treatment) &
                     ELISA == "24(s)Hydroxycholesterol"&
                      grepl('normalized',sheetname) ), mapping = aes(x=treatment,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color=treatment))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(~sheetname, scales = "free")+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "ALL 24sHydroxycholesterol Normalized Data Colored by Treatment from media faceted by sheetname")


p8.2<-ggplot(filter(ALL4, grepl("mg/kg",treatment) &
                      ELISA == "24(s)Hydroxycholesterol"&
                      grepl('normalized',sheetname) ), mapping = aes(x=treatment,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color=treatment))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(~sheetname, scales = "free")+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "ALL 24sHydroxycholesterol Normalized Data Colored by Treatment dosed by bodyweight faceted by sheetname")









unique(ALL4$ELISA)
                                ##ALPHA-SYN###

#multiple files worksheets collapsed ALPHA-SYNUCLEIN
p9<-ggplot(filter(ALL4,  grepl('normalized',sheetname) &
                ELISA=="alphasynuclein" ), mapping = aes(x=treatment,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color=treatment))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(~ELISA)+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")+
  labs(title = "ALL Apha-Synuclein Normalized Data Colored by Treatment Collapsed")

p10<-ggplot(filter(ALL4,  grepl('normalized',sheetname) &
                ELISA=="alphasynuclein" ), mapping = aes(x=treatment,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color=sheetname))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(~ELISA)+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")+
  labs(title = "ALL Apha-Synuclein Normalized Data Colored by sheetname ")
  

p11<-ggplot(filter(ALL4,  grepl('normalized',sheetname) &
                ELISA=="alphasynuclein" ), mapping = aes(x=treatment,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color=sheetname))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(~interaction(ELISA,date))+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")+
  labs(title = "ALL Apha-Synuclein Normalized Data Colored by sheetname faceted by date")



p12<-ggplot(filter(ALL4,  grepl('normalized',sheetname) &
                     ELISA=="alphasynuclein" ), mapping = aes(x=treatment,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color= treatment))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  facet_wrap(~sheetname, scales = 'free')+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")+
  #scale_x_discrete(labels = abbreviate)+
  labs(title = "ALL Apha-Synuclein Normalized Data Colored by treatment faceted by sheetname")



p13<-ggplot(filter(ALL4,  grepl('normalized',sheetname) ), mapping = aes(x=treatment,y=as.numeric(value), fill = treatment ))+
  #geom_bar(position = 'dodge',stat = 'identity', color = "blue",fill = 'white', aes(x=treatment,y=average))+
  geom_jitter(stat = "identity", size = 3, alpha = .4, width = .2, height=.2, na.rm = FALSE,aes(color= ELISA))+
  #geom_errorbar(aes(ymin=average-SEM, ymax = average + SEM), width = .2)+
  #facet_wrap(~sheetname, scales = 'free')+
  # geom_text(aes(label = ELISA))
  #geom_text(aes(label=ifelse(value<20 & rep == "1",ELISA,'')),hjust=0,vjust=0)
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")+
  #scale_x_discrete(labels = abbreviate)+
  labs(title = "ALL  Normalized Data Colored by ELISA")













filter(ALL4,  grepl('10uM 807',treatment) &
          ELISA=="alphasynuclein" )


filter(ALL4,  treatment == "10uM CTT-807"&
         ELISA=="alphasynuclein" & value != "0" )

#selects only normalized data
test<-filter(ALL4,  grepl('normalized',sheetname))











#grouping by worksheet we can have compute analysis of variance for a small subset of our data this tells if there signigicance overall but not total
my_anova<- aov(average ~ treatment,data=test)
summary(my_anova)

#to find out which can use TUKey Honest Significant Differences to do pairwise comparisonsand obtain p values for each
TukeyHSD(my_anova) 


unique(ALL4$treatment)
unique(ALL4$sheetname) 

unique(ALL4$ELISA) 














###################################################### power point################################
library(officer) 
# Create a PowerPoint 
doc <- read_pptx()
layout_summary(doc)
doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p1 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p2 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p3 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p4 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p5 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p6 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p7 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p8 ,location = ph_location_fullsize() )


doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p9 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p10 ,location = ph_location_fullsize() )

doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
doc <- ph_with(x = doc, value = p11 ,location = ph_location_fullsize() )


#doc <- add_slide(doc, layout = "Two Content",master = "Office Theme")
#doc <- ph_with(x = doc, value = p9 ,location = ph_location_fullsize() )
print(doc, target = paste0( "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/Prism Gateway/","Prism_puller3",".pptx" )) 



write.csv(ALL4, file = paste0( "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/Prism Gateway/","Prism_puller3",".csv" ))  
