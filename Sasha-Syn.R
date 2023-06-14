library(plyr)
library(tidyr)


FILES<-list.files("C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/Sandbox/Sasha", full.names = TRUE)


##Identify the files we want
grep("Allnuclei.csv", FILES)

grep("CellBodyaSyn.csv", FILES)

grep("CellBodyaSynCount.csv", FILES)

grep("CellBodyCount.csv", FILES)

grep("CellSyn.csv", FILES)


grep("Cy5.csv", FILES)

grep("FITC.csv", FILES)

grep("fitcPuncta.csv", FILES)

grep("CellMasksResults.csv",FILES)

grep("CellSynCount.csv", FILES)
#########################################DAPI and MAP2#########################################
grep("Allnuclei.csv", FILES)
grep("Cy5.csv", FILES)

DAPI<-read.csv(file = FILES[grep("Allnuclei.csv", FILES)])
DAPI<-DAPI %>% 
      separate(., col = Slice, into = c("Row","Column",LETTERS[3:8]), remove = TRUE) %>% 
      unite(.,col = "Field", C:D) %>% 
      unite(.,col = "Channel",E:H)
colnames(DAPI)[4:12]<-paste(colnames(DAPI)[4:12],"DAPI",sep="-")
#only "Count..." is needed

 

MAP2<-read.csv(file = FILES[grep("Cy5.csv", FILES)])
MAP2<-MAP2[,-1] %>%
      separate(.,col = Label,into = c("Row","Column",LETTERS[3:10]),remove= TRUE) %>% 
      unite(.,col = "Field", C:D) %>% 
      unite(., col= "Channel", E:H) 
MAP2<-MAP2[,c(-5,-6)]
colnames(MAP2)[4:15]<-paste(colnames(MAP2)[4:15],"MAP2",sep="-")
#only "Area..." is needed



DAPI_MAP2<-merge(DAPI,MAP2, by = c("Row", "Column", "Field") )
#only "Count-DAPI" and "Area-MAP2" are needed CAREFUL OF WHICH!!!!
##########################################################################################










######################################SYNUCLEIN NUMBER AND INTENSITY######################
grep("fitcPuncta.csv", FILES)
grep("FITC.csv", FILES)
  
Syn_Puncta<-read.csv(file = FILES[grep("fitcPuncta.csv", FILES)])
Syn_Puncta<-Syn_Puncta %>% 
            separate(.,col = "Slice", into = c("Row","Column",LETTERS[3:9])) %>% 
            unite(.,col = "Field", C:D) %>% 
            unite(.,col = "Channel", c(E:G,I))
Syn_Puncta$H = NULL
colnames(Syn_Puncta)[4:12]<-paste(colnames(Syn_Puncta)[4:12],"Syn-Puncta",sep="-")
###only "Count..." is needed



Syn_Puncta_Int<-read.csv(file = FILES[grep("FITC.csv", FILES)])
Syn_Puncta_Int<-Syn_Puncta_Int[-1] %>% 
                separate(.,col = "Label", into = c("Row","Column", LETTERS[3:10])) %>% 
                unite(.,col = "Field", C:D) %>% 
                unite(.,col = "Channel", c(E:H))
Syn_Puncta_Int$I<-NULL
Syn_Puncta_Int$J<-NULL 
colnames(Syn_Puncta_Int)[4:15]<-paste(colnames(Syn_Puncta_Int)[4:15],"Syn-Puncta-Int",sep="-")
#only "RawIntDen..." is needed


Syn_No_Int<-merge(Syn_Puncta,Syn_Puncta_Int, by = c("Row", "Column", "Field") )
#only "Count" and "RawIntDen"" are needed CAREFUL OF WHICH!!!!
########################################################################################## 














######################################MAP2 SPECIFIC SYNUCLEIN AREA, NUMBER AND INTENSITY######################
grep("CellMasksResults.csv",FILES)#Cell body area
grep("CellSynCount.csv", FILES)
grep("CellSyn.csv", FILES)#intensity


MAP2_Syn_Area<-read.csv(file = FILES[grep("CellMasksResults.csv",FILES)])
MAP2_Syn_Area<-MAP2_Syn_Area[,-1] %>% 
                 separate(.,col = "Label", into = c("Row","Column",LETTERS[3:10])) %>% 
                 unite(.,col = "Field", C:D) %>% 
                 unite(.,col = "Channel", c(E:H))
                 MAP2_Syn_Area$I = NULL
                 MAP2_Syn_Area$J = NULL
                 colnames(MAP2_Syn_Area)[4:15]<-paste(colnames(MAP2_Syn_Area)[4:15],"MAP2-Syn-Area",sep="-")
###only "Area..." is needed


MAP2_Syn_Puncta<-read.csv(file = FILES[grep("CellSynCount.csv", FILES)])
MAP2_Syn_Puncta<-MAP2_Syn_Puncta%>% 
                 separate(.,col = "Slice", into = c("Row","Column", LETTERS[3:9])) %>% 
                 unite(.,col = "Field", C:D) %>% 
                 unite(.,col = "Channel", c(E:G,I))
                 MAP2_Syn_Puncta$H = NULL
                 colnames(MAP2_Syn_Puncta)[4:12]<-paste(colnames(MAP2_Syn_Puncta)[4:12],"MAP2-Syn-Puncta",sep="-")
#only "Count..." is needed



MAP2_Syn_Int<-read.csv(file = FILES[grep("CellSyn.csv", FILES)])
MAP2_Syn_Int<-MAP2_Syn_Int[-1]%>% 
              separate(.,col = "Label", into = c("Row","Column", LETTERS[3:10])) %>% 
              unite(.,col = "Field", C:D) %>% 
              unite(.,col = "Channel", c(E:H))
              MAP2_Syn_Int$I = NULL
              MAP2_Syn_Int$J = NULL
              colnames(MAP2_Syn_Int)[4:15]<-paste(colnames(MAP2_Syn_Int)[4:15],"MAP2-Syn-Int",sep="-")
#only "RawIntDen..." is needed. 


MAP2_Syn_area_No<-merge(MAP2_Syn_Area,MAP2_Syn_Puncta, by = c("Row", "Column", "Field") )
MAP2_Syn_Area_No_Int<-merge(MAP2_Syn_area_No,MAP2_Syn_Int, by = c("Row", "Column", "Field") )
###only "Area"... "Count"... and "RawIntden"... are needed....CAREFUL OF WHICH!!!!
#############################################################################################################








########################NEURONAL ...........SPECIFIC SYNUCLEIN  NUMBER AND INTENSITY
grep("CellBodyCount.csv", FILES)#Neuron area
grep("CellBodyaSynCount.csv", FILES)#Count
grep("CellBodyaSyn.csv", FILES)#Intensity


Neuronal<-read.csv(file = FILES[grep("CellBodyCount.csv", FILES)])
Neuronal<-Neuronal %>% 
          separate(.,col = "Slice", into = c("Row","Column", LETTERS[3:8])) %>% 
          unite(.,col = "Field", C:D) %>% 
          unite(.,col = "Channel", c(E:H))
          colnames(Neuronal)[4:12]<-paste(colnames(Neuronal)[4:12],"Neuronal",sep="-")
#only "Count..." and "Total.Area..." is needed. 



Neuronal_Syn_Puncta<-read.csv(file = FILES[grep("CellBodyaSynCount.csv", FILES)])
Neuronal_Syn_Puncta<-Neuronal_Syn_Puncta %>% 
                 separate(.,col = "Slice", into = c("Row","Column", LETTERS[3:9])) %>% 
                 unite(.,col = "Field", C:D) %>% 
                 unite(.,col = "Channel", c(E:G,I))
                 Neuronal_Syn_Puncta$H = NULL
                 colnames(Neuronal_Syn_Puncta)[4:12]<-paste(colnames(Neuronal_Syn_Puncta)[4:12],"Neuro_Syn_Puncta",sep="-")
#only "Count..."   is needed.
                 
Neuronal_Syn_Int<-read.csv(file = FILES[grep("CellBodyaSyn.csv", FILES)])
Neuronal_Syn_Int<-Neuronal_Syn_Int[,-1] %>% 
                  separate(.,col = "Label", into = c("Row","Column", LETTERS[3:10])) %>% 
                  unite(.,col = "Field", C:D) %>% 
                  unite(.,col = "Channel", c(E:H))
                  Neuronal_Syn_Int$I=NULL
                  Neuronal_Syn_Int$J=NULL
                  colnames(Neuronal_Syn_Int)[4:15]<-paste(colnames(Neuronal_Syn_Int)[4:15],"Neuro_Syn_Int",sep="-")
#only "RawIntDen..."   is needed.

                  
Neuro_Syn_Puncta<-merge(Neuronal,Neuronal_Syn_Puncta,by=c("Row", "Column", "Field"))
Neuro_Syn_No_Int<-merge(Neuro_Syn_Puncta,Neuronal_Syn_Int,by= c("Row", "Column", "Field"))
#####################################################################################################################







#MERGED FRAMES ARE....
SASHA<-list(DAPI_MAP2,
            Syn_No_Int,
            MAP2_Syn_Area_No_Int,
            Neuro_Syn_No_Int)


colnames(SASHA)



SASHA<-join_all(SASHA, by =  c("Row", "Column", "Field"), type = 'full')
colnames(SASHA) 

####CALCULATIONS TO CREATE NEW COLUMNS   
SASHA2<-mutate(SASHA, 
       SYN_PER_AREA_OF_MAP2= SASHA$`Total.Area-Syn-Puncta`/SASHA$`Area-MAP2` ,#1
       SYN_INT_PER_AREA_OF_MAP2 =SASHA$`Area-Syn-Puncta-Int`/SASHA$`Area-MAP2`,#2
       MAP2_PROXY = SASHA$`Area-MAP2`, #3,
       DAPI_PROXY = SASHA$`Count-DAPI`,#4,
       INTRA_SYN_COUNT_PUNCTA_PER_CELL_MASK_AREA = SASHA$`Count-MAP2-Syn-Puncta`/SASHA$`Area-MAP2-Syn-Area`,#5
       INT_SYN_PER_CELL_MASK_AREA = SASHA$`RawIntDen-MAP2-Syn-Int`/SASHA$`Area-MAP2-Syn-Area`,#6
       CELL_BODY_SYN_COUNT_PER_CELL_BODY_AREA= SASHA$`Count-Neuro_Syn_Puncta`/SASHA$`Total.Area-Neuronal`,#7
       CELL_BODY_SYN_AREA_PER_CELL_BODY_AREA= SASHA$`Area-Neuro_Syn_Int`/SASHA$`Total.Area-Neuronal`,#7b
       CELL_BODY_SYN_INTENSITY_PER_CELL_BODY_AREA= SASHA$`IntDen-Neuro_Syn_Int`/SASHA$`Total.Area-Neuronal`,#7c
       CELL_BODY_SYN_INTENSITY_PER_CELL_BODY_AREA2= SASHA$`IntDen-Neuro_Syn_Puncta`/SASHA$`Total.Area-Neuronal`,#7d
      # CELL_BODY_SYN_COUNT_PER_CELL_BODY_AREA5= SASHA$``/SASHA$`Total.Area-Neuronal`,#7e
       #CELL_BODY_SYN_COUNT_PER_CELL_BODY_AREA6= SASHA$``/SASHA$`Total.Area-Neuronal`,#7f
       CELL_BODY_SYN_Int_PER_CELL_BODY_AREA= SASHA$`RawIntDen-Neuro_Syn_Int`/SASHA$`Total.Area-Neuronal`,#8
       CELL_BODY_AREA_PROXY = SASHA$`Total.Area-Neuronal`)#9

library(dplyr)
#write out a summary
SASHA3<-SASHA2 %>% 
        group_by(.,Row,Column) %>% 
        dplyr::summarise(across(SYN_PER_AREA_OF_MAP2:CELL_BODY_AREA_PROXY, ~ mean(.x, na.rm = TRUE)))
write.csv(SASHA3,file = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/Sandbox/SASHA.csv")

##some how Sasha doesnt take the mean across fields but the sum....
#This could be why we're not getting the exact value but close


####I think this would summurize each of the columns by created above by grouping by FOV
SASHA_SUM_BY_FIELD<-SASHA2 %>% 
  group_by(.,Field) %>% 
  summarise_each(.,funs(sum), 
                 SYN_PER_AREA_OF_MAP2,#1
                 SYN_INT_PER_AREA_OF_MAP2,#2
                 MAP2_PROXY, #3,
                 DAPI_PROXY,#4,
                 INTRA_SYN_COUNT_PUNCTA_PER_CELL_MASK_AREA,#5
                 INT_SYN_PER_CELL_MASK_AREA,#6
                 CELL_BODY_SYN_COUNT_PER_CELL_BODY_AREA,#7
                 CELL_BODY_SYN_Int_PER_CELL_BODY_AREA,#8
                 CELL_BODY_AREA_PROXY  )#9







# This package allows us to work with pacakges from python
#install.packages("reticulate")
library(reticulate)
#use_condaenv(condaenv = "C:/Users/Jabbar/anaconda3/pkgs/python-3.9.13-h6244533_2/python.exe")
use_condaenv(condaenv = "C:/Users/Jabbar/anaconda3/python.exe")
seaborn<-import('seaborn')
matplotlib <- import('matplotlib.pyplot')

seaborn$pairplot(r_to_py(SASHA))
matplotlib$show()


