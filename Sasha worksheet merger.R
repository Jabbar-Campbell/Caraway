library(openxlsx)
library(magrittr)
library(dplyr)

####HERE I READ IN EACH SHEET AND REMAKE COLUMN NAMES
sheet1<-read.xlsx(xlsxFile = "C:/Users/Jabbar/Downloads/All Data_Test.xlsx", sheet = 1) %>% 
  dplyr::select(Label:Solidity) %>% 
  tidyr::separate(.,col= "Label", into= c("row","column" ,"field", "field_no"),remove=FALSE)
  colnames(sheet1)[6:16]<-paste0(colnames(sheet1)[6:16], "_sheet1")


sheet2<-read.xlsx(xlsxFile = "C:/Users/Jabbar/Downloads/All Data_Test.xlsx", sheet = 2) %>% 
  tidyr::separate(.,col= "Slice", into= c("row","column" ,"field", "field_no"),remove=FALSE)
  colnames(sheet2)[6:14]<-paste0(colnames(sheet2)[6:14], "_sheet2")


sheet3<-read.xlsx(xlsxFile = "C:/Users/Jabbar/Downloads/All Data_Test.xlsx", sheet = 3) %>% 
        tidyr::separate(.,col= "Slice", into= c("row","column" ,"field", "field_no"),remove=FALSE)
        colnames(sheet3)[6:13]<-paste0(colnames(sheet3)[6:13], "_sheet3")

sheet4<-read.xlsx(xlsxFile = "C:/Users/Jabbar/Downloads/All Data_Test.xlsx", sheet = 4) %>% 
         dplyr::select(Label:Solidity) %>% 
         tidyr::separate(.,col= "Label", into= c("row","column" ,"field", "field_no"),remove=FALSE)
         colnames(sheet4)[6:16]<-paste0(colnames(sheet4)[6:16], "_sheet4")



         
         
         
 #SHEETS ARE NOW JOINED TOGETHER        
         
sheet5<- dplyr::left_join(sheet3,sheet4,by = c("row","column","field_no"))


sheet6<- dplyr::left_join(sheet5,sheet2,by = c("row","column","field_no"))

sheet7<- dplyr::left_join(sheet6,sheet1,by = c("row","column","field_no"))




  #FINAL SHEET 7 IS MUTATED CREATING NEW CALCULATED COLUMNS 
sasha<-mutate(sheet7, count_ratio = Count_sheet3/Count_sheet2  ) %>% 
       mutate(., Count_over_area = Count_sheet3/ Area_sheet1 ) %>% 
       mutate(., Area_over_count = Area_sheet4/Count_sheet2) %>% 
       mutate(., RawIntDen_over_count= RawIntDen_sheet1/Count_sheet2) %>% 
       mutate(., RawIntDen_over_count= Area_sheet1 ) #%>% 
  #mutate(., avg_count_ratio =  mean(count_ratio))
 
##min max mean of newly created columns
sasha_sum<-summary(sasha[,51:54])

write.csv(sasha, file = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/temp/output.csv")
write.csv(sasha_sum, file = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/temp/output_sum.csv")
